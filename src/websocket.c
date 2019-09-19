#include "websocket.h"
#include "str.h"

#include <endian.h>
#include <unistd.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <openssl/sha.h>

static void websocket_client_callback(struct net_client_context *ctx);

int websocket_init(struct websocket_context *ctx, char *node, char *service)
{
	int err;

	ctx->net.client_callback = websocket_client_callback;

	err = net_init(&ctx->net, node, service);
	if (err) {
		return -1;
	}

	return 0;
}

#define READ_BUFFER_SIZE 4096

struct read_buffer {
	int fd;

	uint8_t *cursor;

	// The beginning of the memory currently used by the client. Do
	// not discard.
	uint8_t *hot_begin;
	uint8_t *limit;
	bool eof;

	uint8_t buffer[READ_BUFFER_SIZE];
};

void read_buffer_init(struct read_buffer *buf, int fd)
{
	memset(buf, 0, sizeof(struct read_buffer));

	buf->fd        = fd;
	buf->limit     = buf->buffer + READ_BUFFER_SIZE;
	buf->hot_begin = buf->limit;
	buf->cursor    = buf->limit;
	buf->eof       = false;
}

bool read_buffer_fill(struct read_buffer *buf, size_t need)
{
	if (buf->eof) {
		return false;
	}

	size_t free = buf->hot_begin - buf->buffer;

	if (free < need) {
		fprintf(stderr, "Not enough space in read buffer.\n");
		return false;
	}
	memmove(buf->buffer, buf->hot_begin, buf->limit - buf->hot_begin);

	buf->limit     -= free;
	buf->cursor    -= free;
	buf->hot_begin -= free;

	size_t bytes_read = 0;

	while (bytes_read < need) {
		ssize_t err;
		err = read(buf->fd, buf->limit + bytes_read, free - bytes_read);

		if (err < 0) {
			perror("read");
			return false;
		} else if (err == 0) {
			buf->eof = true;
			return bytes_read >= need;
		} else {
			buf->limit += err;
			bytes_read += err;
		}
	}

	return true;
}

static void *websocket_read_bytes(struct read_buffer *buf, size_t length)
{
	buf->hot_begin = buf->cursor;

	if (buf->limit - buf->hot_begin < length) {
		if (!read_buffer_fill(buf, length)) {
			return NULL;
		}
	}

	buf->cursor += length;
	return buf->hot_begin;
}

static ssize_t websocket_read_bytes_into(struct read_buffer *buf, uint8_t *out, size_t length)
{
	size_t bytes_read = 0;

	while (bytes_read < length) {
		size_t bytes_to_read = buf->limit - buf->cursor;

		memcpy(&out[bytes_read], buf->cursor, bytes_to_read);

		buf->hot_begin = buf->cursor;

		if (!read_buffer_fill(buf, 1)) {
			return -1;
		}

		buf->cursor += bytes_to_read;
		bytes_read += bytes_to_read;
	}

	return bytes_read;
}

static bool websocket_http_read_line(struct read_buffer *buf, struct string *out)
{
	buf->hot_begin = buf->cursor;

	// States:
	// 0: Idle
	//     '\r' -> state 1
	//     else -> state 0
	// 1: Found \r
	//     '\n' -> state 2
	//     else -> state 0
	// 2: Found \n
	//     accept
	int state = 0;

	if (buf->cursor >= buf->limit) {
		if (!read_buffer_fill(buf, 1)) {
			return false;
		}
	}

	while (state != 2) {
		switch (state) {
		case 0:
			if (*buf->cursor == '\r') {
				state = 1;
			}
			break;

		case 1:
			if (*buf->cursor == '\n') {
				state = 2;
			}
			break;

		default:
			state = 0;
			break;
		}

		buf->cursor += 1;

		if (buf->cursor >= buf->limit && state != 2) {
			if (!read_buffer_fill(buf, 1)) {
				return false;
			}
		}
	}

	// 2 is to compansate for newline
	out->text = (char *)buf->hot_begin;
	out->length = buf->cursor - buf->hot_begin - 2;

	/* printf("read line '%.*s' (%zu)\n", LIT(*out), out->length); */

	return true;
}

static void string_trim(char needle, struct string in, struct string *out)
{
	struct string result = {0};

	result = in;

	while (*result.text == needle && result.length > 0) {
		result.text   += 1;
		result.length -= 1;
	}

	*out = result;
}

static void string_split_by(char needle, struct string in,
							struct string *out_head,
							struct string *out_tail)
{
	struct string head = {0};
	struct string tail = {0};

	head.text = in.text;
	head.length = 0;

	tail.text = in.text;
	tail.length = in.length;

	// Trim empty strings from the beginning
	while (*head.text == needle && tail.length > 0) {
		head.text   += 1;
		tail.length -= 1;
	}

	while (*(head.text + head.length) != needle && tail.length > 0) {
		head.length += 1;
		tail.length -= 1;
	}

	// The 1 is to compensate for the needle.
	tail.text = head.text + head.length + 1;
	tail.length -= 1;

	*out_head = head;
	*out_tail = tail;
}

static bool websocket_http_read_request(struct read_buffer *buf,
										struct string *out_method,
										struct string *out_resource,
										struct string *out_http_version)
{
	struct string line = {0};

	if (!websocket_http_read_line(buf, &line)) {
		return false;
	}

	string_split_by(' ', line, out_method,       &line);
	string_split_by(' ', line, out_resource,     &line);
	string_split_by(' ', line, out_http_version, &line);

	return true;
}

static bool websocket_http_read_header(struct read_buffer *buf,
									   struct string *out_header,
									   struct string *out_value)
{
	struct string line = {0};

	if (!websocket_http_read_line(buf, &line)) {
		return false;
	}

	if (line.length == 0) {
		return false;
	}

	string_split_by(':', line, out_header, &line);
	string_trim(' ', line, out_value);

	return true;
}

static void websocket_send_string(int fd, struct string str)
{
	size_t bytes_written = 0;

	while (bytes_written < str.length) {
		ssize_t err;
		err = write(fd, str.text + bytes_written, str.length - bytes_written);
		if (err <= 0) {
			perror("write");
			return;
		}

		bytes_written += err;
	}
}

static void websocket_handshake_fail(int fd)
{
	static const struct string str = STR("HTTP/1.1 400 Bad Request\r\n\r\n");

	websocket_send_string(fd, str);
}

#define BASE64_LENGTH(x) ((((x) / 3) + 1) * 4)

static void websocket_base64_encode(struct string in, struct string *out)
{
	static const char *alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

	assert(out->text && out->length == BASE64_LENGTH(in.length));

	unsigned char *it     = (unsigned char *)in.text;
	unsigned char *it_end = (unsigned char *)in.text + in.length;
	unsigned char *it_out = (unsigned char *)out->text;
	unsigned char *it_out_end = (unsigned char *)out->text + out->length;

#define PUSH_VALUE(x) assert(it_out < it_out_end); *(it_out++) = (x);

	int result = 0;

	while (it < it_end) {
		result = (*it & 0xfc) >> 2;
		PUSH_VALUE(alphabet[result]);
		result = (*it & 0x3) << 4;

		it += 1;
		if (it >= it_end) {
			PUSH_VALUE(alphabet[result]);
			PUSH_VALUE('=');
			PUSH_VALUE('=');
			break;
		}

		result |= (*it & 0xf0) >> 4;
		PUSH_VALUE(alphabet[result]);
		/* *(it_out++) = alphabet[result]; */
		result = (*it & 0xf) << 2;

		it += 1;
		if (it >= it_end) {
			PUSH_VALUE(alphabet[result]);
			PUSH_VALUE('=');
			break;
		}

		result |= (*it & 0xc0) >> 6;
		PUSH_VALUE(alphabet[result]);

		result = (*it & 0x3f);
		PUSH_VALUE(alphabet[result]);

		it += 1;
	}

#undef PUSH_VALUE
}

static void websocket_handshake_accept_key(struct string key, struct string *out)
{
	static const struct string magic = STR("258EAFA5-E914-47DA-95CA-C5AB0DC85B11");

	assert(out->text != NULL && out->length == BASE64_LENGTH(SHA_DIGEST_LENGTH));

	SHA_CTX ctx;
	SHA1_Init(&ctx);

	SHA1_Update(&ctx, key.text,   key.length);
	SHA1_Update(&ctx, magic.text, magic.length);

	unsigned char buffer[SHA_DIGEST_LENGTH];

	SHA1_Final(buffer, &ctx);

	struct string hash = {0};
	hash.text = (char*)buffer;
	hash.length = sizeof(buffer);

	websocket_base64_encode(hash, out);
}

static bool websocket_handshake(struct read_buffer *buf)
{
	struct string http_method = {0};
	struct string http_resource = {0};
	struct string http_version = {0};

	struct string accept_key = {0};
	char accept_key_buffer[BASE64_LENGTH(SHA_DIGEST_LENGTH)] = {0};
	accept_key.text = accept_key_buffer;
	accept_key.length = sizeof(accept_key_buffer);

	if (!websocket_http_read_request(buf, &http_method, &http_resource, &http_version)) {
		return false;
	}

	if (!string_equal(http_method, STR("GET"))) {
		websocket_handshake_fail(buf->fd);
		return false;
	}

	if (!string_equal(http_version, STR("HTTP/1.1"))) {
		websocket_handshake_fail(buf->fd);
		return false;
	}

	bool fail = false;

	bool connection_correct = false;
	bool upgrade_correct = false;
	bool websocket_version_correct = false;
	bool websocket_key_correct = false;

	struct string http_header;
	struct string http_header_value;
	while (websocket_http_read_header(buf, &http_header, &http_header_value)) {
		if (string_equal(http_header, STR("Connection"))) {
			// @TODO: Check Connection contains Upgrade
			if (string_equal(http_header_value, STR("Upgrade"))) {
				connection_correct = true;
			} else if (string_equal(http_header_value, STR("keep-alive, Upgrade"))) {
				connection_correct = true;
			} else {
				printf("Expected Connection header to be 'Upgrade', got '%.*s'.\n",
					   LIT(http_header_value));
				fail = true;
			}

		} else if (string_equal(http_header, STR("Upgrade"))) {
			if (string_equal(http_header_value, STR("websocket"))) {
				upgrade_correct = true;
			} else {
				printf("Expected Upgrade header to be 'websocket', got '%.*s'.\n",
					   LIT(http_header_value));
				fail = true;
			}

		} else if (string_equal(http_header, STR("Sec-WebSocket-Key"))) {
			websocket_handshake_accept_key(http_header_value, &accept_key);
			websocket_key_correct = true;

		} else if (string_equal(http_header, STR("Sec-WebSocket-Version"))) {
			if (string_equal(http_header_value, STR("13"))) {
				websocket_version_correct = true;
			} else {
				printf("Expected Sec-WebSocket-Version header to be '13', got '%.*s'.\n",
					   LIT(http_header_value));
				fail = true;
			}
		}

		/* printf("header '%.*s' : '%.*s'\n", LIT(http_header), LIT(http_header_value)); */
	}

	if (!connection_correct) {
		printf("Missing Connection header.\n");
		fail = true;
	}

	if (!upgrade_correct) {
		printf("Missing Upgrade header.\n");
		fail = true;
	}

	if (!websocket_version_correct) {
		printf("Missing Sec-WebSocket-Version header.\n");
		fail = true;
	}

	if (fail) {
		websocket_handshake_fail(buf->fd);
		return false;
	}

	static const struct string response_pre = STR("HTTP/1.1 101 Switching Protocols\r\n"
												  "Upgrade: websocket\r\n"
												  "Connection: Upgrade\r\n"
												  "Sec-WebSocket-Accept: ");
	static const struct string response_post = STR("\r\n\r\n");

	websocket_send_string(buf->fd, response_pre);
	websocket_send_string(buf->fd, accept_key);
	websocket_send_string(buf->fd, response_post);

	return true;
}

enum websocket_frame_op {
	FRAME_OP_CONTINUATION = 0x0,
	FRAME_OP_TEXT         = 0x1,
	FRAME_OP_BINARY       = 0x2,
	FRAME_OP_CLOSE        = 0x8,
	FRAME_OP_PING         = 0x9,
	FRAME_OP_PONG         = 0xa,
};

struct websocket_frame {
	bool fin;
	uint8_t op;
	bool mask;
	uint64_t len;
	uint8_t *data;
	size_t data_capacity;
};

static int websocket_read_frame(struct net_client_context *ctx, struct read_buffer *buf,
								struct websocket_frame *out)
{
	uint8_t *header = websocket_read_bytes(buf, 2);

	if (!header) {
		return -1;
	}

	bool fin = !!(header[0] & 0x80);
	uint8_t op = header[0] & 0xf;
	bool mask = !!(header[1] & 0x80);
	uint64_t len = header[1] & 0x7f;

	if (!mask) {
		printf("Websocket frames the server receive must be masked.\n");
		return -1;
	}

	if (len == 126) {
		header = websocket_read_bytes(buf, 2);
		len = be16toh(*(uint16_t *)header);
	} else if (len == 127) {
		header = websocket_read_bytes(buf, 8);
		len = be64toh(*(uint64_t *)header);
	}

	header = websocket_read_bytes(buf, 4);

	if (!header) {
		printf("Failed to read masking key.\n");
		return -1;
	}

	uint8_t masking_key[4];
	for (int i = 0; i < 4; i++) {
		masking_key[i] = header[i];
	}

	uint8_t *buffer = out->data;
	if (len > 0) {
		if (len > out->data_capacity) {
			buffer = realloc(buffer, len);

			if (!buffer) {
				printf("Could not allocate memory for web socket frame.\n");
				return -1;
			}

			out->data = buffer;
			out->data_capacity = len;
		}

		ssize_t err;
		err = websocket_read_bytes_into(buf, buffer, len);

		if (err < 0) {
			printf("Failed to read frame.\n");
			return -1;
		}

		for (size_t i = 0; i < len; i++) {
			buffer[i] = buffer[i] ^ masking_key[i % 4];
		}
	}

	out->fin = fin;
	out->op = op;
	out->mask = mask;
	out->len = len;
	out->data = buffer;

	return 0;
}

static void websocket_client_callback(struct net_client_context *ctx)
{
	struct read_buffer buffer;

	read_buffer_init(&buffer, ctx->socket_fd);

	if (!websocket_handshake(&buffer)) {
		return;
	}

	struct websocket_frame frame = {0};

	while (!ctx->should_quit) {
		int err;
		err = websocket_read_frame(ctx, &buffer, &frame);
		if (err < 0) {
			// Disconnect on error.
			ctx->should_quit = true;
			break;
		}

		switch (frame.op) {
		case FRAME_OP_CONTINUATION:
			printf("TODO: Implement websocket continuation.\n");
			break;

		case FRAME_OP_TEXT:
			printf("%.*s\n", (int)frame.len, (char*)frame.data);
			break;

		case FRAME_OP_BINARY:
			printf("Binary\n");
			break;

		case FRAME_OP_CLOSE:
			ctx->should_quit = true;
			break;
		}
	}

	free(frame.data);
}
