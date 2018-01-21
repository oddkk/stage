all: build

build:
	@bash ./build.sh

clean:
	@rm src/*.y.c src/*.y.re2c ./stage

indent:
	@indent src/*.c src/*.h

.PHONY: build indent
