all: build

build:
	@bash ./build.sh

indent:
	@indent src/*.c src/*.h

.PHONY: build indent
