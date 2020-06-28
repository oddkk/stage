all: build

build:
	@bash ./build.sh
	@cd ./modules/channel/ && bash ./build.sh
	@cd ./modules/message/ && bash ./build.sh
	@cd ./modules/stream/ && bash ./build.sh
	@cd ./modules/sql/ && bash ./build.sh

clean:
	@rm -f src/*.y.c src/*.y.re2c src/*.y.output ./stage

indent:
	@indent src/*.c src/*.h
test:
	@perl ./test/run_tests.pl

.PHONY: build indent test
