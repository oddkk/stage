# Stage

A functional programming language.

## Building

The compiler has a few dependencies:

- libffi
- bison
- re2c
- libpq (This dependency can be omitted by not compiling or using the sql
  module. To disable this module, comment out the relevant line from
  `Makefile`.)

After these dependencies have been installed the application can be compiled on
Linux using `make` or `./bash.sh`.

## Running

Any `.stg` file or module can be executed by running `./stage <filename>`. For
example, `./stage examples/hello_world.stg` will run the hello world example.
