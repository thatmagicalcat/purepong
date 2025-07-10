alias b := build
alias r := run

lib_path := "./libs"
include_path := "./include"
out_dir := "./build"

build:
    ghc -odir {{out_dir}} -hidir {{out_dir}} src/*.hs -I{{include_path}} -L{{lib_path}} -lraylib -o build/pong

run: build
    LD_LIBRARY_PATH={{lib_path}} {{out_dir}}/pong

clean:
    rm {{out_dir}}/*
