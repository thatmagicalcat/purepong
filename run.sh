#!/bin/sh

raylib="./raylib-5.5_linux_amd64/"

ghc Main.hs -I"$raylib/include" -L"$raylib/lib" -lraylib && LD_LIBRARY_PATH="$raylib/lib" ./Main
