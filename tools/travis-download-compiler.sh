#!/bin/bash
[[ $- == *e* ]] && state=-e || state=+e
set -e

if [ "$AM_CC" == "emcc" ]; then
  git clone --depth 1 --branch $EMSCRIPTEN_SDK_BRANCH $EMSCRIPTEN_SDK_URI $HOME/emscripten-sdk
  $HOME/emscripten-sdk/emsdk activate --build=Release sdk-$EMSCRIPTEN_SDK_BRANCH-64bit

  source $HOME/emscripten-sdk/emsdk_env.sh

  for compiler in $EMSCRIPTEN/{emcc,em++}; do
    touch -d "2017-01-01 00:00:00 +0800" $compiler
  done
fi

set "$state"
