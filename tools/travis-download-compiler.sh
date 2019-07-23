#!/bin/bash
[[ $- == *e* ]] && state=-e || state=+e
set -e

if [ "$AM_CC" == "emcc" ]; then
  git clone --depth 1 https://github.com/emscripten-core/emsdk.git $HOME/emsdk

  $HOME/emsdk/emsdk install latest
  $HOME/emsdk/emsdk activate latest

  source $HOME/emsdk/emsdk_env.sh

  # The SDK no longer sets this envvar, but ambuild expects it.
  export EMSCRIPTEN=$HOME/emsdk/fastcomp/emscripten

  for compiler in $EMSCRIPTEN/{emcc,em++}; do
    touch -d "2017-01-01 00:00:00 +0800" $compiler
  done
fi

set "$state"
