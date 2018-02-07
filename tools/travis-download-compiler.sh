#!/bin/bash
[[ $- == *e* ]] && state=-e || state=+e
set -e

if [ "$AM_CC" != "emcc" ]; then
  wget -nc $LLVM_ARCHIVE_URI -O $HOME/clang+llvm.tar.xz
  mkdir -p $HOME/clang+llvm
  tar -xf $HOME/clang+llvm.tar.xz -C $HOME/clang+llvm --strip-components 1

  export PATH=$HOME/clang+llvm/bin:$PATH
else
  wget -nc $EMSCRIPTEN_SDK_URI -O $HOME/emsdk-portable.tar.gz
  tar -xf $HOME/emsdk-portable.tar.gz -C $HOME
  pushd $HOME/emsdk-portable
  ./emsdk update
  ./emsdk install latest
  ./emsdk activate latest
  popd

  source $HOME/emsdk-portable/emsdk_env.sh
fi

set "$state"
