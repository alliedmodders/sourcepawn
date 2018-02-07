#!/bin/bash
[[ $- == *e* ]] && state=-e || state=+e
set -e

if [ "$AM_CC" == "emcc" ]; then
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
