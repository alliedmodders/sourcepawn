#!/usr/bin/env bash
# This should be run inside a folder that contains sourcepawn.

trap "exit" INT

ismac=0
iswin=0

archive_ext=tar.gz
decomp="tar zxf"

if [ `uname` = "Darwin" ]; then
  ismac=1
elif [ `uname` != "Linux" ] && [ -n "${COMSPEC:+1}" ]; then
  iswin=1
  archive_ext=zip
  decomp=unzip
fi

if [ ! -d "sourcepawn" ]; then
  echo "Could not find a SourcePawn repository; make sure you aren't running this script inside it."
  exit 1
fi

checkout ()
{
  if [ ! -d "$name" ]; then
    git clone $repo -b $branch $name
    if [ -n "$origin" ]; then
      cd $name
      git remote rm origin
      git remote add origin $origin
      cd ..
    fi
  else
    cd $name
    git checkout $branch
    git pull origin $branch
    cd ..
  fi
}

`python -c "import ambuild2"`
if [ $? -eq 1 ]; then
  repo="https://github.com/alliedmodders/ambuild"
  origin=
  branch=master
  name=ambuild
  checkout

  cd ambuild
  if [ $iswin -eq 1 ]; then
    python setup.py install
  else
    python setup.py build
    echo "About to install AMBuild - press Ctrl+C to abort, otherwise enter your password for sudo."
    sudo python setup.py install
  fi
fi

repo="https://github.com/alliedmodders/amtl"
origin=
branch=master
name=amtl
checkout
