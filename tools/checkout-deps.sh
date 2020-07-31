#!/usr/bin/env bash
# This should be run inside a folder that contains sourcemod, otherwise, it will checkout things into "sm-dependencies".

trap "exit" INT

if [ ! -d "sourcepawn" ]; then
  echo "Could not find a SourceMod repository; make sure you aren't running this script inside it."
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

python_cmd=`command -v python`
if [ -z "$python_cmd" ]; then
  python_cmd=`command -v python3`

  if [ -z "$python_cmd" ]; then
    echo "No suitable installation of Python detected"
    exit 1
  fi
fi

`$python_cmd -c "import ambuild2"` 2>&1 1>/dev/null
if [ $? -eq 1 ]; then
  echo "AMBuild is required to build SourcePawn"

  `$python_cmd -m pip --version` 2>&1 1>/dev/null
  if [ $? -eq 1 ]; then
    echo "The detected Python installation does not have PIP"
    echo "Installing the latest version of PIP available (VIA \"get-pip.py\")"

    get_pip="./get-pip.py"
    get_pip_url="https://bootstrap.pypa.io/get-pip.py"

    if [ `command -v wget` ]; then
      wget $get_pip_url -O $get_pip
    elif [ `command -v curl` ]; then
      curl -o $get_pip $get_pip_url
    else
      echo "Failed to locate wget or curl. Install one of these programs to download 'get-pip.py'."
      exit 1
    fi

    $python_cmd $get_pip
    if [ $? -eq 1 ]; then
      echo "Installation of PIP has failed"
      exit 1
    fi
  fi

  repo="https://github.com/alliedmodders/ambuild"
  origin=
  branch=master
  name=ambuild
  checkout

  if [ $iswin -eq 1 ] || [ $ismac -eq 1 ]; then
    $python_cmd -m pip install ./ambuild
  else
    echo "Installing AMBuild at the user level. Location can be: ~/.local/bin"
    $python_cmd -m pip install --user ./ambuild
  fi
fi
