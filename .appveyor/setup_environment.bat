@echo on
git submodule update --init
git clone https://github.com/alliedmodders/ambuild ambuild
cd ambuild
c:\python38\scripts\pip.exe install .
chdir /D "%APPVEYOR_BUILD_FOLDER%"
