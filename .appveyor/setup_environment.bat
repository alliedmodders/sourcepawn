@echo on
git submodule update --init
git clone https://github.com/alliedmodders/ambuild ambuild
cd ambuild
c:\python27\python.exe setup.py install
chdir /D "%APPVEYOR_BUILD_FOLDER%"
