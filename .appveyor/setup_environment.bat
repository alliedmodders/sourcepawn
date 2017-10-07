git clone https://github.com/alliedmodders/ambuild ambuild
cd ambuild
c:\python27\python.exe setup.py install
chdir /D c:\projects\
git clone https://github.com/alliedmodders/amtl amtl
chdir /D "%APPVEYOR_BUILD_FOLDER%"
