"%VS140COMNTOOLS%\..\..\VC\vcvarsall.bat" x86
mkdir opt32 && cd opt32
C:\python27\python.exe ..\configure.py --enable-optimize --amtl=..\..\amtl
C:\python27\Scripts\ambuild
testing\test-all.x86.bat

cd ..

"%VS140COMNTOOLS%\..\..\VC\vcvarsall.bat" amd64
mkdir opt64 && cd opt64
C:\python27\python.exe ..\configure.py --enable-optimize --amtl=..\..\amtl --target-arch=x64
C:\python27\Scripts\ambuild
testing\test-all.x64.bat
