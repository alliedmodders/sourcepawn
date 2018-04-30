"%VS140COMNTOOLS%\..\..\VC\vcvarsall.bat" x86
mkdir opt32 && cd opt32
C:\python27\python.exe ..\configure.py --enable-optimize --amtl=..\..\amtl
C:\python27\Scripts\ambuild
cd ..
C:\python27\python.exe tests\runtests.py opt32


"%VS140COMNTOOLS%\..\..\VC\vcvarsall.bat" amd64
mkdir opt64 && cd opt64
C:\python27\python.exe ..\configure.py --enable-optimize --amtl=..\..\amtl --target-arch=x64
C:\python27\Scripts\ambuild
cd ..
C:\python27\python.exe tests\runtests.py opt64
