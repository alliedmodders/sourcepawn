@echo on
call "C:\Program Files (x86)\Microsoft Visual Studio 15.0\VC\vcvarsall.bat" x86
mkdir opt32 && cd opt32
C:\python38\python.exe ..\configure.py --enable-optimize
C:\python38\scripts\ambuild
cd ..
C:\python38\python.exe tests\runtests.py opt32


call "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.cmd" /x64
call "C:\Program Files (x86)\Microsoft Visual Studio 15.0\VC\vcvarsall.bat" x86_amd64
mkdir opt64 && cd opt64
C:\python38\python.exe ..\configure.py --enable-optimize
C:\python38\scripts\ambuild
cd ..
C:\python38\python.exe tests\runtests.py opt64
