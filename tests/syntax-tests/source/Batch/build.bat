@echo off


:: Change to your LLVM installation
set "LLVMPath=C:\Program Files\LLVM"
:: Change to your Visual Studio 2017 installation
set "VSPath=C:\Program Files (x86)\Microsoft Visual Studio\2017\Community"
set "VSVersion=14.10.25017"
:: Change to your Windows Kit version & installation
set "WinSDKVersion=10.0.15063.0"
set "WinSDKPath=C:\Program Files (x86)\Windows Kits\10"
:: Change this to your resulting exe
set "OUTPUT=test.exe"


:: Setup
set "VSBasePath=%VSPath%\VC\Tools\MSVC\%VSVersion%"
set "PATH=%PATH%;%LLVMPath%\bin;%VSBasePath%\bin\HostX64\x64"

:: Compiler Flags
set CFLAGS= ^
 -std=c++14 -Wall -Wextra

set CPPFLAGS= ^
  -I "%VSBasePath%\include" ^
  -I "%WinSDKPath%\Include\%WinSDKVersion%\shared" ^
  -I "%WinSDKPath%\Include\%WinSDKVersion%\ucrt" ^
  -I "%WinSDKPath%\Include\%WinSDKVersion%\um"


:: Linker Libs
set LDFLAGS= ^
 -machine:x64 ^
 -nodefaultlib ^
 -subsystem:console

set LDLIBS= ^
 -libpath:"%VSBasePath%\lib\x64" ^
 -libpath:"%WinSDKPath%\Lib\%WinSDKVersion%\ucrt\x64" ^
 -libpath:"%WinSDKPath%\Lib\%WinSDKVersion%\um\x64" ^
 libucrt.lib libvcruntime.lib libcmt.lib libcpmt.lib ^
 legacy_stdio_definitions.lib oldnames.lib ^
 legacy_stdio_wide_specifiers.lib ^
 kernel32.lib User32.lib


:: Compiling
@echo on
@for %%f in (*.cc) do (
    clang++.exe "%%~f" -o "%%~nf.o" -c %CFLAGS%
)

:: Linking
@set "LINK_FILES="
@for %%f in (*.o) do (
    @set "LINK_FILES=%LINK_FILES% %%~f"
)

lld-link.exe %LINK_FILES% -out:"%OUTPUT%" %LDFLAGS% %LDLIBS%
