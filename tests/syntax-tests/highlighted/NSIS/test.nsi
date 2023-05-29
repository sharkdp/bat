[38;2;73;72;62m/*[0m
[38;2;73;72;62m * Multi-line[0m
[38;2;73;72;62m * Comment[0m
[38;2;73;72;62m */[0m

[38;2;73;72;62m# Single-line comment[0m
[38;2;73;72;62m; Another-single line comment[0m

[38;2;73;72;62m; Includes[0m
[38;2;73;72;62m!include "LogicLib.nsh"[0m

[38;2;73;72;62m; Defines[0m
[38;2;73;72;62m!define ARCHITECTURE "x64"[0m

[38;2;73;72;62m; Compile time command[0m
[38;2;73;72;62m!echo "Building ${ARCHITECTURE} script"[0m

[38;2;73;72;62m; Macro definition[0m
[38;2;73;72;62m!macro SayHello name[0m
[38;2;73;72;62m    !ifdef name[0m
[38;2;73;72;62m        !echo "Hello, ${name}"[0m
[38;2;73;72;62m    !else[0m
[38;2;73;72;62m        !echo "Hello, world"[0m
[38;2;73;72;62m    !endif[0m
[38;2;73;72;62m!macroend[0m

[38;2;73;72;62m; Macro usage[0m
[38;2;73;72;62m!insertmacro SayHello "John Doe"[0m

[38;2;73;72;62m; Settings[0m
[38;2;73;72;62mName "installer_name"[0m
[38;2;73;72;62mOutFile "installer_name.exe"[0m
[38;2;73;72;62mRequestExecutionLevel user[0m
[38;2;73;72;62mCRCCheck on[0m
[38;2;73;72;62mUnicode true[0m

[38;2;73;72;62m!ifdef ${ARCHITECTURE}[0m
[38;2;73;72;62m  InstallDir "$PROGRAMFILES64\installer_name"[0m
[38;2;73;72;62m!else[0m
[38;2;73;72;62m  InstallDir "$PROGRAMFILES\installer_name"[0m
[38;2;73;72;62m!endif[0m

[38;2;73;72;62m; Pages[0m
[38;2;73;72;62mPage components[0m
[38;2;73;72;62mPage instfiles[0m

[38;2;73;72;62m; Functions[0m
[38;2;73;72;62mFunction PrintTestStrings[0m
[38;2;73;72;62m  DetailPrint "The install button reads $(^InstallBtn)"[0m
[38;2;73;72;62m  DetailPrint 'Here comes a$\n$\rline-break!'[0m
[38;2;73;72;62m  DetailPrint `Escape the dollar-sign: $$`[0m
[38;2;73;72;62mFunctionEnd[0m

[38;2;73;72;62m; Sections[0m
[38;2;73;72;62mSection "section_name" section_index[0m
[38;2;73;72;62m    Call PrintTestStrings    [0m

[38;2;73;72;62m    ; NSIS plugin call[0m
[38;2;73;72;62m    nsExec::ExecToLog "calc.exe"[0m
[38;2;73;72;62mSectionEnd[0m

