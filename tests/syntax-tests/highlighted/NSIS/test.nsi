[38;2;248;248;242m/*[0m
[38;2;248;248;242m * Multi-line[0m
[38;2;248;248;242m * Comment[0m
[38;2;248;248;242m */[0m

[38;2;248;248;242m# Single-line comment[0m
[38;2;248;248;242m; Another-single line comment[0m

[38;2;248;248;242m; Includes[0m
[38;2;248;248;242m!include "LogicLib.nsh"[0m

[38;2;248;248;242m; Defines[0m
[38;2;248;248;242m!define ARCHITECTURE "x64"[0m

[38;2;248;248;242m; Compile time command[0m
[38;2;248;248;242m!echo "Building ${ARCHITECTURE} script"[0m

[38;2;248;248;242m; Macro definition[0m
[38;2;248;248;242m!macro SayHello name[0m
[38;2;248;248;242m    !ifdef name[0m
[38;2;248;248;242m        !echo "Hello, ${name}"[0m
[38;2;248;248;242m    !else[0m
[38;2;248;248;242m        !echo "Hello, world"[0m
[38;2;248;248;242m    !endif[0m
[38;2;248;248;242m!macroend[0m

[38;2;248;248;242m; Macro usage[0m
[38;2;248;248;242m!insertmacro SayHello "John Doe"[0m

[38;2;248;248;242m; Settings[0m
[38;2;248;248;242mName "installer_name"[0m
[38;2;248;248;242mOutFile "installer_name.exe"[0m
[38;2;248;248;242mRequestExecutionLevel user[0m
[38;2;248;248;242mCRCCheck on[0m
[38;2;248;248;242mUnicode true[0m

[38;2;248;248;242m!ifdef ${ARCHITECTURE}[0m
[38;2;248;248;242m  InstallDir "$PROGRAMFILES64\installer_name"[0m
[38;2;248;248;242m!else[0m
[38;2;248;248;242m  InstallDir "$PROGRAMFILES\installer_name"[0m
[38;2;248;248;242m!endif[0m

[38;2;248;248;242m; Pages[0m
[38;2;248;248;242mPage components[0m
[38;2;248;248;242mPage instfiles[0m

[38;2;248;248;242m; Functions[0m
[38;2;248;248;242mFunction PrintTestStrings[0m
[38;2;248;248;242m  DetailPrint "The install button reads $(^InstallBtn)"[0m
[38;2;248;248;242m  DetailPrint 'Here comes a$\n$\rline-break!'[0m
[38;2;248;248;242m  DetailPrint `Escape the dollar-sign: $$`[0m
[38;2;248;248;242mFunctionEnd[0m

[38;2;248;248;242m; Sections[0m
[38;2;248;248;242mSection "section_name" section_index[0m
[38;2;248;248;242m    Call PrintTestStrings    [0m

[38;2;248;248;242m    ; NSIS plugin call[0m
[38;2;248;248;242m    nsExec::ExecToLog "calc.exe"[0m
[38;2;248;248;242mSectionEnd[0m

