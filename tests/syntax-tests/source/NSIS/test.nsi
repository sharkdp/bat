/*
 * Multi-line
 * Comment
 */

# Single-line comment
; Another-single line comment

; Includes
!include "LogicLib.nsh"

; Defines
!define ARCHITECTURE "x64"

; Compile time command
!echo "Building ${ARCHITECTURE} script"

; Macro definition
!macro SayHello name
    !ifdef name
        !echo "Hello, ${name}"
    !else
        !echo "Hello, world"
    !endif
!macroend

; Macro usage
!insertmacro SayHello "John Doe"

; Settings
Name "installer_name"
OutFile "installer_name.exe"
RequestExecutionLevel user
CRCCheck on
Unicode true

!ifdef ${ARCHITECTURE}
  InstallDir "$PROGRAMFILES64\installer_name"
!else
  InstallDir "$PROGRAMFILES\installer_name"
!endif

; Pages
Page components
Page instfiles

; Functions
Function PrintTestStrings
  DetailPrint "The install button reads $(^InstallBtn)"
  DetailPrint 'Here comes a$\n$\rline-break!'
  DetailPrint `Escape the dollar-sign: $$`
FunctionEnd

; Sections
Section "section_name" section_index
    Call PrintTestStrings    

    ; NSIS plugin call
    nsExec::ExecToLog "calc.exe"
SectionEnd

