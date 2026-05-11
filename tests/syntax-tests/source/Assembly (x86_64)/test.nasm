global enlight

section .data
  red dq 0 ; some comment
  green dq 0
  blue dq 0
  data dq 0
  N dd 0
  M dd 0
  change dd 0
  delta db 0

section .text
enlight:
  call assign_arguments
  call set_data
  call make_deltas
  ret

assign_arguments:
  mov qword[red], rdi
  mov qword[green], rsi
  mov qword[blue], rdx
  mov dword[N], ecx
  mov dword[M], r8d
  mov dword[change], r9d
  mov al, byte[rsp + 16]
  mov byte[delta], al
  ret

set_data:
  mov eax, dword[change]
  cmp eax, 1
  jne not_1
  mov rax, qword[red]
  mov qword[data], rax
  ret
not_1:
  cmp eax, 2
  jne not_2
  mov rax, qword[green]
  mov qword[data], rax
  ret
not_2:
  mov rax, qword[blue]
  mov qword[data], rax
  ret


make_deltas:
  mov ecx, dword[N]
  mov eax, dword[M]
  imul ecx, eax
loop_start:
  call make_delta
  loop loop_start
  ret

make_delta:
  mov rax, qword[data]
  add rax, rcx
  dec rax
  mov dl, byte[delta]
  cmp dl, 0
  jl substracting
adding:
  add dl, byte[rax]
  jc adding_overflow
  mov byte[rax], dl
  ret
adding_overflow:
  mov byte[rax], 255 
  ret
substracting:
  mov r9b, dl
  mov dl, 0
  sub dl, r9b
  mov r8b, byte[rax]
  sub r8b, dl
  jc substracting_overflow
  mov byte[rax], r8b
  ret
; another comment
substracting_overflow:
  mov byte[rax], 0
  ret
