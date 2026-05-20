.data

.balign 4
red: .word 0
green: .word 0
blue: .word 0

.text
.global grayscale
.func grayscale

grayscale:
assign:
  /* some comment */
  ldr ip, addr_red
  str r3, [ip]
  ldr ip, addr_green
  ldmfd r13!, {r3}
  str r3, [ip]
  ldr ip, addr_blue
  ldmfd r13!, {r3}
  str r3, [ip]
  stmfd r13!, {r4-r8}
  ldr ip, addr_red
  ldr r3, [ip]
  ldr ip, addr_green
  ldr r4, [ip]
  ldr ip, addr_blue
  ldr r5, [ip] /* another comment */
grayscale_loop:
  ldrb r6, [r1]
  mul r6, r3, r6
  add r1, r1, #1
  ldrb r7, [r1]
  mul r7, r4, r7
  add r1, r1, #1
  ldrb r8, [r1]
  mul r8, r5, r8
  add r1, r1, #1
  add r6, r6, r7
  add r6, r6, r8
  asr r6, r6, #8
  str r6, [r2]
  add r2, r2, #1
  sub r0, r0, #1
  cmp r0, #0
  bne grayscale_loop
  ldmfd r13!, {r4-r8}
  stmfd r13!, {r0-r1}
  bx lr

addr_red: .word red
addr_green: .word green
addr_blue: .word blue

