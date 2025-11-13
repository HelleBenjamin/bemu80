; SPDX-License-Identifier: GPL-2.0-or-later
; Copyright (c) 2025 Benjamin Helle
;
; io.asm
; Example of how to read input from stdin and write output to stdout
; You can use this as a template for your own assembly code

  org 0x0000

main:
  ld    sp, $fffe   ; Set SP to top of memory
  ld    hl, msg1    ; Load address of message
  call  puts        ; Print it
  ld    hl, namebuf ; Load address of name buffer
  call  gets        ; Get name
  ld    hl, msg2    ; Load address of message
  call  puts        ; Print it
  ld    a, $0a      ; Load newline
  call  putc        ; Print it
  halt              ; Exit


getc:
  in  a, ($81)  ; Read from stdin
  out ($81), a  ; Echo
  or  a         ; If not valid, read again
  ret nz        ; If valid, return in A
  jr  getc      ; Loop until valid
  ret

gets:
  call getc     ; Get character
  cp  $0a       ; If CR(enter), return
  jr  z, gets_end
  ld  (hl), a   ; Save character
  inc hl        ; Next character
  jr  gets      ; Loop
gets_end:
  ld  (hl), $00 ; Null-terminate string
  ret

putc:
  out ($81), a  ; Write to stdout
  ret

puts:
  ld  a, (hl)   ; Get character from memory
  or  a         ; Check if null
  ret z         ; If so, return
  out ($81), a  ; Output it
  inc hl        ; Next character
  jr  puts      ; Loop

msg1:
  db "Hello! What's your name? ", 0

msg2:
  db "Hello, "

namebuf: ; Free space for name
  db 0
