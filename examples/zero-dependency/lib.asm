;; BSD 2-Clause License
;;
;; Copyright (c)  2022, name
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


global _start ; Entrypoint.
extern main ; Defined in main.xls

section .text 									; Code section.
_start:
	; Call main with argc
	mov rdi, [rsp] 								; RSP stores the argument count.
	call main

	; EXIT(Return code)
	mov rdi, rax ; Set return status to return value of main.
	mov rax, 60 ; Indicate syscall 60, which is EXIT.
	syscall	; Initiate the syscall.

global printchar ; Function, mark global.
printchar:
	mov [pchar_char], rdi ; Set character we want to print to first argument.
	push rdi ; Push later, so we can return the first argument.

  ; WRITE(File Descriptor, Buffer to message, Size of message buffer)
	mov rdi, 1 ; STDOUT = 1
	mov rsi, pchar_char ; Memory address of where our "message" is stored.
	mov rdx, 1 ; Length of message, 1, as it is a single character.
	mov rax, 1 ; Indicate syscall 1, which is WRITE.
	syscall ; Initiate the syscall.

	pop rdi ; Antisave the first argument.
	mov rax, rdi ; Set return value to value of RDI.
	ret

section .bss ; Section for uninitialised data.
pchar_char:	resb 1 ; Reserve 1 byte, since we have one character.
