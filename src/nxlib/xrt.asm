global _start ; Entrypoint.
extern main ; Defined in the object file that this should link statically with.

section .text ; Code section.
_start:
	; Call main with argc
	mov rdi, [rsp] ; RSP stores the argument count.
	mov rsi, rsp ; Location of argument vector - 8.
	add rsi, 8 ; Real location of argument vector.
	call main

	; EXIT(Return code)
	mov rdi, rax ; Set return status to return value of main.
	mov rax, 60 ; Indicate syscall 60, which is EXIT.
	syscall	; Initiate the syscall.
