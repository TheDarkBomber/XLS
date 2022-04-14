default rel
%include "syscalls.inc"
%include "string.inc"
%include "streams.inc"
%include "signals.inc"

global i.abort:function
section .text
i.abort:
	push rdi ; Save first argument, used as argument for strlen.
	call strlen wrt ..plt
	mov rdx, rax
	pop rdi ; Antisave first argument.
	mov rsi, rdi ; Move first argument to second argument of WRITE.
	mov rdi, STDERR
	call WRITE wrt ..plt
	call GETPID wrt ..plt
	mov rdi, rax
	mov rsi, SIGABRT
	jmp TERMINATE wrt ..plt


global i.raisesignal:function
i.raisesignal:
	call GETPID wrt ..plt
	mov rsi, rdi
	mov rdi, rax
	jmp TERMINATE wrt ..plt
