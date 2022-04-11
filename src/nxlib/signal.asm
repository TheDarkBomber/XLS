default rel
%include "syscalls.inc"

global Abort:function
section .text
Abort:
	call GETPID wrt ..plt
	mov rdi, rax
	mov rsi, 6
	jmp TERMINATE wrt ..plt

global AbnormalExit:function
AbnormalExit:
	call GETPID wrt ..plt
	mov rsi, rdi
	mov rdi, rax
	jmp TERMINATE wrt ..plt
