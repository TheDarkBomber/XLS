%include "common.inc"
%macro defsyscall 2
global %1:function
%1:
	mov rax, %2
	syscall
	ret
%endmacro

section .text
defsyscall READ, 0
defsyscall WRITE, 1
defsyscall OPEN, 2
defsyscall CLOSE, 3
defsyscall BRK, 12
defsyscall GETPID, 39
defsyscall FORK, 57
defsyscall EXIT, 60
defsyscall TERMINATE, 62
defsyscall FSYNC, 74
