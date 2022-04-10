%macro defsyscall 2
global %1
%1:
	mov rax, %2
	syscall
	ret
%endmacro

section .text
defsyscall READ, 0
defsyscall WRITE, 1
defsyscall EXIT, 60
