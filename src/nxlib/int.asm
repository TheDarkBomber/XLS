%include "common.inc"
global itoa
section .text
itoa:
	push r9
	push rbx ; Save volatile registers.
	push rcx
	mov r9, rdx ; Store radix in R9.
	xor rbx, rbx ; Prepare RBX to zero.
	mov rax, rdi ; Set RAX to first argument, the integer.
	mov dl, 128 ; Sentinel value.
	push rdx ; Push sentinel.
	.loop:
	xor rdx, rdx ; Division uses RAX and RDX, we don't need to use RDX.
	mov rcx, r9 ; Prepare RCX for division.
	div rcx ; Divide RAX by R9.
	cmp rdx, 10 ; RDX = remainder.
	jge .alpha
	or rdx, 48 ; Convert RDX to ASCII.
	push rdx ; Push RDX to stack.
	jmp .alpha.end
	.alpha:
	or rdx, 65 ; Convert RDX to ASCII. ((RDX | 65) - 10)
	sub rdx, 10
	push rdx ; Push RDX to stack.
	.alpha.end:
	test rax, rax ; Is quotient zero? i.e., no more digits.
	jnz .loop
	.write:
	pop rdx ; Stack is LIFO, this gets digits in order.
	cmp rdx, 128 ; Is sentinel?
	jge .after_write
	mov qword [rsi + rbx], rdx ; str[RBX] = RDX
	inc rbx ; Increment counter.
	jmp .write
	.after_write:
	mov rax, rbx ; Set return value to RBX, the length.
	pop rcx ; Antisave volatile registers.
	pop rbx
	pop r9
	ret
