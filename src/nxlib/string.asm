default rel

global strlen:function
strlen:
	push rbx ; Save non-argument registers.
	push rcx
	mov rbx, rdi
	xor al, al ; AL = lowest byte of RAX
	mov rcx, 0xFFFFFFFFFFFFFFFF ; Maximum possible bytes a string can contain on AMD64
	repne scasb ; Scan string while RDI != 0 (AL)
	sub rdi, rbx ; Subtract RBX (final string character) from RDI
	mov rax, rdi ; RAX = return value.
	pop rcx
	pop rbx ; Antisave non-argument registers.
	ret
