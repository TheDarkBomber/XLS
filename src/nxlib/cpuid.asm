%include "common.inc"
global i.cpuvendor:function
global cpuinfo:function
global i.cpufeature_available:function

section .text
i.cpuvendor:
	push rbx ; Save registers used to stack.
	push rcx
	push rdx
	xor rax, rax ; Set RAX = 0.
	cpuid ; CPUID with RAX = 0, get CPU vendor.
	mov qword [rdi], rbx ; Record vendor to first argument.
	mov qword [rdi + 4], rdx
	mov qword [rdi + 8], rcx
	pop rdx ; Antisave registers used.
	pop rcx
	pop rbx
	ret

cpuinfo:
	push rcx
	push rbx
	push r9
	xor rax, rax ; Set RAX = 0.
	inc rax ; Actually, set RAX = 1.
	mov r9, rdx
	cpuid ; CPUID with RAX = 1, get CPU information.
	mov dword [rdi], edx
	mov dword [rsi], ecx
	mov dword [r9], ebx
	mov rdx, r9
	pop r9
	pop rcx
	pop rdx
	ret

i.cpufeature_available:
	push rdx ; Save registers used to stack.
	push rbx
	push rcx
	xor rax, rax
	inc rax ; Set RAX = 1.
	cpuid ; CPUID with RAX = 1, get CPU information.
	cmp rsi, 1 ; ECX feature set?
	cmove rdx, rcx ; If so, RDX = RCX.
%ifdef level3 ; If AMD64-v3
	shrx rax, rdx, rdi ; RAX = RDX >> RDI, RDI is first argument, RAX is return.
%else ; If less than AMD64-v3
	mov rcx, rdi ; Set RCX to value of first argument.
	mov rax, rdx ; Set return register to RDX, the feature set.
	shr rax, cl ; Shift the feature set by first argument
%endif
	pop rcx ; Antisave registers used.
	pop rbx
	pop rdx
	ret
