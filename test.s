			# This code was produced by the CERI Compiler
	.data
	FormatString1:    .string "%llu\n"
	FormatString2:    .string "%f\n"
	FormatString3:    .string "%c\n"
	.align 8
i:	.quad 0     # 32-bit unsigned integer
x:	.quad 0     # 32-bit unsigned integer
y:	.quad 0     # 32-bit unsigned integer
ch:	.byte 0      # character
flag:	.byte 0        # 1 byte boolean
d:	.double 0.0  # 64-bit double
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, %rbp	# Save the position of the stack's top
	push $10
	pop %rax
	mov %rax, x(%rip)
	push $5
	pop %rax
	mov %rax, y(%rip)
	subq $8,%rsp			# allocate 8 bytes on stack's top
	movl	$0, (%rsp)	# Conversion of 2.5 (32 bit high part)
	movl	$1074003968, 4(%rsp)	# Conversion of 2.5 (32 bit low part)
	movsd (%rsp), %xmm0
	addq $8, %rsp
	movsd %xmm0, d(%rip)
	mov x(%rip), %rax	# Load integer variable
	push %rax
	# Affichage de type: UNSIGNED_INT
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatString1(%rip), %rdi# 1st arg → RDI
	movl   $0, %eax                  # no SSE regs
	call   printf@PLT
	movsd d(%rip), %xmm0	# Load double variable
	subq $8, %rsp
	movsd %xmm0, (%rsp)
	# Affichage de type: DOUBLE
	movsd  (%rsp), %xmm0            # load double from stack
	addq   $8, %rsp                 # pop it
	subq   $8, %rsp                 # align stack (RSP%16==8)
	leaq   FormatString2(%rip), %rdi# format string → RDI
	movl   $1, %eax                  # one SSE-reg in varargs
	call   printf@PLT
	addq   $8, %rsp                 # restore stack
	mov x(%rip), %rax	# Load integer variable
	push %rax
	mov y(%rip), %rax	# Load integer variable
	push %rax
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	ja Vrai7	# If above
	push $0		# False
	jmp Suite7
Vrai7:	push $0xFFFFFFFFFFFFFFFF		# True
Suite7:
	pop %rax
	cmp $0, %rax
	je Else6	# If condition is false, jump to Else
	mov x(%rip), %rax	# Load integer variable
	push %rax
	# Affichage de type: UNSIGNED_INT
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatString1(%rip), %rdi# 1st arg → RDI
	movl   $0, %eax                  # no SSE regs
	call   printf@PLT
	jmp Suite6
Else6:
	mov y(%rip), %rax	# Load integer variable
	push %rax
	# Affichage de type: UNSIGNED_INT
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatString1(%rip), %rdi# 1st arg → RDI
	movl   $0, %eax                  # no SSE regs
	call   printf@PLT
Suite6:
While10:
	mov x(%rip), %rax	# Load integer variable
	push %rax
	push $0
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	ja Vrai11	# If above
	push $0		# False
	jmp Suite11
Vrai11:	push $0xFFFFFFFFFFFFFFFF		# True
Suite11:
	pop %rax
	cmp $0, %rax
	je EndWhile10	# Exit loop if false
	mov x(%rip), %rax	# Load integer variable
	push %rax
	# Affichage de type: UNSIGNED_INT
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatString1(%rip), %rdi# 1st arg → RDI
	movl   $0, %eax                  # no SSE regs
	call   printf@PLT
	mov x(%rip), %rax	# Load integer variable
	push %rax
	push $1
	pop %rbx
	pop %rax
	subq %rbx, %rax	# SUB
	push %rax
	pop %rax
	mov %rax, x(%rip)
	jmp While10
EndWhile10:
Repeat14:
	mov y(%rip), %rax	# Load integer variable
	push %rax
	push $1
	pop %rbx
	pop %rax
	subq %rbx, %rax	# SUB
	push %rax
	pop %rax
	mov %rax, y(%rip)
	mov y(%rip), %rax	# Load integer variable
	push %rax
	# Affichage de type: UNSIGNED_INT
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatString1(%rip), %rdi# 1st arg → RDI
	movl   $0, %eax                  # no SSE regs
	call   printf@PLT
	mov y(%rip), %rax	# Load integer variable
	push %rax
	push $0
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	je Vrai17	# If equal
	push $0		# False
	jmp Suite17
Vrai17:	push $0xFFFFFFFFFFFFFFFF		# True
Suite17:
	pop %rax
	cmp $0, %rax
	je Repeat14	# Repeat if condition is false
EndRepeat14:
	push $0
	pop %rax
	mov %rax, i(%rip)
	mov i, %rax
	push $5
	pop %rdx
	jmp TestFor18
LoopFor18:
	mov i(%rip), %rax	# Load integer variable
	push %rax
	# Affichage de type: UNSIGNED_INT
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatString1(%rip), %rdi# 1st arg → RDI
	movl   $0, %eax                  # no SSE regs
	call   printf@PLT
	add $1, %rax
	mov %rax, i
TestFor18:
	cmp %rax, %rdx
	jb LoopFor18	# If still less than limit, continue
EndFor18:
	mov x(%rip), %rax	# Load integer variable
	push %rax
	pop %rax		# Pop INTEGER or BOOLEAN into rax
	cmp $1, %rax	# Compare with case label
	je CaseMatch24
	jmp Skip24	# No match, skip this case block
CaseMatch24:
	push $1
	# Affichage de type: UNSIGNED_INT
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatString1(%rip), %rdi# 1st arg → RDI
	movl   $0, %eax                  # no SSE regs
	call   printf@PLT
	jmp EndCase22
Skip24:
	cmp $2, %rax	# Compare with case label
	je CaseMatch26
	cmp $3, %rax	# Compare with case label
	je CaseMatch26
	jmp Skip26	# No match, skip this case block
CaseMatch26:
	push $2
	# Affichage de type: UNSIGNED_INT
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatString1(%rip), %rdi# 1st arg → RDI
	movl   $0, %eax                  # no SSE regs
	call   printf@PLT
	jmp EndCase22
Skip26:
	cmp $10, %rax	# Compare with case label
	je CaseMatch28
	jmp Skip28	# No match, skip this case block
CaseMatch28:
	push $10
	# Affichage de type: UNSIGNED_INT
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatString1(%rip), %rdi# 1st arg → RDI
	movl   $0, %eax                  # no SSE regs
	call   printf@PLT
	jmp EndCase22
Skip28:
EndCase22:
	push $0xFFFFFFFFFFFFFFFF		# True
	pop %rax
	movb %al, flag(%rip)
	movq $0, %rax
	movb $65, %al
	push %rax	# push char 'A'
	pop %rax
	movb %al, ch(%rip)
	movq %rbp, %rsp		# Restore the position of the stack's top
	ret			# Return from main function
	.section .note.GNU-stack,"",@progbits
