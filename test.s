			# This code was produced by the CERI Compiler
	.section .rodata
FormatUInt: .asciz "%llu\n"

	.data
	FormatString1:    .string "%llu\n"
	FormatString2:    .string "%f\n"
	FormatString3:    .string "%c\n"
	.align 8
a:	.quad 0     # 32-bit unsigned integer
b:	.quad 0     # 32-bit unsigned integer
c:	.quad 0     # 32-bit unsigned integer
counter:	.quad 0     # 32-bit unsigned integer
i:	.quad 0     # 32-bit unsigned integer
j:	.quad 0     # 32-bit unsigned integer
result:	.quad 0     # 32-bit unsigned integer
d1:	.double 0.0  # 64-bit double
d2:	.double 0.0  # 64-bit double
d3:	.double 0.0  # 64-bit double
dTotal:	.double 0.0  # 64-bit double
cond:	.byte 0        # 1 byte boolean
flag:	.byte 0        # 1 byte boolean
success:	.byte 0        # 1 byte boolean
letter:	.byte 0      # character
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, %rbp	# Save the position of the stack's top
	push $3
	pop %rax
	mov %rax, a(%rip)
	push $2
	pop %rax
	mov %rax, b(%rip)
	push $1
	pop %rax
	mov %rax, c(%rip)
	subq $8,%rsp			# allocate 8 bytes on stack's top
	movl	$0, (%rsp)	# Conversion of 2 (32 bit high part)
	movl	$1073741824, 4(%rsp)	# Conversion of 2 (32 bit low part)
	movsd (%rsp), %xmm0
	addq $8, %rsp
	movsd %xmm0, d1(%rip)
	subq $8,%rsp			# allocate 8 bytes on stack's top
	movl	$0, (%rsp)	# Conversion of 1.5 (32 bit high part)
	movl	$1073217536, 4(%rsp)	# Conversion of 1.5 (32 bit low part)
	movsd (%rsp), %xmm0
	addq $8, %rsp
	movsd %xmm0, d2(%rip)
	mov a(%rip), %rax	# Load integer variable
	push %rax
	push $1
	pop %rbx
	pop %rax
	addq %rbx, %rax
	push %rax
	movsd d1(%rip), %xmm0	# Load double variable
	subq $8, %rsp
	movsd %xmm0, (%rsp)
	mov a(%rip), %rax	# Load integer variable
	push %rax
	# Promote type2 (top of stack) to double
	pop %rax
	cvtsi2sd %rax, %xmm0
	subq $8, %rsp
	movsd %xmm0, (%rsp)
	fldl (%rsp)           # Load type2 → st(0)
	addq $8, %rsp
	fldl (%rsp)           # Load type1 → st(0), type2 → st(1)
	addq $8, %rsp
	faddp %st, %st(1)
	subq $8, %rsp         # Make space
	fstpl (%rsp)          # Store result
	mov a(%rip), %rax	# Load integer variable
	push %rax
	mov c(%rip), %rax	# Load integer variable
	push %rax
	pop %rbx
	pop %rax
	subq %rbx, %rax
	push %rax
	mov b(%rip), %rax	# Load integer variable
	push %rax
	pop %rbx
	pop %rax
	addq %rbx, %rax
	push %rax
	# Promote type2 (top of stack) to double
	pop %rax
	cvtsi2sd %rax, %xmm0
	subq $8, %rsp
	movsd %xmm0, (%rsp)
	fldl (%rsp)           # Load type2 → st(0)
	addq $8, %rsp
	fldl (%rsp)           # Load type1 → st(0), type2 → st(1)
	addq $8, %rsp
	fmulp %st, %st(1)       # DOUBLE MUL
	subq $8, %rsp         # Make space
	fstpl (%rsp)          # Store result
	# Promote type1 (second on stack) to double
	pop %rax		# type2 (already promoted)
	pop %rbx		# type1 to be promoted
	cvtsi2sd %rbx, %xmm1
	subq $8, %rsp
	movsd %xmm1, (%rsp)
	subq $8, %rsp
	movsd %xmm0, (%rsp)	# re-push type2
	fldl (%rsp)           # Load type2 → st(0)
	addq $8, %rsp
	fldl (%rsp)           # Load type1 → st(0), type2 → st(1)
	addq $8, %rsp
	fmulp %st, %st(1)       # DOUBLE MUL
	subq $8, %rsp         # Make space
	fstpl (%rsp)          # Store result
	movsd (%rsp), %xmm0
	addq $8, %rsp
	movsd %xmm0, d3(%rip)
	movsd d3(%rip), %xmm0	# Load double variable
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
	movq %rbp, %rsp		# Restore the position of the stack's top
	ret			# Return from main function
	.section .note.GNU-stack,"",@progbits
