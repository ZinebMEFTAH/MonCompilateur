			# This code was produced by the CERI Compiler
	.section .rodata
FormatUInt: .asciz "%llu\n"

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
	movq $0, %rax
	movb $97, %al
	push %rax	# push char 'a'
	pop %rax
	movb %al, ch(%rip)
	push $0xFFFFFFFFFFFFFFFF		# True
	pop %rax
	movb %al, flag(%rip)
	movzbl flag(%rip), %eax	# Load byte (bool/char)
	push %rax
	# Affichage de type: BOOLEAN
	# print boolean in %rax
	pop    %rax                     # boolean byte (0x00 or 0xFF)
	testb  %al, %al                 # set flags on AL
	setne  %al                      # AL=1 if non-zero, else 0
	movzbq %al, %rax                # zero-extend AL -> full RAX (0 or 1)
	mov    %rax, %rsi               # 2nd arg -> RSI
	leaq   FormatUInt(%rip), %rdi   # 1st arg -> RDI
	movl   $0, %eax                 # no SSE args
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
	ja Vrai8	# If above
	push $0		# False
	jmp Suite8
Vrai8:	push $0xFFFFFFFFFFFFFFFF		# True
Suite8:
	pop %rax
	cmp $0, %rax
	je Else7	# If condition is false, jump to Else
	mov x(%rip), %rax	# Load integer variable
	push %rax
	# Affichage de type: UNSIGNED_INT
	# print integer in %rax
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatUInt(%rip), %rdi   # 1st arg → RDI
	movl   $0, %eax                 # no SSE args
	call   printf@PLT

	jmp Suite7
Else7:
	mov y(%rip), %rax	# Load integer variable
	push %rax
	# Affichage de type: UNSIGNED_INT
	# print integer in %rax
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatUInt(%rip), %rdi   # 1st arg → RDI
	movl   $0, %eax                 # no SSE args
	call   printf@PLT

Suite7:
While11:
	mov x(%rip), %rax	# Load integer variable
	push %rax
	push $0
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	ja Vrai12	# If above
	push $0		# False
	jmp Suite12
Vrai12:	push $0xFFFFFFFFFFFFFFFF		# True
Suite12:
	pop %rax
	cmp $0, %rax
	je EndWhile11	# Exit loop if false
	mov x(%rip), %rax	# Load integer variable
	push %rax
	# Affichage de type: UNSIGNED_INT
	# print integer in %rax
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatUInt(%rip), %rdi   # 1st arg → RDI
	movl   $0, %eax                 # no SSE args
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
	jmp While11
EndWhile11:
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
Repeat16:
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
	# print integer in %rax
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatUInt(%rip), %rdi   # 1st arg → RDI
	movl   $0, %eax                 # no SSE args
	call   printf@PLT

	mov y(%rip), %rax	# Load integer variable
	push %rax
	push $0
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	je Vrai19	# If equal
	push $0		# False
	jmp Suite19
Vrai19:	push $0xFFFFFFFFFFFFFFFF		# True
Suite19:
	pop %rax
	cmp $0, %rax
	je Repeat16	# Repeat if condition is false
EndRepeat16:
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
	#FOR LOOP STARTED
	push $0
	pop %rax
	mov %rax, i(%rip)
	mov i(%rip), %rax    # load initial i
	push $5
	pop %rdx
	mov %rdx, %rbx    # keep limit in callee-saved reg
	jmp TestFor21
LoopFor21:
	mov i(%rip), %rax	# Load integer variable
	push %rax
	# Affichage de type: UNSIGNED_INT
	# print integer in %rax
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatUInt(%rip), %rdi   # 1st arg → RDI
	movl   $0, %eax                 # no SSE args
	call   printf@PLT

	mov i(%rip), %rax    # reload counter
	add $1, %rax
	mov %rax, i(%rip)    # store incremented counter
TestFor21:
	cmp %rbx, %rax    # compare limit (rbx), counter
	jbe LoopFor21
EndFor21:
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
	# Affichage de type: UNSIGNED_INT
	# print integer in %rax
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatUInt(%rip), %rdi   # 1st arg → RDI
	movl   $0, %eax                 # no SSE args
	call   printf@PLT

	#CASE STARTED
	mov x(%rip), %rax	# Load integer variable
	push %rax
	pop %rax		# Pop INTEGER or BOOLEAN into rax
	cmp $1, %rax	# Compare with case label
	je CaseMatch29
	jmp Skip29	# No match, skip this case block
CaseMatch29:
	push $1
	# Affichage de type: UNSIGNED_INT
	# print integer in %rax
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatUInt(%rip), %rdi   # 1st arg → RDI
	movl   $0, %eax                 # no SSE args
	call   printf@PLT

	jmp EndCase27
Skip29:
	cmp $2, %rax	# Compare with case label
	je CaseMatch31
	cmp $0, %rax	# Compare with case label
	je CaseMatch31
	jmp Skip31	# No match, skip this case block
CaseMatch31:
	push $2
	# Affichage de type: UNSIGNED_INT
	# print integer in %rax
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatUInt(%rip), %rdi   # 1st arg → RDI
	movl   $0, %eax                 # no SSE args
	call   printf@PLT

	jmp EndCase27
Skip31:
	cmp $10, %rax	# Compare with case label
	je CaseMatch33
	jmp Skip33	# No match, skip this case block
CaseMatch33:
	push $10
	# Affichage de type: UNSIGNED_INT
	# print integer in %rax
	pop    %rax                     # integer to print
	mov    %rax, %rsi               # 2nd arg → RSI
	leaq   FormatUInt(%rip), %rdi   # 1st arg → RDI
	movl   $0, %eax                 # no SSE args
	call   printf@PLT

	jmp EndCase27
Skip33:
EndCase27:
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
	movq %rbp, %rsp		# Restore the position of the stack's top
	ret			# Return from main function
	.section .note.GNU-stack,"",@progbits
