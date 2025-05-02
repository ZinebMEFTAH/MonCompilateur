			# This code was produced by the CERI Compiler
	.data
	.align 8
x:	.quad 0
y:	.quad 0
i:	.quad 0
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, %rbp	# Save the position of the stack's top
	push $1
	pop x
	push $2
	pop y
	push x
	pop %rax
	cmp $0, %rax
	je Else1	# If condition is false, jump to Else
	push $3
	pop y
	jmp Suite1
Else1:
	push $4
	pop y
Suite1:
While2:
	push x
	pop %rax
	cmp $0, %rax
	je EndWhile2	# Exit loop if false
	push y
	push $1
	pop %rbx
	pop %rax
	addq	%rbx, %rax	# ADD
	push %rax
	pop y
	push x
	push $1
	pop %rbx
	pop %rax
	subq	%rbx, %rax	# SUB
	push %rax
	pop x
	jmp While2
EndWhile2:
	push $0
	pop i
	mov i, %rax
	push $3
	pop %rdx
	jmp TestFor3
LoopFor3:
	push y
	push $1
	pop %rbx
	pop %rax
	addq	%rbx, %rax	# ADD
	push %rax
	pop y
	add $1, %rax
	mov %rax, i
TestFor3:
	cmp %rax, %rdx
	jb LoopFor3	# If still less than limit, continue
EndFor3:
	push $5
	pop x
	push $6
	pop y
	movq %rbp, %rsp		# Restore the position of the stack's top
	ret			# Return from main function
