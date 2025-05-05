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
