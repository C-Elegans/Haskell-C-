mul: ;takes 2 parameters
mov r3,r0
xor r0,r0
mul_loop:
	cmp r1,0
	jmp.eq mul_end
	test r1,1
	jmp.eq mul_skip
	add r0,r3
mul_skip:
	shl r3,1
	shr r1,1
	cmp r1,0
	jmp mul_loop
mul_end:
ret

