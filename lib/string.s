strcpy: ;r0 = dest r1 = src
	ld.b r2,[r1]
	st.b [r0],r2
	test r2,r2
	jmp.eq strcpy_end
	add r0,1
	add r1,1
	jmp strcpy
strcpy_end:
	ret

strlen: ;ptr in r0
	xor r2,r2
strlen_loop:
	ld.b r1,[r0]
	test r1,r1
	jmp.eq strlen_end
	add r0,1
	add r2,1
	jmp strlen_loop
strlen_end:
	mov r0,r2
	ret

memcpy: ;r0 - dest, r1 - source, r3 count
	push r0
	xor r3,r3
	or r3,r0
	or r3,r1
	or r3,r2
	test r3,1
	jmp.eq memcpy_2
memcpy_loop:
	ld.b r3,[r1]
	st.b [r0],r3
	add r0,1
	add r1,1
	sub r2,1
	jmp.ne memcpy_loop
	pop r0
	ret
	
memcpy_2:
	ld r3,[r1]
	st [r0],r3
	add r0,2
	add r1,2
	sub r2,2
	jmp.ne memcpy_2
	pop r0
	ret

strcmp: ;str1 in r0, str1 in r1
	ld.b r2,[r0]
	ld.b r3,[r1]
	test r2,r2
	jmp.eq strcmp_end
	add r0,1
	add r1,1
	cmp r2,r3
	jmp.eq strcmp
	sub r2,r3
strcmp_end:
mov r0,r2
ret

strchr:
	and r1,0xff
strchr_loop:
	ld.b r2,[r0]
	test r2,r2
	jmp.eq strchr_end
	add r0,1
	cmp r2,r1
	jmp.ne strchr_loop
	sub r0,1
strchr_end:
	ret
