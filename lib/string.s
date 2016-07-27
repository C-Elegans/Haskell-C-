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
	
