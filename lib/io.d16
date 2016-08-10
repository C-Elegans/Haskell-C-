print_hex:
	mov r1,0x30 ;0
	st.b [0xff02],r1
	mov r1,0x78 ;x
	st.b [0xff02],r1
	mov r3,r0
	shr r3,12
	and r3,0x0f
	ld.b r1,[r3+hexdata]
	st.b [0xff02],r1
	
	mov r3,r0
	shr r3,8
	and r3,0x0f
	ld.b r1,[r3+hexdata]
	st.b [0xff02],r1
	
	mov r3,r0
	shr r3,4
	and r3,0x0f
	ld.b r1,[r3+hexdata]
	st.b [0xff02],r1
	
	mov r3,r0
	and r3,0x0f
	ld.b r1,[r3+hexdata]
	st.b [0xff02],r1
	
	mov r1,10
	st.b [0xff02],r1
	ret
hexdata: ;ascii hex constants
	.ascii "0123456789ABCDEF"

putc:
	st.b [0xff02],r0
	ret
puts:
	ld.b r1,[r0]
	test r1,r1
	jmp.eq puts_end
	st.b [0xff02],r1
	add r0,1
	jmp puts
puts_end:
	ret
