#include <stdio.h>
int glob;
void main(int i){
	int t;
	
	t=2;
	t=t+3;
	print_hex(t);
	t=t*4;
	print_hex(t);
	t=t-1;
	print_hex(t);
}
