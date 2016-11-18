#include <stdio.h>

int test(int i, int j, int k){
	return i+j;
}
void main(int i){
	int k=3;
	i=test(i,2,3);
	print_hex(i);
	//print_hex(j);
}

