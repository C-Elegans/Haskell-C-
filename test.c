#include <stdio.h>

int test(int i, int j, int k){
	return 5;
}
void main(int i, int j){
	int k=3;
	j=j+j+1+i;
	i=test(j+1,2,3);
	print_hex(i);
	//print_hex(j);
}

