#include <stdio.h>
void main(void){
	int j;
	j=5;
	print_hex(j&4);
	j=j|19;
	print_hex(j);
	print_hex(j^37);
	print_hex(~j);

}
