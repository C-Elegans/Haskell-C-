#include <stdio.h>


void main(int i){
    int i=3;
 	int* j = &i;
 	*(j+2)=4;
 	print_hex(i);
 	return i;
}
