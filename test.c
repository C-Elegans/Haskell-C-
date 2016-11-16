#include <stdio.h>
void test(char* str){
	while(*str != 0){
		*str = *str + 1;
	}
}


void main(int i, int j, int k){
	char* str = "testing";
	test(str);
	puts(str);
}
