#include <stdio.h>

void main(void){
	int i;
	i=0;
	while(i<6){
		if(i>2){
			print_hex(i);
		}else{
			print_hex(1);
		}
		i=i+1;
	}

}

