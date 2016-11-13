#include <stdio.h>

void main(int i, int j, int k){
	int r;
	r=i;
	while(i<10){
		r=r+i;
		i=i+1;
	}
	
	return r+r+k;
}
