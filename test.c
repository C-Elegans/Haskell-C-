#include <stdio.h>
int a(int i){
	int k;
	int j = i+i;
	k=j*2;
	return k;
	
}


void main(int i, int j){
	int k;
	int j = i+i+j;
	k = a(j);
	return k*2;
}
