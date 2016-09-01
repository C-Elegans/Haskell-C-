#include <stdio.h>
#include <stdlib.h>
int glob2;

void main(int i){
    int i;
    i=0;
    while(i<16){
        print_hex(rand()&0xff);
        i = i+1;
    }
}
