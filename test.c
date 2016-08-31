#include <stdio.h>
#include <stdlib.h>
int glob2;

void main(int i){
    int* ptr;
    int* ptr2;
    ptr = sbrk(4);
    ptr2 = sbrk(4);
    print_hex((int)ptr);
    print_hex((int)ptr2);
}
