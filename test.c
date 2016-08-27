#include <stdio.h>
#include <stdlib.h>
int glob2;

void main(int i){
    int arr[2];
    *arr = 0xbeef;
    *(arr + 1) = 0xa5a5;
    print_hex(*arr);
    print_hex(*(arr+1));
}
