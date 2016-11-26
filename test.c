#include <stdio.h>
int strlen_test(char* str){
    int count = 0;
    while(*str != 0){
        str=str+1;
        count = count+1;
    }
    return count;
}

void main(void){
    print_hex(strlen_test("hello!"));
}

