#include <stdio.h>
#include <string.h>
int strlen_test(char* str){
    char* sav = str;
    while(*str != 0){
        str=str+1;
    }
    return str-sav;
}
char* strcpy(char* dest, char* src){
    char* sav = dest;
    while(*src != 0 ){
        *dest=*src;
        dest=dest+1;
        src=src+1;
    }
    return sav;
}

void main(void){
    print_hex(strlen_test("hello!"));
    print_hex(strlen("hello!"));
}

