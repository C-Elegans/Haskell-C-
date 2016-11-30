#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_int(int i){
    char str[6];
    int index;
    int sign;
    sign = i<0;
    i=abs(i);
    index = 4;
    *(str+5) = 0;
    while(i!= 0){
        *(str+index) = '0'+ i%10;
        i=i/10;
        index--;
    }
    if(sign) putc('-');
    puts(str+index+1);
    putc('\n');
}

void main(int i){
    print_int(15);
    print_int(3456);
    print_int(-20);
}

