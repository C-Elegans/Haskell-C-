#include <stdio.h>
#include <string.h>
void main(void){
    char str[20];
    strcpy(str,"test\n");
    *str = 'r';
    puts(str);
    *(str+1) = 'k';
    puts(str);
}

