#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*void print_int(int i){*/
    /*char str[7];*/
    /*int index;*/
    /*int sign;*/
    /*sign = i<0;*/
    /*i=abs(i);*/
    /*index = 4;*/
    /**(str+6) = 0;*/
    /**(str+5) = 10;*/
    /*while(i!= 0){*/
        /**(str+index) = '0'+ i%10;*/
        /*i=i/10;*/
        /*index--;*/
    /*}*/
    /*if(sign) putc('-');*/
    /*puts(str+index+1);*/
/*}*/

void main(int i){
    int j;
    int k;
    int l;
    int *p;
    int *p2;
    i=3;
    j=4; 
    k=10;
    p=&k;
    p2=&j;
    *p=4;
    *p = *p + 1;
    *p2 = *p;
    print_hex(i+j+k);
    //puts("test");
    return i+j+k;

}

