#include <stdio.h>
/*void print_hex(int i){*/
    /*printf("%04x\n",i);*/
/*}*/
int mod(int num,int den){
    int div = num/den;
    return num-div*den;
}
int gcd(int a, int b);
int gcd(int a, int b){
    if (a==0) return b;
    return gcd(mod(b,a),a);
}

void main(void){
    print_hex(gcd(15,6));
}

