#include <string.h>
#include <stdio.h>
int main(void){
	char test[20];
	strcpy(test,"test1 ");
	strcat(test,"test2 ");
	strcat(test,"test3\n");
	puts(test);

}
