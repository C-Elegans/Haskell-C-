# 1 "test.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "test.c"
# 1 "include/stdio.h" 1
void puts(char* string);
void putc(char* c);
void print_hex(int i);
# 2 "test.c" 2
# 1 "include/string.h" 1
char* strcpy(char* dest, char* source);
int strlen(char* str);
void* memcpy(void* dest, void* src, int nbytes);
# 3 "test.c" 2

int glob2;

void main(int i){
 char t[10];
 strcpy(t,"testing\n");
 puts(t);
}
