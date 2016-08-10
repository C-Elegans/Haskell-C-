#define size_t int
#define NULL (void*)0
void* memcpy(void* dest, void* src, size_t nbytes);
void* memset(void* dest, int val, size_t nbytes);
//String functions
char* strcpy(char* dest, char* source);
size_t strlen(char* str);
int strcmp(char* str1, char* str2);
char* strchr(char* string, int chr);

