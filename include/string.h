#define size_t int
#define NULL (void*)0
//Memory functions
void* memcpy(void* dest, void* src, size_t nbytes);
void* memset(void* dest, int val, size_t nbytes);
void* memchr(void* mem, int val, size_t nbytes);
int memcmp(void* data1, void* data2, size_t nbytes);
//String functions
char* strcpy(char* dest, char* source);
size_t strlen(char* str);
int strcmp(char* str1, char* str2);
char* strchr(char* string, int chr);

