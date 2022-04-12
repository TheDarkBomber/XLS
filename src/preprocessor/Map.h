#ifndef __MAP_XPP_H_
#define __MAP_XPP_H_

typedef struct {
	char* Key;
	char* Value;
} DefineMap;

int DefineCompare(const void *left, const void *right);
void AddDefine(char* key, char* value);
char* FindDefine(char* key);

#endif
