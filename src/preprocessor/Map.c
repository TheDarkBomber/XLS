#include "Map.h"
#include <stdlib.h>
#include <string.h>
#include <search.h>

void *DefineRootNode = 0;

int DefineCompare(const void *left, const void *right) {
	const DefineMap *LM = (DefineMap*)left;
	const DefineMap *RM = (DefineMap*)right;
	return strcmp(LM->Key, RM->Key);
}

void AddDefine(char *key, char *value) {
	DefineMap *M = malloc(sizeof(DefineMap));
	M->Key = key;
	M->Value = value;
	tsearch(M, &DefineRootNode, DefineCompare);
}

char* FindDefine(char* key) {
	DefineMap *M = malloc(sizeof(DefineMap));
	M->Key = key;
	void *access = tfind(M, &DefineRootNode, DefineCompare);
	if (access == NULL) {
		free(M);
		return NULL;
	}
	return (*(DefineMap**)access)->Value;
}


