#ifndef __PREPROCESSOR_XPP_H_
#define __PREPROCESSOR_XPP_H_
#include <stdio.h>

void Preprocess(FILE* stream);
void PreprocessInclude(FILE* stream);
void PreprocessDefine(FILE* stream);

#endif
