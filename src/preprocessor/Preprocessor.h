#ifndef __PREPROCESSOR_XPP_H_
#define __PREPROCESSOR_XPP_H_
#include "File.h"

void Preprocess(PPUnit stream);
void PreprocessInclude(PPUnit stream);
void PreprocessDefine(PPUnit stream);

#endif
