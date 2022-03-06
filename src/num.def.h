#ifndef __NUMBER_XLS_DEF_
#define __NUMBER_XLS_DEF_
#include <stdint.h>

typedef uint8_t byte;
typedef uint16_t word;
typedef uint32_t dword;
typedef uint64_t qword;
typedef unsigned uint;

typedef int16_t sword;
typedef int32_t sdword;
typedef int64_t sqword;

typedef uintptr_t uintp;
typedef intptr_t intp;

#define BITS(x, y) unsigned int x : y

#endif
