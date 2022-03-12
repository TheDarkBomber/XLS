#include <stdio.h>
#include <stdint.h>

extern "C" uint32_t putchard(uint32_t X) {
  fputc((char)X, stderr);
  return 0;
}

extern "C" uint32_t printd(uint32_t X) {
  fprintf(stderr, "%u\n", X);
  return 0;
}
