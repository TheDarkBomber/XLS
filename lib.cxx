#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

extern "C" uint32_t printd(uint32_t X) {
  fprintf(stderr, "%u\n", X);
  return 0;
}

extern "C" uint32_t scand() {
	char buffer[10], c;
	for(uint8_t i = 0; (c = getchar()) != '\n' && i < 10; i++) {
		buffer[i] = c;
	}
	return atoi(buffer);
}
