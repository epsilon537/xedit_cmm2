#include "ARMCFunctions.h"

void moveBlockDown(long long *from, long long *to, long long *numElemsp) {
	long long *endp = from + *numElemsp;

	while (from < endp) {
		*to = *from;
		++to;
		++from;
	}
}