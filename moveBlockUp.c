#include "ARMCFunctions.h"

void moveBlockUp(long long *from, long long *to, long long *numElemsp) {
	long long numElems = *numElemsp;
	long long *fptr = from + numElems - 1;
	long long *tptr = to + numElems - 1;

	while (fptr >= from) {
		*tptr = *fptr;
		--tptr;
		--fptr;
	}
}