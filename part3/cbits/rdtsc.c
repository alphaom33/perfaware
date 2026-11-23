#include "rdtsc.h"
#include <x86intrin.h>

unsigned long long rdtsc() {
    return __rdtsc();
}
