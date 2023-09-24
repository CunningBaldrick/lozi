#include <fenv.h>

int tonearest  (void) { return FE_TONEAREST; }
int upward     (void) { return FE_UPWARD; }
int downward   (void) { return FE_DOWNWARD; }
int towardzero (void) { return FE_TOWARDZERO; }
