#include "symbol_sequences.h"

bool Increment(SymbolSequence::iterator Begin, SymbolSequence::iterator End) {
  for (; Begin != End; ++Begin) {
    if (*Begin == LEFT) {
      *Begin = RIGHT;
      return false;
    }
    *Begin = LEFT;
  }
  return true;
}

bool operator<(SymbolSequence const &LHS, SymbolSequence const &RHS) {
  const size_t min_size = std::min(LHS.size(), RHS.size());
  for (unsigned i = 0; i != min_size; ++i)
    if (LHS[i] != RHS[i])
      return LHS[i] < RHS[i];
  return LHS.size() < RHS.size();
}
