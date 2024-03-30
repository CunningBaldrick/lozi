#ifndef SYMBOL_SEQUENCES_H
#define SYMBOL_SEQUENCES_H

#include <iostream>
#include <vector>

enum Symbol { LEFT, RIGHT };

typedef std::vector<Symbol> SymbolSequence;

extern bool Increment(SymbolSequence::iterator Begin, SymbolSequence::iterator End);
// Increment - Increase the given slice of a symbol sequence.  Return 'true' if
// the elements all wrapped around back to LEFT.

extern bool operator<(SymbolSequence const &, SymbolSequence const &);

inline std::ostream& write(std::ostream& os,
                           SymbolSequence::const_iterator Begin,
                           SymbolSequence::const_iterator End) {
  for (; Begin != End; ++Begin)
    os << (*Begin == LEFT ? 'L' : 'R');
  return os;
}

inline std::ostream& operator<<(std::ostream& os, const SymbolSequence& s) {
  return write(os, s.begin(), s.end());
}

#endif
