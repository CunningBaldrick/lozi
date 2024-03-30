#ifndef CRITICAL_H
#define CRITICAL_H

#include "affine_maps.h"
#include "interval_arithmetic.h"
#include "symbol_sequences.h"

// FindVerticalIntervalsObeyingSymbolSequence - Calculate the maximal vertical
// interval in the critical line (returned in Initial) that is mapped to the
// critical line (giving a vertical interval returned in Final) when mapped
// by the given sequence of maps.  If some point of the interval might be in
// the correct half plane (as given by S), then return 'true'.  For example,
// if S[0] is LEFT, Maps[0] is the left Lozi map, S[1] is RIGHT, Maps[1] is
// the right Lozi map, then finds I (aka Initial) such that:
//   - proj_x(I) intersects x<= 0 (corresponds to S[0]); this is always true if
//     I exists, since I is a vertical interval contained in the critical line.
//     Thus S[0] is always ignored (there is no need to initialize it).
//   - proj_x(Maps[0](I)) intersects x >= 0 (corresponds to S[1]).
//   - Final = Maps[1](Maps[0](I)) is contained in the critical line.
bool FindVerticalIntervalsObeyingSymbolSequence(
  const std::vector<AffineMap> &Maps, const SymbolSequence &S, DOUBLE &Initial,
  DOUBLE &Final);

#endif
