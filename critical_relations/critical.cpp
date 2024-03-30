#include "critical.h"
#include <cassert>

// ComputeVerticalIntervalsMappedToEachOther - Calculate the maximal vertical
// interval in the critical line (returned in Initial) that is mapped to the
// critical line (giving a vertical interval returned in Final).
void ComputeVerticalIntervalsMappedToEachOther(AffineMap &M, DOUBLE &Initial,
                                               DOUBLE &Final) {
  MATRIX A = M.getLinearPart();
  VECTOR B = M.getOffsetPart();

  Initial = -B(0)/A(0,1);
  Final = A(1,1)*Initial + B(1);
}

bool FindVerticalIntervalsObeyingSymbolSequence(
  const std::vector<AffineMap> &Maps, const SymbolSequence &S, DOUBLE &Initial,
  DOUBLE &Final) {
  assert(Maps.size() == S.size() && "Size mismatch!");
  if (Maps.empty()) {
    // The solution is the whole critical line!
    Initial = Final = DOUBLE::whole();
    return true;
  }

  // Compose the maps.  Try to improve precision by precomposing by
  // projection to the y axis.
  MATRIX Project = boost::numeric::ublas::zero_matrix<DOUBLE>(2);
  Project(1,1) = 1;
  AffineMap M(Project);
  for (unsigned i = 0, e = Maps.size(); i != e; ++i)
    M = Maps[i] * M;
#ifndef NDEBUG
  std::cerr << M << std::endl;
#endif

  ComputeVerticalIntervalsMappedToEachOther(M, Initial, Final);
  if (boost::numeric::empty(Initial) || boost::numeric::empty(Final))
    return false; // Shouldn't happen but handle it anyway.

  if (Maps.size() == 1)
    return true; // No need to check symbol sequences.

  VECTOR Image;
  Image(0) = 0;
  Image(1) = Initial;
  for (unsigned i = 0, e = Maps.size() - 1; i != e; ++i) {
    Image = Maps[i] * Image;
    // Check that some point may have the right symbol.
    if (S[i+1] == LEFT && Image(0).lower() > 0)
      return false;
    else if (S[i+1] == RIGHT && Image(0).upper() < 0)
      return false;
  }

  return true;
}
