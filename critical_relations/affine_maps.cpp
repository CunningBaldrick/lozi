#include "affine_maps.h"

AffineMap Compose(const std::vector<AffineMap> &Maps) {
  AffineMap R;
  size_t s = Maps.size();
  if (!s)
    return R; // identity
  R = Maps[0];
  for (unsigned i = 1; i != s; ++i)
    R = Maps[i] * R;
  return R;
}
