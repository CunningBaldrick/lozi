#ifndef AFFINE_MAPS_H
#define AFFINE_MAPS_H

#include "matrices.h"
#include "vectors.h"
#include <iostream>

class AffineMap {
   MATRIX A;
   VECTOR B;
public:
   AffineMap() { // Identity map.
     A = boost::numeric::ublas::identity_matrix<DOUBLE>(2);
     B = boost::numeric::ublas::zero_vector<DOUBLE>(2);
   }

   AffineMap(MATRIX a) : A(a) { // Linear map.
     B = boost::numeric::ublas::zero_vector<DOUBLE>(2);
   }
   AffineMap(VECTOR b) : B(b) { // Translation.
     A = boost::numeric::ublas::identity_matrix<DOUBLE>(2);
   }
   AffineMap(MATRIX a, VECTOR b) : A(a), B(b) {}

   MATRIX getLinearPart() const {
     return this->A;
   }

   VECTOR getOffsetPart() const {
     return this->B;
   }

   VECTOR operator*(const VECTOR &x) const { // evaluation
     return prod(this->A, x) + this->B;
   }

   AffineMap operator*(const AffineMap &that) const { // composition
     AffineMap R(prod(this->A, that.A), prod(this->A, that.B) + this->B);
     return R;
   }
};

AffineMap Compose(const std::vector<AffineMap> &Maps);

inline std::ostream& operator<<(std::ostream& os, const AffineMap& m) {
  os << m.getLinearPart() << "()+" << m.getOffsetPart();
  return os;
}

#endif
