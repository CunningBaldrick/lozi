#ifndef PARAMETERS_H
#define PARAMETERS_H

#include "affine_maps.h"
#include "interval_arithmetic.h"
#include "matrices.h"
#include "symbol_sequences.h"
#include <iostream>

class Parameter {
   DOUBLE a;
   DOUBLE b;
public:
   Parameter(DOUBLE A, DOUBLE B) : a(A), b(B) {}

   DOUBLE getA() const {
     return this->a;
   }

   DOUBLE getB() const {
     return this->b;
   }

   AffineMap getMap(Symbol s) const {
     // (x,y) -> (1-a|x|+y,bx)
     MATRIX L;
     if (s == LEFT)
       L(0, 0) = a;
     else
       L(0, 0) = -a;
     L(0, 1) = 1;
     L(1, 0) = b;
     L(1, 1) = 0;

     VECTOR O;
     O(0) = 1;
     O(1) = 0;

     AffineMap A(L, O);
     return A;
   }

   std::vector<AffineMap> getMaps(const SymbolSequence &S) {
     AffineMap LeftMap = getMap(LEFT);
     AffineMap RightMap = getMap(RIGHT);
     size_t s = S.size();
     std::vector<AffineMap> M(s);
     for (size_t i = 0; i < s; ++i)
       if (S[i] == LEFT)
         M[i] = LeftMap;
       else
         M[i] = RightMap;
     return M;
   }
};

inline std::ostream& operator<<(std::ostream& os, const Parameter& p) {
  os << "a=" << p.getA() << ",b=" << p.getB();
  return os;
}

#endif
