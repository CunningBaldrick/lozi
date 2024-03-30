#ifndef MATRICES_H
#define MATRICES_H

#include "interval_arithmetic.h"
#include <boost/numeric/ublas/matrix.hpp>
#include <iostream>

typedef boost::numeric::ublas::c_matrix<DOUBLE, 2, 2> MATRIX;

inline std::ostream& operator<<(std::ostream& os, const MATRIX& m) {
  os << "[";
  for (size_t i = 0, e = m.size1(); i < e; ++i) {
    if (i)
      os << ",[";
    else
      os << "[";
    for (size_t j = 0, f = m.size2(); j < f; ++j) {
      if (j)
        os << ", ";
      os << m(i, j);
    }
    os << "]";
  }
  os << "]";
  return os;
}

#endif
