#ifndef VECTORS_H
#define VECTORS_H

#include "interval_arithmetic.h"
#include <boost/numeric/ublas/vector.hpp>
#include <iostream>

typedef boost::numeric::ublas::c_vector<DOUBLE, 2> VECTOR;

inline std::ostream& operator<<(std::ostream& os, const VECTOR& v) {
  os << "[";
  for (size_t i = 0, e = v.size(); i < e; ++i) {
    if (i)
      os << ",";
    os << v(i);
  }
  os << "]";
  return os;
}

#endif
