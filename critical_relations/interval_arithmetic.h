#ifndef INTERVAL_ARITHMETIC_H
#define INTERVAL_ARITHMETIC_H

#include <boost/numeric/interval.hpp>
#include <iostream>

typedef boost::numeric::interval<double> DOUBLE;

inline std::ostream& operator<<(std::ostream& os, const DOUBLE& r) {
  if (boost::numeric::empty(r))
    os << "<>";
  else
    os << "<" << r.lower() << ":" << r.upper() << ">";
  return os;
}

#endif
