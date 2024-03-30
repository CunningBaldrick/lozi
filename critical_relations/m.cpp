#include "affine_maps.h"
#include "critical.h"
#include "matrices.h"
#include "parameters.h"
#include "symbol_sequences.h"
#include "vectors.h"
#include <boost/numeric/ublas/io.hpp>
#include <cassert>
#include <climits>
#include <vector>

using namespace boost::numeric::ublas;

typedef std::pair<DOUBLE, DOUBLE> Rectangle;

static void AddParameterRectangle(std::vector<Rectangle> &List, DOUBLE A,
                                  DOUBLE B) {
#ifndef NDEBUG
  std::cerr << "Adding " << A << " " << B << std::endl;
#endif
  List.push_back(std::make_pair(A, B));
}

// Empty - reinitialize this symbol sequence to the "empty" sequence (since
// the first symbol is ignored, for convenience we set it to LEFT).
static void Empty(SymbolSequence &S) {
  S.clear();
  S.push_back(LEFT);
}

struct CriticalSection {
  SymbolSequence Symbols;
  DOUBLE Initial;
  DOUBLE Final;
};

// Returns true if there exists a path starting from i that visits exactly once
// every node not already marked visited in Visted.
static bool path_exists(std::vector<bool> &TransitionMatrix, unsigned ORDER,
                        unsigned i, unsigned Visited) {
  // Mark the current node as visited.
  Visited |= 1U << i;
  // Check if all nodes have been visited.
  if ((Visited | (~0U << ORDER)) == ~0U)
    return true;
  for (unsigned j = 0; j != ORDER; ++j)
    // If j has not been visited, and there exists an edge from i to j,
    // recursively consider paths from j.
    if (!(Visited & (1U << j)) && TransitionMatrix[i*ORDER+j])
      if (path_exists(TransitionMatrix, ORDER, j, Visited))
        return true;
  return false;
}

int main(int argc, char * argv[]) {
  if (argc < 2) {
    std::cerr << "usage: max_symbol_sequence_length [num_segments]" << std::endl;
    exit(1);
  }
  --argc; ++argv;

  const unsigned MAX_N = atoi(argv[0]);
  --argc; ++argv;

  const unsigned ORDER = argc > 0 ? atoi(argv[0]) : 2;
  if (ORDER < 1) {
    std::cerr << "the critical order should be at least 1, but got " << ORDER << std::endl;
    exit(1);
  }
  const unsigned MAX_ORDER = sizeof(unsigned) * CHAR_BIT;
  if (ORDER > MAX_ORDER) {
    std::cerr << "the critical order should be at most " << MAX_ORDER
      << ", but got " << ORDER << std::endl;
    exit(1);
  }

  // If there are no possible critical relations of type *...*...* (with ORDER
  // being the number of *...* segments), then we are done!  Checking this here
  // makes life a bit simpler later, by reducing the number of corner cases.
  if (ORDER + 1 > MAX_N)
    return 0;

  const double base_a_range = 0.005;
  const double base_b_range = 0.005;

  // Add initial parameter rectangles.
  std::vector<Rectangle> Worklist;
  AddParameterRectangle(Worklist, DOUBLE(-2, 3.13), DOUBLE(-1, 1));

  // The symbol sequences (and associated data), initialized to 'L' (the first
  // symbol is ignored, so these are really "empty" symbol sequences).
  std::vector<CriticalSection> symbols(ORDER);
  for (unsigned i = 0; i != ORDER; ++i)
    Empty(symbols[i].Symbols);

  // Storage for the transition matrix computed later.
  std::vector<bool> TransitionMatrix(ORDER*ORDER);

  // Loop over all symbol sequences in order of length.
  do {
    // Print the current symbol sequences under consideration.
    std::cerr << '*';
    for (unsigned i = 0; i != ORDER; ++i) {
      write(std::cerr, 1+symbols[i].Symbols.begin(), symbols[i].Symbols.end());
      std::cerr << '*';
    }
    std::cerr << std::endl;

    // Record the size of the current total symbol sequence for printing later.
    size_t current_size = 1;
    for (unsigned i = 0; i != ORDER; ++i)
      current_size += symbols[i].Symbols.size();

    std::vector<Rectangle> LocalWorklist;

    // Initialize the local worklist with all pending rectangles (and clear out
    // the pending list; rectangles we be placed back into the pending list if
    // we can prove that they have no critical relationship of the type given by
    // the current set of symbol sequences).
    Worklist.swap(LocalWorklist);

    while (!LocalWorklist.empty()) {
       // Examine the next parameter rectangle.
       Rectangle R = LocalWorklist.back();
       LocalWorklist.pop_back();
       DOUBLE &A = R.first;
       DOUBLE &B = R.second;
#ifndef NDEBUG
       std::cerr << "Considering " << A << " " << B << std::endl;
#endif

       // We now consider the graph where the nodes are the current symbol
       // sequences, and there is an edge from a node S1 to a different node
       // S2 if and only if there may be a point satisfying *S1*S2*.

       // Calculate the possible set of points satisfying each symbol sequence.
       bool critical_relation_possible = true;
       for (unsigned i = 0; i != ORDER; ++i) {
         const SymbolSequence &S = symbols[i].Symbols;
         DOUBLE &I = symbols[i].Initial;
         DOUBLE &F = symbols[i].Final;

         Parameter P(A, B);
         std::vector<AffineMap> Maps = P.getMaps(S);
         if (!FindVerticalIntervalsObeyingSymbolSequence(Maps, S, I, F)) {
           critical_relation_possible = false;
           break;
         }
       }

       if (!critical_relation_possible) {
         // No critical relation involving this symbol sequence exists.  Put
         // the parameters back on the main worklist, so they will be examined
         // for the next symbol sequence, but no longer for this one.
         Worklist.push_back(R);
         continue;
       }

       // Compute the transition matrix of the graph described above.
       for (unsigned i = 0; i != ORDER; ++i) {
         TransitionMatrix[i*ORDER + i] = false; // No self edges.
         for (unsigned j = 0; j != ORDER; ++j)
           if (j != i) {
             // Determine if the final interval for the first symbol sequence
             // intersects the initial interval for the second symbol sequence.
             const DOUBLE &F1 = symbols[i].Final;
             const DOUBLE &I2 = symbols[j].Initial;
             TransitionMatrix[i*ORDER + j] = // set to true if they intersect
               (I2.upper() >= F1.lower() && I2.lower() <= F1.upper());
           }
       }

       // If there is a path through the graph that visits each node exactly
       // once, then there may be a critical relation of the current type.
       bool found_critical_relation = false;
       assert(MAX_ORDER <= sizeof(unsigned) * CHAR_BIT);
       for (unsigned i = 0; i != ORDER; ++i) {
         // Mark all nodes as unvisited.
         unsigned Visited = 0;
         if ((found_critical_relation =
              path_exists(TransitionMatrix, ORDER, i, Visited)))
          break;
       }

       if (found_critical_relation) {
         // Found an apparent critical relationship.  However it might go
         // away if we consider smaller parameter rectangles, or at least
         // it might be located more precisely by considering such smaller
         // rectangles.  So subdivide the current parameter rectangle into
         // pieces for further inspection, unless it is already very tiny.
         double a_width = boost::numeric::width(A);
         double b_width = boost::numeric::width(B);
         bool split_a = a_width > base_a_range / pow(1.5, current_size);
         bool split_b = b_width > base_b_range / pow(1.5, current_size);
         if (split_a) {
           std::pair<DOUBLE, DOUBLE> AA = boost::numeric::bisect(A);
           if (split_b) {
             // Divide horizontally and vertically, adding the four new
             // rectangles to the local worklist for further scrutiny.
#ifndef NDEBUG
             std::cerr << "Four way split" << std::endl;
#endif
             std::pair<DOUBLE, DOUBLE> BB = boost::numeric::bisect(B);
             AddParameterRectangle(LocalWorklist, AA.first, BB.first);
             AddParameterRectangle(LocalWorklist, AA.first, BB.second);
             AddParameterRectangle(LocalWorklist, AA.second, BB.first);
             AddParameterRectangle(LocalWorklist, AA.second, BB.second);
           } else {
             // Divide horizontally, adding the two rectangles to the local
             // worklist.
#ifndef NDEBUG
             std::cerr << "Horizontal split" << std::endl;
#endif
             AddParameterRectangle(LocalWorklist, AA.first, B);
             AddParameterRectangle(LocalWorklist, AA.second, B);
           }
         } else if (split_b) {
           // Divide vertically, adding the two rectangles to the local
           // worklist.
#ifndef NDEBUG
           std::cerr << "Vertical split" << std::endl;
#endif
           std::pair<DOUBLE, DOUBLE> BB = boost::numeric::bisect(B);
           AddParameterRectangle(LocalWorklist, A, BB.first);
           AddParameterRectangle(LocalWorklist, A, BB.second);
         } else {
           // We have a very tiny parameter rectangle with an apparent
           // critical relationship.  Maybe it really has one!
           std::cout << A.lower() << " ";
           std::cout << B.lower() << " ";
           std::cout << a_width << " ";
           std::cout << b_width << " ";
           std::cout << current_size << " " << std::endl;
         }
       } else {
         // No critical relation for this symbol sequence.  Put it back on
         // the main worklist, so it will be examined for the next symbol
         // sequence, but no longer for this one.
         Worklist.push_back(R);
       }
    }

    // Move to the next values for the symbol sequences.  We only consider
    // sequences that are ordered by length (and when consecutive symbol
    // sequences have the same length, they should be ordered, not strictly,
    // by symbol).

    // First loop over the sequences, increasing them symbolically, without
    // changing their lengths.
    bool ordered;
    do {
      for (unsigned i = 0; i != ORDER; ++i) {
        // Increment this symbol.
        SymbolSequence &Symbols = symbols[i].Symbols;
        if (!Increment(1+Symbols.begin(), Symbols.end()))
          goto check_order; // It incremented without wrapping around, use it.
        // It wrapped around, increment the next symbol.
      }

      // All of the symbols wrapped around when incremented, increment the
      // lengths.
      for (unsigned base = 0; base != ORDER; ++base) {
        // All symbols with index in [0, base) are empty.
#ifndef NDEBUG
        for (unsigned i = 0; i != base; ++i) {
          assert(symbols[i].Symbols.size() == 1 && "Sequence is not empty!");
          assert(symbols[i].Symbols[0] == LEFT && "Sequence emptied wrong!");
        }
#endif

        for (unsigned i = base; i != ORDER; ++i) {
          SymbolSequence &Symbols = symbols[i].Symbols;
          // Increment the length of this symbol.  However, if the symbol would get
          // longer than the next one, shrink this symbol to "empty" and increment
          // the length of the next one instead.
          if (i + 1 < ORDER && Symbols.size() == symbols[i+1].Symbols.size()) {
            // Empty out this symbol, and make the next one longer.
            Empty(Symbols);
            continue;
          }
          // Make the symbol sequence longer by one.
          Symbols.push_back(LEFT);
          break;
        }

        // Check whether the total length of the symbols became too long.
        size_t total_size = base;
        for (unsigned j = base; j != ORDER; ++j)
          total_size += symbols[j].Symbols.size();
        if (total_size + 1 <= MAX_N)
          goto check_order; // Not too long.
        if (base + 1 == ORDER)
          return 0; // No symbols left, all done!  Yup, this is where we exit :)

        // Empty out this symbol and increment the next one.
        Empty(symbols[base].Symbols);
      }

  check_order:
      // As an optimization, if successive symbol sequences have the same length
      // then we require them to be ordered (<=), avoiding pointless work.
      ordered = true;
      for (unsigned i = 1; i < ORDER; ++i)
        if (symbols[i-1].Symbols.size() == symbols[i].Symbols.size() &&
            symbols[i-1].Symbols > symbols[i].Symbols) {
          ordered = false;
          break;
        }
    } while (!ordered);
  } while (true);
}
