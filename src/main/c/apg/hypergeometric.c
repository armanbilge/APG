#include <gsl/gsl_randist.h>
#include "apg.h"

#define DIM 100
double cache[DIM+1][DIM+1][DIM+1][DIM+1];

void apg_hypergeometric_initialize() {
  int N, K, n, k;
  for (N = 0; N <= DIM; ++N) {
    for (K = 0; K <= DIM; ++K) {
      for (n = 0; n <= DIM; ++n) {
        for (k = 0; k <= n; ++k) {
          cache[N][K][n][k] = gsl_ran_hypergeometric_pdf(k, K, N-K, n);
        }
      }
    }
  }
}

double apg_hypergeometric_pmf(int N, int K, int n, int k) {
  return cache[N][K][n][k];
}
