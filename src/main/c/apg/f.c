#include <stdlib.h>
#include "apg.h"

#define F_GET(f, n, r) ((f) + (n) * ((n)+1) / 2 - 1 + (r))

double* apg_f_from_partial(int n, double* partial) {
  double* f = (double*) malloc(sizeof(double) * APG_CALCULATE_DIMENSION(n));
  double* fptr;
  double* fn = F_GET(f, n, 0);
  for (fptr = f; fptr < fn; ++fptr) *fptr = 0;
  int r;
  for (r = 0; r <= n; ++r, ++fptr) {
    *fptr = *(partial++);
  }
  return f;
}

double* apg_f_with_partial(int n, double* restrict f, int k, double* restrict partial) {
  int i, r, np = APG_CALCULATE_DIMENSION(n+k);
  double* fp = (double*) malloc(sizeof(double) * np);
  double fnr, *fptr;
  for (i = 0, fptr = fp; i < np; ++i, ++fptr) *fptr = 0;
  for (i = 0; i <= k; ++i, ++partial) {
    for (np = 1; np <= n; ++np) {
      for (r = 0; r <= n; ++r) {
        fnr = *F_GET(f, np, r);
        *F_GET(fp, np+k, r+i) += MAX(fnr, 0) * *partial * apg_hypergeometric_pmf(np+k, r+i, k, i);
      }
    }
  }
  return fp;
}

double apg_dot_product(int n, double* restrict x, double* restrict y) {
  double sum = 0;
  int i;
  for (i = 0; i < n; ++i) {
    sum += *(x++) * *(y++);
  }
  return sum;
}
