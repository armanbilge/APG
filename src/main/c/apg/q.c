#include <stdlib.h>
#include <complex.h>
#include "apg.h"

#define CF_DEG 12
const double complex ci[CF_DEG] =
  {
    0.000818433612497 + 0.000581353207069 * I,
    -0.068571505514864 + -0.038419074245887 * I,
    1.319411815998137 + 0.18352349775048 * I,
    -8.238258033274786 + 2.796192505614474 * I,
    18.78598262947607 + -20.237292093573895 * I,
    -11.799383335697918 + 46.4116507772796 * I,
    -11.79938333569789 + -46.41165077727957 * I,
    18.785982629476067 + 20.237292093573895 * I,
    -8.238258033274763 + -2.796192505614448 * I,
    1.319411815998138 + -0.18352349775048 * I,
    -0.068571505514865 + 0.038419074245888 * I,
    0.000818433612497 + -0.000581353207069 * I
  };
const double complex zi[CF_DEG] =
  {
    -6.998688082445778 + -13.995917029301355 * I,
    -2.235968223749446 + -11.10929640046187 * I,
    0.851707264834878 + -8.503832905826961 * I,
    2.91786880030717 + -6.017345968518187 * I,
    4.206124506834328 + -3.59092078313014 * I,
    4.827493775040721 + -1.193987999180278 * I,
    4.827493775040721 + 1.193987999180278 * I,
    4.206124506834328 + 3.59092078313014 * I,
    2.91786880030717 + 6.017345968518187 * I,
    0.851707264834878 + 8.503832905826961 * I,
    -2.235968223749446 + 11.10929640046187 * I,
    -6.998688082445778 + 13.995917029301355 * I
  };

void solve_central_block_transposed(double* restrict x, double* restrict y, double offset, int n, double u, double v, double gamma) {

  double K = -(gamma * (n * (n-1))) / 2.0 - n * v + offset;

  int r;
  double m;
  double d[n+1];
  double e[n+1];
  d[0] = K;
  e[0] = y[0];

  for (r = 1; r <= n; ++r) {
    m = ((n-r+1) * v) / d[r-1];
    d[r] = K + r * (v-u) - m * r * u;
    e[r] = y[r] - m * e[r-1];
  }

  x[n] = e[n] / d[n];

  for (r = n-1; r >= 0; --r) {
    x[r] = (e[r] - (r+1) * u * x[r+1]) / d[r];
  }

}

double* apg_find_orthogonal_vector(apg_q_t* q) {

  int N = q->N;
  double u = q->u;
  double v = q->v;
  double gamma = q->gamma;

  int n, r, i;
  int dim = APG_CALCULATE_DIMENSION(N);

  double* x = malloc(sizeof(double) * dim);
  double xn[N+1];
  double yn[N+1];

  xn[0] = u;
  xn[1] = v;
  x[0] = u;
  x[1] = v;

  double* xptr = x + 2;

  for (n = 2; n <= N; ++n) {

    yn[0] = - ((gamma * (n-1) * n) / 2.0) * xn[0];
    for (r = 1; r < n; ++r) {
      yn[r] = - ((gamma * (r-1) * n) / 2.0) * xn[r-1] - ((gamma * (n-1-r) * n) / 2.0) * xn[r];
    }
    yn[n] = - ((gamma * (n-1) * n) / 2.0) * xn[n-1];

    solve_central_block_transposed(xn, yn, 0, n, u, v, gamma);

    for (i = 0; i <= n; ++i) {
      *(xptr++) = xn[i];
    }

  }

  double z = x[0] + x[1];
  for (n = 0, xptr = x; n <= dim; ++n, ++xptr) {
    *xptr /= z;
  }

  return x;

}

void solve_central_block(double complex * restrict y, double complex offset, int n, double u, double v, double gamma, double complex * restrict x) {

  double complex K = - (gamma * (n * (n-1))) / 2.0 - n * v + offset;

  int r;
  double complex m;
  double complex d[n+1];
  double complex e[n+1];
  d[0] = K;
  e[0] = y[0];

  for (r = 1; r <= n; ++r) {
    m = (r * u) / d[r-1];
    d[r] = K + r * (v-u) - m * ((n-r+1) * v);
    e[r] = y[r] - m * e[r-1];
  }

  x[n] = e[n] / d[n];

  for (r = n-1; r >= 0; --r) {
    x[r] = (e[r] - (n-r) * v * x[r+1]) / d[r];
  }

}

void multiply_upper_block(double complex * restrict x, int n, double gamma, double complex * restrict y) {
  int r;
  for (r = 0; r <= n; ++r) {
    y[r] = (gamma * (n-r) * (n+1) / 2.0) * x[r] + (gamma * r * (n+1) / 2.0) * x[r+1];
  }
}

void solve(apg_q_t* q, double complex * restrict y, double complex offset, double complex * restrict x) {

  int N = q->N;
  double u = q->u;
  double v = q->v;
  double gamma = q->gamma;
  int dim = APG_CALCULATE_DIMENSION(N) + 1;

  int i, n, r;
  double complex xn[N+1];
  double complex yn[N+1];

  double complex* xptr = x + (dim - 1 - N);
  double complex* yptr = y + (dim - 1 - N);

  for (i = 0; i <= N; ++i) {
    yn[i] = yptr[i];
  }

  solve_central_block(yn, offset, N, u, v, gamma, xn);

  for (i = 0; i <= N; ++i) {
    xptr[i] = xn[i];
  }

  for (n = N-1; n >= 1; --n) {

    multiply_upper_block(xn, n, gamma, yn);

    xptr -= n+1;
    yptr -= n+1;
    for (r = 0; r <= n; ++r) {
      yn[r] = yptr[r] - yn[r];
    }

    solve_central_block(yn, offset, n, u, v, gamma, xn);

    for (i = 0; i <= n; ++i) {
      xptr[i] = xn[i];
    }
  }

}

double* apg_cf_expmv(double time, apg_q_t* q, double v[]) {

  int N = q->N;
  int n = APG_CALCULATE_DIMENSION(N);
  double* w = malloc(sizeof(double) * n);

  int i, j, k;
  double complex c_i, offset;

  if (time == 0.0) {
    for (i = 0; i < n; ++i) {
      w[i] = v[i];
    }
    return w;
  }

  complex double xc[n+1];
  complex double wc[n+1];
  complex double vc[n+1];

  for (i = 0; i < n; ++i) {
    wc[i+1] = v[i];
  }

  int steps = 1;
  double stepsize = time / steps;

  for (k = 0; k < steps; ++k) {

    for (i = 1; i < n+1; ++i) {
      vc[i] = wc[i] / stepsize;
      wc[i] = 0;
    }

    for (i = 0; i < CF_DEG; ++i) {
      offset = - zi[i] / stepsize;
      solve(q, vc, offset, xc);
      c_i = ci[i];
      for (j = 1; j < n+1; ++j) {
        wc[j] += c_i * xc[j];
      }
    }

  }

  for (i = 0; i < n; ++i) {
    w[i] = creal(wc[i+1]);
  }

  return w;

}
