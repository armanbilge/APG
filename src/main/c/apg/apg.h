#ifndef APG_H
#define APG_H

#define MAX(x, y) (((x) > (y)) ? (x) : (y))

#define APG_CALCULATE_DIMENSION(N) (((N+1) * (N+1) + N - 1) / 2)

typedef struct apg_q {
  unsigned int N;
  double u;
  double v;
  double gamma;
} apg_q_t;

void apg_hypergeometric_initialize();

double apg_hypergeometric_pmf(unsigned int N, unsigned int K, unsigned int n, unsigned int k);

double* apg_f_from_partial(unsigned int n, double* partial);

double* apg_f_with_partial(unsigned int n, double* f, unsigned int k, double* partial);

double* apg_find_orthogonal_vector(apg_q_t* q);

double* apg_cf_expmv(double time, apg_q_t* q, double v[]);

double apg_dot_product(unsigned int n, double* x, double* y);

#endif
