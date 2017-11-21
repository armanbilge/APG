#ifndef APG_H
#define APG_H

typedef struct apg_q
{
  unsigned int N;
  double u;
  double v;
  double gamma;
} apg_q_t;

#endif

unsigned int apg_calculate_dimension(unsigned int N);

double* apg_find_orthogonal_vector(apg_q_t* q);

double* apg_cf_expmv(double time, apg_q_t* q, double v[]);
