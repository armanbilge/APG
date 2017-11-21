#include <stdlib.h>
#include "apg_QMath__.h"
#include "apg.h"

JNIEXPORT jdoubleArray JNICALL Java_apg_QMath_00024_findOrthogonalVector
  (JNIEnv* env, jobject this, jint N, jdouble u, jdouble v, jdouble gamma) {

  unsigned int size = apg_calculate_dimension(N) + 1;
  apg_q_t q = {N, u, v, gamma};
  double* x = apg_find_orthogonal_vector(&q);
  jdoubleArray y = (*env)->NewDoubleArray(env, size);
  (*env)->SetDoubleArrayRegion(env, y, 0, size, x);
  free(x);
  return y;

}

JNIEXPORT jdoubleArray JNICALL Java_apg_QMath_00024_expQTtx
  (JNIEnv* env, jobject this, jint N, jdouble u, jdouble v, jdouble gamma, jdouble t, jdoubleArray x) {

  unsigned int size = apg_calculate_dimension(N) + 1;
  apg_q_t q = {N, u, v, gamma};
  jdouble* xp = (*env)->GetDoubleArrayElements(env, x, NULL);
  double* y = apg_cf_expmv(t, &q, xp);
  (*env)->ReleaseDoubleArrayElements(env, x, xp, 0);
  jdoubleArray z = (*env)->NewDoubleArray(env, size - 1);
  (*env)->SetDoubleArrayRegion(env, z, 0, size - 1, y + 1);
  free(y);
  return z;

}
