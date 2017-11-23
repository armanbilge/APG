#include <stdlib.h>
#include "apg_NativeLib.h"
#include "apg.h"

JNIEXPORT void JNICALL Java_apg_NativeLib_hypergeometricInitialize(JNIEnv *env, jclass clazz) {
  apg_hypergeometric_initialize();
}

JNIEXPORT jlong JNICALL Java_apg_NativeLib_fFromPartial(JNIEnv *env, jclass clazz, jint n, jlong partial) {
  return (jlong) apg_f_from_partial(n, (double*) partial);
}

JNIEXPORT jlong JNICALL Java_apg_NativeLib_fWithPartial(JNIEnv *env, jclass clazz, jint n, jlong f, jint k, jlong partial) {
  return (jlong) apg_f_with_partial(n, (double*) f, k, (double*) partial);
}

JNIEXPORT jlong JNICALL Java_apg_NativeLib_findOrthogonalVectorImpl(JNIEnv *env, jclass clazz, jint N, jdouble u, jdouble v, jdouble gamma) {
  apg_q_t q = {N, u, v, gamma};
  return (jlong) apg_find_orthogonal_vector(&q);
}

JNIEXPORT jlong JNICALL Java_apg_NativeLib_expQTtx(JNIEnv *env, jclass clazz, jint N, jdouble u, jdouble v, jdouble gamma, jdouble t, jlong x) {
  apg_q_t q = {N, u, v, gamma};
  return (jlong) apg_cf_expmv(t, &q, (double*) x);
}

JNIEXPORT jdouble JNICALL Java_apg_NativeLib_siteLikelihood(JNIEnv* env, jclass clazz, jint N, jlong f, jlong x) {
  unsigned int n = APG_CALCULATE_DIMENSION(N);
  unsigned int i;
  return apg_dot_product(n, (double*) f, (double*) x);
}

JNIEXPORT jlong JNICALL Java_apg_NativeLib_copyToNativeImpl(JNIEnv* env, jclass clazz, jdoubleArray a) {
  jsize len = (*env)->GetArrayLength(env, a);
  double* b = malloc(sizeof(double) * len);
  (*env)->GetDoubleArrayRegion(env, a, 0, len, b);
  return (jlong) b;
}

JNIEXPORT void JNICALL Java_apg_NativeLib_free(JNIEnv* env, jclass clazz, jlong ptr) {
  free((void*) ptr);
}
