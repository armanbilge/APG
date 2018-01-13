#include <stdlib.h>
#include <complex.h>
#include "apg_Q__.h"

#define MAX_DEG 12
#define APG_CALCULATE_DIMENSION(N) ((((N)+1) * ((N)+1) + (N) - 1) / 2)

double complex residues[MAX_DEG][MAX_DEG] = {
                                              {-0.6518555207051950+0.0000000000000000*I},
                                              {0.1691526336115412+0.8098011115445225*I,0.1691526336115412-0.8098011115445225*I},
                                              {0.6911221950418143-0.0431437283572168*I,-1.4837496454512236+0.0000000000000000*I,0.6911221950418143+0.0431437283572168*I},
                                              {0.0733924192341568-0.4500049158538197*I,-0.0616835225549505+1.9050594559799960*I,-0.0616835225549493-1.9050594559799954*I,0.0733924192341568+0.4500049158538197*I},
                                              {-0.2387881203872477-0.1037389202661832*I,1.8723828149827564+0.3782067899418312*I,-3.2718146221929332+0.0000000000000003*I,1.8723828149827570-0.3782067899418296*I,-0.2387881203872477+0.1037389202661833*I},
                                              {-0.0835816171565442+0.1064292607479337*I,0.6630068705296522-1.4514129199038115*I,-0.5790130040319096+4.2868885645853663*I,-0.5790130040319096-4.2868885645853636*I,0.6630068705296502+1.4514129199038115*I,-0.0835816171565442-0.1064292607479337*I},
                                              {0.0396804610539999+0.0524776158218305*I,-0.9026051607525094-0.7400361476512833*I,4.4566692114588156+1.5604498800553177*I,-7.1876256810160069+0.0000000000000028*I,4.4566692114588138-1.5604498800553313*I,-0.9026051607525096+0.7400361476512842*I,0.0396804610539999-0.0524776158218305*I},
                                              {0.0281297571644922-0.0115773845637671*I,-0.6325880537660820+0.4439231026209059*I,2.4362407330406533-3.7167556405488122*I,-1.8317717107179401+9.5256081293976962*I,-1.8317717107179377-9.5256081293976820*I,2.4362407330406555+3.7167556405488185*I,-0.6325880537660816-0.4439231026209053*I,0.0281297571644922+0.0115773845637670*I},
                                              {-0.0018222944370002-0.0134002772189243*I,0.1549621662308459+0.4475871796384582*I,-2.4638881224875169-2.7896859422796361*I,10.1971957415825418+4.5617679286460833*I,-15.7728982099406032-0.0000000000000016*I,10.1971957415825614-4.5617679286460646*I,-2.4638881224875222+2.7896859422796418*I,0.1549621662308464-0.4475871796384580*I,-0.0018222944370002+0.0134002772189243*I},
                                              {-0.0057849038663418-0.0006858506968526*I,0.2725869807964668-0.0142117269905465*I,-2.5655849631461405+1.2163857077662577*I,7.1171651325235938-8.8195331841506963*I,-4.8183820114563121+21.0545973121228727*I,-4.8183820114563334-21.0545973121228833*I,7.1171651325235938+8.8195331841507052*I,-2.5655849631461480-1.2163857077662641*I,0.2725869807964674+0.0142117269905461*I,-0.0057849038663418+0.0006858506968526*I},
                                              {-0.0008816719527048+0.0022803680938143*I,0.0340472600480015-0.1456521870852162*I,0.3085129346168129+1.9833751645528999*I,-5.9835669670360438-8.4644564889457943*I,22.9407070659032719+11.8498539171933466*I,-34.5976373096457053-0.0000000000000023*I,22.9407070659033039-11.8498539171932826*I,-5.9835669670360323+8.4644564889458032*I,0.3085129346168086-1.9833751645529027*I,0.0340472600480010+0.1456521870852159*I,-0.0008816719527048-0.0022803680938143*I},
                                              {0.0008184330748349+0.0005813540920431*I,-0.0685714834879943-0.0384191198580998*I,1.3194116644658251+0.1835240172994595*I,-8.2382575309715200+2.7961900510080597*I,18.7859816629539615-20.2372854423050903*I,-11.7993827414360464+46.4116397667583698*I,-11.7993827414359647-46.4116397667583627*I,18.7859816629539473+20.2372854423050939*I,-8.2382575309715307-2.7961900510080913*I,1.3194116644658231-0.1835240172994600*I,-0.0685714834879943+0.0384191198580997*I,0.0008184330748349-0.0005813540920431*I}
                                            };
double complex poles[MAX_DEG][MAX_DEG] = {
                                           {0.5772255475209103+0.0000000000000000*I},
                                           {0.5850515606551329-1.1858472517236744*I,0.5850515606551329+1.1858472517236744*I},
                                           {0.1981697296161092-2.4106677661885407*I,1.3688034212107369+0.0000000000000000*I,0.1981697296161092+2.4106677661885407*I},
                                           {-0.3678383143997997-3.6581332720633215*I,1.5484005705394606-1.1918258539276221*I,1.5484005705394606+1.1918258539276221*I,-0.3678383143997997+3.6581332720633215*I},
                                           {-1.0395069762767801-4.9213886032220104*I,1.4405947739927016-2.3969827787124571*I,2.1554142894325157+0.0000000000000000*I,1.4405947739927016+2.3969827787124571*I,-1.0395069762767801+4.9213886032220104*I},
                                           {-1.7819882759194345-6.1965124673456948*I,1.1585525717205898-3.6147726008190508*I,2.4006029389341874-1.1931293084019476*I,2.4006029389341874+1.1931293084019476*I,1.1585525717205898+3.6147726008190508*I,-1.7819882759194345+6.1965124673456948*I},
                                           {-2.5757261310748256-7.4809773676109472*I,0.7576085974311838-4.8436184803208580*I,2.4231963194494655-2.3930292987763089*I,2.9410968925807794+0.0000000000000000*I,2.4231963194494655+2.3930292987763089*I,0.7576085974311838+4.8436184803208580*I,-2.5757261310748256+7.4809773676109472*I},
                                           {-3.4085395014280828-8.7730345642027885*I,0.2694909873620118-6.0820325925855112*I,2.2922491477926989-3.6007714960130528*I,3.2209452450180547-1.1936196053982671*I,3.2209452450180547+1.1936196053982671*I,2.2922491477926989+3.6007714960130528*I,0.2694909873620118+6.0820325925855112*I,-3.4085395014280828+8.7730345642027885*I},
                                           {-4.2722773428735765-10.0714134048255755*I,-0.2857163620324885-7.3287579587923100*I,2.0477955224197255-4.8162322653866960*I,3.3196836145411144-2.3913415666988245*I,3.7264404414400425+0.0000000000000000*I,3.3196836145411144+2.3913415666988245*I,2.0477955224197255+4.8162322653866960*I,-0.2857163620324885+7.3287579587923100*I,-4.2722773428735765+10.0714134048255755*I},
                                           {-5.1611912709751371-11.3751562546998084*I,-0.8944047002925406-8.5827568985442397*I,1.7154060184000033-6.0389349244973722*I,3.2837528866866865-3.5943867716177431*I,4.0277324712543709-1.1938560662095921*I,4.0277324712543709+1.1938560662095921*I,3.2837528866866865+3.5943867716177431*I,1.7154060184000033+6.0389349244973722*I,-0.8944047002925406+8.5827568985442397*I,-5.1611912709751371+11.3751562546998084*I},
                                           {-6.0710610740996183-12.6835206201904978*I,-1.5468803930181365-9.8431734176342243*I,1.3125380433652947-7.2683189187691166*I,3.1429902452015810-4.8030732927009039*I,4.1765092800709169-2.3904652989651805*I,4.5116222515925406+0.0000000000000000*I,4.1765092800709169+2.3904652989651805*I,3.1429902452015810+4.8030732927009039*I,1.3125380433652947+7.2683189187691166*I,-1.5468803930181365+9.8431734176342243*I,-6.0710610740996183+12.6835206201904978*I},
                                           {-6.9986879889029705-13.9959159319744479*I,-2.2359681312388116-11.1092957074519330*I,0.8517072680705801-8.5038324815455280*I,2.9178687095502327-6.0173457169342512*I,4.2061243495806435-3.5909206484859526*I,4.8274935855605436-1.1939879564602069*I,4.8274935855605436+1.1939879564602069*I,4.2061243495806435+3.5909206484859526*I,2.9178687095502327+6.0173457169342512*I,0.8517072680705801+8.5038324815455280*I,-2.2359681312388116+11.1092957074519330*I,-6.9986879889029705+13.9959159319744479*I}
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

JNIEXPORT jdoubleArray JNICALL Java_apg_Q_00024_findOrthogonalVector
  (JNIEnv* env, jobject this, jint N, jdouble u, jdouble v, jdouble gamma) {

  int n, r, i;
  int dim = APG_CALCULATE_DIMENSION(N);

  jdoubleArray jx = (*env)->NewDoubleArray(env, dim);
  jdouble* restrict x = (*env)->GetPrimitiveArrayCritical(env, jx, NULL);

  double xn[N+1];
  double yn[N+1];
  double z = u + v;

  xn[0] = u;
  xn[1] = v;
  x[0] = u / z;
  x[1] = v / z;

  double* restrict xptr = x + 2;

  for (n = 2; n <= N; ++n) {

    yn[0] = - ((gamma * (n-1) * n) / 2.0) * xn[0];
    for (r = 1; r < n; ++r) {
      yn[r] = - ((gamma * (r-1) * n) / 2.0) * xn[r-1] - ((gamma * (n-1-r) * n) / 2.0) * xn[r];
    }
    yn[n] = - ((gamma * (n-1) * n) / 2.0) * xn[n-1];

    solve_central_block_transposed(xn, yn, 0, n, u, v, gamma);

    for (i = 0; i <= n; ++i) {
      *(xptr++) = xn[i] / z;
    }

  }

  (*env)->ReleasePrimitiveArrayCritical(env, jx, x, 0);

  return jx;

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

void solve(int N, double u, double v, double gamma, double complex * restrict y, double complex offset, double complex * restrict x) {

  int dim = APG_CALCULATE_DIMENSION(N) + 1;

  int i, n, r;
  double complex xn[N+1];
  double complex yn[N+1];

  double complex * restrict xptr = x + (dim - 1 - N);
  double complex * restrict yptr = y + (dim - 1 - N);

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

JNIEXPORT jdoubleArray JNICALL Java_apg_Q_00024_expQTtx
  (JNIEnv* env, jobject this, jint degree, jint steps, jint N, jdouble u, jdouble v, jdouble gamma, jdouble t, jdoubleArray jx) {

  int n = APG_CALCULATE_DIMENSION(N);
  double complex * restrict ci = residues[degree-1];
  double complex * restrict zi = poles[degree-1];

  int i, j, k;
  double complex c_i, offset;

  double complex xc[n+1];
  double complex wc[n+1];
  double complex vc[n+1];

  jdouble* restrict x = (*env)->GetPrimitiveArrayCritical(env, jx, NULL);
  for (i = 0; i < n; ++i) {
    wc[i+1] = x[i];
  }
  (*env)->ReleasePrimitiveArrayCritical(env, jx, x, JNI_ABORT);

  double stepsize = t / steps;

  for (k = 0; k < steps; ++k) {

    for (i = 1; i < n+1; ++i) {
      vc[i] = wc[i] / stepsize;
      wc[i] = 0;
    }

    for (i = 0; i < degree; ++i) {
      offset = - zi[i] / stepsize;
      solve(N, u, v, gamma, vc, offset, xc);
      c_i = ci[i];
      for (j = 1; j < n+1; ++j) {
        wc[j] += c_i * xc[j];
      }
    }

  }

  jdoubleArray jw = (*env)->NewDoubleArray(env, n);
  jdouble* restrict w = (*env)->GetPrimitiveArrayCritical(env, jw, NULL);
  for (i = 0; i < n; ++i) {
    w[i] = creal(wc[i+1]);
  }
  (*env)->ReleasePrimitiveArrayCritical(env, jw, w, 0);

  return jw;

}
