package apg;

import cz.adamh.utils.NativeUtils;

import java.io.IOException;

public class NativeLib {

    private NativeLib() {}

    static {
        try {
            NativeUtils.loadLibraryFromJar("/apg.jni");
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
        hypergeometricInitialize();
    }

    private static native void hypergeometricInitialize();

    public static NativePointer fFromPartial(int n, NativePointer partial) {
        return new NativePointer(fFromPartial(n, partial.value()));
    }

    private static native long fFromPartial(int n, long partial);

    public static NativePointer fWithPartial(int n, NativePointer f, int k, NativePointer partial) {
        return new NativePointer(fWithPartial(n, f.value(), k, partial.value()));
    }

    private static native long fWithPartial(int n, long f, int k, long partial);

    public static NativePointer findOrthogonalVector(int N, double u, double v, double gamma) {
        return new NativePointer(findOrthogonalVectorImpl(N, u, v, gamma));
    }

    private static native long findOrthogonalVectorImpl(int N, double u, double v, double gamma);

    public static NativePointer expQTtx(int N, double u, double v, double gamma, double t, NativePointer x) {
        return new NativePointer(expQTtx(N, u, v, gamma, t, x.value()));
    }

    private static native long expQTtx(int N, double u, double v, double gamma, double t, long x);

    public static double siteLikelihood(int N, NativePointer f, NativePointer x) {
        return siteLikelihood(N, f.value(), x.value());
    }

    private static native double siteLikelihood(int N, long f, long x);

    public static NativePointer copyToNative(double[] a) {
        return new NativePointer(copyToNativeImpl(a));
    }

    public static native long copyToNativeImpl(double[] a);

    public static native void free(long ptr);

}
