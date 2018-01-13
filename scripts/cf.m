function [zk,ck,r_inf] = cf(n);
  K = 75;
  nf = 1024;
  w = exp(2i*pi*(0:nf-1)/nf);
  t = real(w);
  scl = 9;
  F = exp(scl*(t-1)./(t+1+1e-16));
  c = real(fft(F))/nf;
  f = polyval(c(K+1:-1:1),w);
  [U,S,V] = svd(hankel(c(2:K+1)));
  s = S(n+1,n+1);
  u = U(K:-1:1,n+1)'; v = V(:,n+1)';
  zz = zeros(1,nf-K);
  b = fft([u zz])./fft([v zz]);
  rt = f-s*w.^K.*b;
  rtc = real(fft(rt))/nf;
  zr = roots(v); qk = zr(abs(zr)>1);
  qc = poly(qk);
  pt = rt.*polyval(qc,w);
  ptc = real(fft(pt)/nf);
  ptc = ptc(n+1:-1:1); ck = 0*qk;
  for k = 1:n
    q = qk(k); q2 = poly(qk(qk~=q));
    ck(k) = polyval(ptc,q)/polyval(q2,q);
  end
  zk = scl*(qk-1).^2./(qk+1).^2;
  ck = 4*ck.*zk./(qk.^2-1);
  [~,order] = sort(imag(zk));
  zk = zk(order);
  ck = ck(order);
  r_inf = real(0.5*(1+sum(ck./zk)) + 0i);
end

Z = zeros(12);
C = zeros(12);
for n = 1:12
  [z,c,~] = cf(n);
  Z(n,1:n) = z;
  C(n,1:n) = c;
end

function print_cstyle(A)
  printf("{\n")
  for i = 1:12
    printf("  {")
    for j = 1:i
      printf("%.16f", real(A(i,j)))
      if imag(A(i,j)) >= 0
        printf("+")
      end
      printf("%.16f*I",imag(A(i,j)))
      if j ~= i
        printf(",")
      end
    end
    printf("}")
    if i ~= 12
      printf(",\n")
    end
  end
  printf("\n}\n")
end

print_cstyle(C)
print_cstyle(Z)
