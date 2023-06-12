* multivariate analysis of linear change btw two groups;
proc iml;
C1={-1 1 0 0 ,
    0 -1 1 0 ,
	0 0 -1 1 };

E={135.3863636	67.92045455	97.75568182	67.75568182,
67.92045455	104.6193182	73.17897727	82.92897727,
97.75568182	73.17897727	161.3934659	103.2684659,
67.75568182	82.92897727	103.2684659	124.6434659};
S=E/(16+11-2);

CSC=C1*S*t(C1);

a=.05;
n1=22; n2=22;
beta_d=.7;
p=nrow(CSC)+1;

j=J(p-1,1,1);
lambda=n1*n2/(n1+n2)*beta_d**2*(t(j)*inv(CSC)*j);

f0=finv(1-a,p-1,n1+n2-p);
power=1-probf(f0,p-1,n1+n2-p,lambda);
print power;

quit;

*multivariate analysis of quadratic change in one group;

proc iml;
C1={-1 1 0 0 ,
    0 -1 1 0 ,
	0 0 -1 1 };
C2={-1 1 0,
    0 -1 1};
C=C2*C1;

E={
1289.664	1205.548	1027.072	1044.246,
1205.548	1247.069333	1021.377333	1004.818667,
1027.072	1021.377333	1006.469333	890.2846667,
1044.246	1004.818667	890.2846667	1056.217333};
S=E/14;

CSC=C*S*t(C);

n=9;
beta2=-1.96;
a=.05;
p=nrow(CSC)+2;

j=J(p-2,1,1);
lambda=4*n*beta2**2*(t(j)*inv(CSC)*j);

f0=finv(1-a,p-2,n-p+2);
power=1-probf(f0,p-2,n-p+2,lambda);
print power;

quit;

/* HLM random coefficients model:
comparing linear change btw 2 groups
*/

proc iml;
 m=3;
 t0=j(m+1,1,1);
 t1=t(0:m);

 z1=t1-t(t1)*t0/t0[##,]*t0;

 a=.05;
 n=112; 
 beta11=.187;
 sigma2=.42;
 p1=.04/.42;

 tau1=sigma2*p1;
 lambda=sqrt(n/4)*beta11/sqrt(tau1+sigma2/z1[##,]);
 power=1-probt(tinv(1-a/2,n-2),n-2,lambda)+
        probt(tinv(a/2,n-2),n-2,lambda);
 print power;
quit;

/* HLM random coefficients model:
quadratic change in one group
*/
proc iml;
 m=6;
 t0=j(m+1,1,1);
 t1=t(0:m);
 t2=t1##2/2;
*print t0 t1 t2;
 z1=t1-t(t1)*t0/t0[##,]*t0;
 z2=t2-t(t2)*t0/t0[##,]*t0-t(t2)*z1/z1[##,]*z1;
 *print z1 z2;
 
 a=.05;
 n=120;
 beta20=.1;
 sigma=1;
 p2=.1;

 tau2=sigma**2*p2;
 lambda=sqrt(n)*beta20/sqrt(tau2+sigma**2/z2[##,]);
 power=1-probt(tinv(1-a/2,n-1),n-1,lambda)
        +probt(tinv(a/2,n-1),n-1,lambda);
 print power;
quit;