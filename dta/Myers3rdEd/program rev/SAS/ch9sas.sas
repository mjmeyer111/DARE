/*
fixed effects meta-analysis
*/
data fma(drop=a lambda vi);
a=.05;
ne=50;
nc=50;
es=.17;
vi=(ne+nc)/(ne*nc) + .5*es**2/(ne+nc);

do I=5 to 20;
 lambda=sqrt(I)*es/sqrt(vi);
 power=1-probnorm(probit(1-a/2)-lambda)
       +probnorm(probit(a/2)-lambda);
 output;
end;
run;

/*
random effects meta-analysis
*/
data rma(drop=a lambda vi);
a=.05;
ne=25;
nc=25;
es=.20;
p=1/3;

vi=(ne+nc)/(ne*nc) + .5*es**2/(ne+nc);

do I=5 to 30;
 lambda=sqrt(I)*es/sqrt(vi)/sqrt(1+p);
 power=1-probnorm(probit(1-a/2)-lambda)
       +probnorm(probit(a/2)-lambda);
 output;
end;
run;