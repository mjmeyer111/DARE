* cluster randomized trial with two arms;
*default values from Englert cited in Raudenbush and Bryk 2002;
data crt2(keep=J n gamma01 rho p);
n=12;
gamma01=.188;
sd=sqrt(.258**2+.019);
rho=.019/(.258**2+.019);

tau=sd**2*rho;
sigma2=sd**2*(1-rho);
a=.05;

do J=4 to 100;
 lambda=gamma01/sqrt(4*(sigma2/n+tau)/J);
 t0=tinv(1-a/2,J-2);
 p=1-probt(t0,J-2,lambda)+probt(-t0,J-2,lambda);
 output;
end;
run;

*proc print; run;

symbol color=black interpol=join line=1;
proc gplot data=crt2;
 plot p*J;
run;

* cluster randomized trial with three arms;
/*HbAC1: default values from 
Sidorov et al 2000 Am J Manag Care
Rothman et al 2003 Am J Med Qual
*/
data crt3(keep=J n D1 D2 rho p p13 p12);
a=.05;
n=40;

D1=1.9; D2=1;
b1=(D1+D2)/2;
b2=D1-D2;

sd=2;
rho=.05;
tau=sd**2*rho;
sigma2=sd**2*(1-rho);

do J=12 to 30;
 lambda1=b1/sqrt(4.5*(tau+sigma2/n)/J);
 lambda2=b2/sqrt(6*(tau+sigma2/n)/J);
 lambda=lambda1**2+lambda2**2;

 f0=finv(1-a,2,J-3);
 p=1-probf(f0,2,J-3,lambda);

 nc1=D1/sqrt(6*(tau+sigma2/n)/J);
 nc2=(D1-D2)/sqrt(6*(tau+sigma2/n)/J);

 t0=tinv(1-a/2/2,J-3);
 p13=1-probt(t0,J-3,nc1)+probt(-t0,J-3,nc1);
 p12=1-probt(t0,J-3,nc2)+probt(-t0,J-3,nc2);
 output;
end;
run;

proc print data=crt3;run;

*multi-site randomized trial with two treatments at site;
data mst2(keep=J n gamma10 sigma2 tau1 p);
a=.05;
n=30;

gamma10=.5;
sigma2=1.5**2;
tau1=1.5**2/9;

do J=10 to 30;
 lambda=gamma10/sqrt((4*sigma2/n+tau1)/J);
 t0=tinv(1-a/2,J-1);
 p=1-probt(t0,J-1,lambda)+probt(-t0,J-1,lambda);
 output;
end;
run;

*proc print data=mst2;run;


*multi-site randomized trial with three treatments at site;
/*
postpartum depression psychotheray/peer support/control
default values from 
Dennis et al 2009 BMJ
Stevenson et al 2010 Health Tech Assessment
total variance=sigma2+tau=4.6^2
tau : sigma2 = 1 :10
*/
data mst3(keep=n J sigma2 tau p p1 p2);
a=.05;
n=45;
Delta1=3.4;
Delta2=1.1;
sigma2=4.6**2/1.1;
tau=4.6**2/1.1*.1;

c11=.5; c12=.5; c13=-1;
c21=1;  c22=-1; c23=0;
sc1=c11**2+c12**2+c13**2;
sc2=c21**2+c22**2+c23**2;

gamma10=(Delta1+Delta2)/2;
gamma20=Delta1-Delta2;

tau1=sc1*tau;
tau2=sc2*tau;

do J=6 to 35;
 lambda1=sqrt(J)*gamma10/sqrt(sc1*3*sigma2/n+tau1);
 lambda2=sqrt(J)*gamma20/sqrt(sc2*3*sigma2/n+tau2);
 lambda=lambda1**2+lambda2**2;

 f0=finv(1-a,2,2*(J-1));
 p=1-probf(f0,2,2*(J-1),lambda);

 *power for orthogonal contrasts;
 t0=tinv(1-a/2,J-1);
 p1=1-probt(t0,J-1,lambda1)+probt(-t0,J-1,lambda1);
 p2=1-probt(t0,J-1,lambda2)+probt(-t0,J-1,lambda2);

 output;
end;
run;

