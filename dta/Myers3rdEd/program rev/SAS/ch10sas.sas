*power in path analysis;
*power for testing model fit;
data null;
R2y1=.49647;
R2y2=.1696;
p=2; q=2;

GR2=1-(1-R2y1)*(1-R2y2);
N=38;
d=p*q+p*(p-1)/2;
W=-(N-d)*log(1-GR2);

a=.05;
lambda=W-d;
power=1-probchi(cinv(1-a,d),d,lambda);
put power=;
run;

*power for comparing path models;
data null;
R2y1_1=.5**2;
R2y2_1=.1696;

R2y1_2=.49647;
R2y2_2=.1696;

GR2_1=1-(1-R2y1_1)*(1-R2y2_1);
GR2_2=1-(1-R2y1_2)*(1-R2y2_2);

d=2;
N=38;
W=-(N-d)*log((1-GR2_2)/(1-GR2_1));

a=.05;
lambda=W-d;
power=1-probchi(cinv(1-a,d),d,lambda);
put power=;
run;

* factor analysis;
DATA   rmatrix  (TYPE=CORR);      
  _TYPE_='CORR';        
  INPUT y1-y3;
cards;
1.00 0.64 0.66
0.64 1.00 0.68
0.66 0.68 1.00
RUN;

proc factor data=rmatrix n=1 score;
run;

*power in sem;

proc iml;
*full model 2;
*eta1 ~ eta2 xi1 xi2;
Rte1={.4, .3, .3};
Rt_1={1 .3 .5,
   .3 1 .2,
   .5 .2 1};
rho2_1=diag({.9,.75, .75});
rho_1=sqrt(rho2_1);

R_1=rho_1*Rt_1*rho_1+I(nrow(Rt_1))-rho_1*rho_1;
Re1=rho_1*Rte1*sqrt(.8);
R2_1=t(Re1)*inv(R_1)*Re1;
print R2_1[label="model 2: R^2 for eta1"];

*eta2 ~ xi1 xi2;
Rte2={.3, .5};
Rt_2={1 .2,
   .2 1 };
rho2_2=diag({.75, .75});
rho_2=sqrt(rho2_2);

R_2=rho_2*Rt_2*rho_2+I(nrow(Rt_2))-rho_2*rho_2; 
Re2=rho_2*Rte2*sqrt(.9);
R2_2=t(Re2)*inv(R_2)*Re2;
print R2_2[label="model 2: R^2 for eta2"];

GR2_2=1-(1-R2_1)*(1-R2_2);
print GR2_2[label="model 2: GR^2"];

*reduced model 1;

* eta1 ~ eta2 + xi2;
Rte1={.4, .3};
Rt_1={1 .5,
      .5  1};
rho2_1=diag({.9,.75});
rho_1=sqrt(rho2_1);

R_1=rho_1*Rt_1*rho_1+I(nrow(Rt_1))-rho_1*rho_1; 
Re1=rho_1*Rte1*sqrt(.8);
R2_1=t(Re1)*inv(R_1)*Re1;
print R2_1[label="model 1: R^2 for eta1"];

*eta2 ~ xi2;
R2_2=.75*.5**2*.9;
print R2_2[label="model 1: R^2 for eta2"];

GR2_1=1-(1-R2_1)*(1-R2_2); 
print GR2_1[label="model 1: GR^2"];
*power;
ratio= (1-GR2_2)/(1-GR2_1);
*print ratio;
d=2; a=.05; N=172;
lambda=-(N-d)*log(ratio) -d;
power=1-probchi(cinv(1-a,d),d,lambda);
print power;
quit iml;




