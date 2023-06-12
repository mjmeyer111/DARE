*power for overall F test;
proc power;
* proc power plotonly; /*plotonly produces only the plot */
  onewayanova test=overall
    groupmeans = 8 | 11 | 15
    stddev = 4
    npergroup = 5 to 12
    power = .;
  plot x=n min=5 max=20;
run;

* power for contrast test;
proc power;
  onewayanova test=contrast
    contrast = (-1 .5 .5) (0 -1 1)
    groupmeans = 8 | 11 | 15
    stddev = 4
    npergroup = 5 to 30
    power = .;
  plot x=n min=5 max=30;
run;

*power for random effects ANOVA;
data raov(keep=n J power); 
 a=.05;
 n=20;
 rho=.30;
 m=(1-rho)/(n*rho+(1-rho));

 do J=3 to 10;
  f0=finv(1-a,J-1,J*(n-1));
  power=1-probf(f0*m,J-1,J*(n-1));
  output;
 end;
run;

proc print data=raov;run;


  

