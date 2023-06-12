*power in simple regression;
data sr(keep=n power);
 a=.05;
 b1=.6;
 sigma=12;
 sigmax=15;
 do n=10 to 50;
  f0=finv(1-a,1,n-2);
  lambda=b1**2*sigmax**2*n/sigma**2;
  power=1-probf(f0,1,n-2,lambda);
  output;
 end;
run;

proc print data=sr; run;

proc gplot data=sr;
 plot power*n;
 title "Statistical power in simple regression";
run;

* power for testing a correlation;
 
*use onecorr statement in proc power;
proc power;
 onecorr 
   dist=t
   model=fixed     /*by default model=random          */
   corr=.40        /*It treats both x and y as random */
   ntotal=10 to 50
   power= .;
 plot x=n min=10 max=50;
run;

*use multreg statement;
   *R square= r^2=.4^2=.16;
   *R square w/o 1 predictor is 0. So R square diff is .16;
proc power;
 multreg
   model = fixed
   nfullpredictors = 1
   ntestpredictors = 1
   rsquarefull = 0.16  
   rsquarediff = 0.16  
   ntotal = 10 to 50
   power = .;
run;

* power in multiple regression;
  *parameter values from Cohen p 432;
proc power;
 multreg
   model = fixed
   alpha=.01
   nfullpredictors = 5
   ntestpredictors = 2
   rsquarefull = 0.25  
   rsquarediff = 0.12  
   ntotal = 90
   power = .;
run;
  *parameter values from Cohen p 439;
proc power;
 multreg
   model = fixed random
   alpha=.01
   nfullpredictors = 12
   ntestpredictors = 3
   rsquarefull = 0.20  
   rsquarediff = 0.06  
   ntotal = 200
   power = .;
run;

 