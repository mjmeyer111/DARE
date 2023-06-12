*chapter 2 ;
* z test;
data null;
n=62;
a=.05;
d=.5;
nc=d*sqrt(n/2);
p1=1-probnorm(probit(1-a)-nc); 
p2=probnorm(nc-probit(1-a/2))+probnorm(probit(a/2)-nc);
put "power in a one-sided test is  " p1;
put "power in a two-sided test is  " p2;
run;

*independent t test;
data null;
n1=65;
n2=65;
v=n1+n2-2;
a=.05;
d=.5;
nc=d*sqrt(n1*n2/(n1+n2));
p=1-probt(tinv(1-a/2,v),v,nc)+probt(tinv(a/2,v),v,nc);
put "power in the independent t test is " p;
run;

*dependent t test;
data null;
n=34;
a=.05;
r=.5;
d=.5;
nc=sqrt(n/2)*d/sqrt(1-r);
p=1-probt(tinv(1-a/2,n-1),n-1,nc)+probt(tinv(a/2,n-1),n-1,nc);
put "power in the dependent t test is " p;
run;

*one sample t test;
proc power;
onesamplemeans
mean = -250
sides= 1
ntotal = 30
stddev = 500
power = .;
plot x=n min=5 max=100;
run;

* use proc power to plot power curve for independent t ;
proc power;
twosamplemeans test=diff
meandiff = 30 50
stddev = 100
npergroup = 20 to 300 by 5
power = .;
plot x=n min=20 max=300;
run;

*use data step and gplot to produce power curve for independent t;
data it(keep=d n p);
a=.05; stddev=100;
do es=30, 50;
 do n=20 to 300 by 5;
  v=2*n-2;
  d=es/stddev;
  nc=d*sqrt(n/2);
  p=1-probt(tinv(1-a/2,v),v,nc)+probt(tinv(a/2,v),v,nc);
  output;
 end;
end;
label d='Effect size' p='Power';
run;

*proc print data=it;run;
symbol1 c=b i=join l=3;
symbol2 c=bl i=join l=1;
proc gplot data=it;
 plot p*n=d;
 title h=2.5 j=c "Statistical power vs. sample size n in independent t";
run;

*power curve for dependent t ;
proc power;
pairedmeans test=diff
meandiff =3 5
corr = 0.5
stddev = 10
npairs = 5 to 100 by 5
power = .;
plot x=n min=5 max=100;
run;

*use data step and gplot to produce power curve for dependent t;
data dt(keep=d n p);
a=.05; stddev=10; r=0.5;
do es=3, 5;
 do n=5 to 100 by 5;
  v=n-1;
  d=es/stddev;
  nc=sqrt(n/2)*d/sqrt(1-r);
  p=1-probt(tinv(1-a/2,v),v,nc)+probt(tinv(a/2,v),v,nc);
  output;
 end;
end;
label d='Effect size' p='Power';
run;

*proc print data=dt;run;
symbol1 c=b i=join l=3;
symbol2 c=bl i=join l=1;
proc gplot data=dt;
 plot p*n=d;
 title h=2.5 j=c "Statistical power vs. sample size n in dependent t";
run;

