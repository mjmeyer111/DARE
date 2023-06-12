****************************************;
*SAS Hotelling T;

proc iml;

sd={120 150};
mu={40, 70};
d=inv(diag(sd))*mu;

*print d;
R={1 .35, .35 1};

start p4mt(n,d,R);
 v1=nrow(d);
 v2=2*n-v1-1;
 a=.05;
 f0=finv(1-a, v1,v2);
 lambda= .5*n*t(d)*inv(R)*d;
 power=1-probf(f0, v1,v2, lambda);
 return(power);
finish p4mt;

             *compute power for sample size 20-120;
 nmin=20;
 nmax=120;
 power=j(nmax-nmin+1,1,.);
 do n=nmin to nmax;
  power[n-nmin+1,1]=p4mt(n,d,R);
 end;

n=nmin:nmax;
table=t(n)||power;
print table [colname={n power}];

create npower from table[colname={n power}];
append from table;
close npower;

quit;
              *plot power against n ;

symbol color=black  interpol=join line=1;
proc gplot data=npower;
plot power*n;
run;


****************************************;

*SAS MANOVA;

proc iml;

sd={120,150,1};
D=diag(sd);
R={ 1 .35 .62,
   .35  1 .22,
   .62 .22  1};

d1={ 50  20  0};
d2={70 20 0};
d3={.5 .2 0};

j=j(1,3,1);

a1=d1-sum(d1)/ncol(d1)*j;
a2=d2-sum(d2)/ncol(d2)*j;
a3=d3-sum(d3)/ncol(d3)*j;
a=a1//a2//a3;

E=D*R*D;

              *power function for MANOVA;
start p4maov(n,a,E);
 p=nrow(a);
 k=ncol(a);
 v1=k-1;
 v2=(n-1)*k;

 t=sqrt((p**2*v1**2-4)/(p**2+v1**2-5));
 w=v1+v2-.5*(p+v1+1);

 df1=v1*p;
 df2=w*t-.5*(p*v1-2);

 aa=a*t(a);
 bigA=n*aa;
 L=det(v2*E)/det(v2*E+bigA);
 lambda=(L**(-1/t)-1)*df2;

 alpha=.05;
 power=1-probf(finv(1-alpha,df1,df2),df1,df2,lambda);
 return(power);
 finish p4maov;

               *compute power for sample size 35-70;
 nmin=35;
 nmax=75;
 power=j(nmax-nmin+1,1,.);
 do n=nmin to nmax;
  power[n-nmin+1,1]=p4maov(n,a,E);
 end;

n=nmin:nmax;
table=t(n)||power;
print table [colname={n power}];

quit;



**************************************************;
*SAS MANCOVA;

proc iml;

sd={120,150,1};
D=diag(sd);
R={ 1 .35 .62,
   .35  1 .22,
   .62 .22  1};

d1={ 50  20  0};
d2={70 20 0};
d3={.5 .2 0};

j=j(1,3,1);

a1=d1-sum(d1)/ncol(d1)*j;
a2=d2-sum(d2)/ncol(d2)*j;
a3=d3-sum(d3)/ncol(d3)*j;
a=a1//a2//a3;

Ryx={.64, .34, .53};

Ex=D*(R-Ryx*t(Ryx))*D;
q=ncol(Ryx);
*print sd, D, R, d1,j,a1,a2,a3,a, AA,E;

                     *power function for MANCOVA;
start p4macov(n,a,Ex,q);
 p=nrow(a);
 k=ncol(a);
 v1=k-1;
 v2=(n-1)*k-q;

 t=sqrt((p**2*v1**2-4)/(p**2+v1**2-5));
 w=v1+v2-.5*(p+v1+1);

 df1=v1*p;
 df2=w*t-.5*(p*v1-2);

 aa=a*t(a);
 bigA=n*aa;
 L=det(v2*Ex)/det(v2*Ex+bigA);
 lambda=(L**(-1/t)-1)*df2;

 alpha=.05;
 power=1-probf(finv(1-alpha,df1,df2),df1,df2,lambda);
 return(power);
finish p4macov;
                   *compute power for sample size 30-60;

 nmin=30;
 nmax=60;
 power=j(nmax-nmin+1,1,.);
 do n=nmin to nmax;
  power[n-nmin+1,1]=p4macov(n,a,Ex,q);
 end;

n=nmin:nmax;
table=t(n)||power;
print table [colname={n power}];

quit;
