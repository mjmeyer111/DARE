/*
3-level cluster randomized trial

rho1 correlation btw obs's within smaller cluster (school)
rho2 correlation btw obs's (sch means) within larger cluster (district)
*/
data l3crt(keep=J n K gamma001 sd rho1 rho2 power);
a=.05;
J=5;
n=12;
gamma001=.3;
sd=1;
rho1=.3;
rho2=.2;

sigma2=sd**2*(1-rho1);
tau_pi=sd**2*rho1*(1-rho2);
tau_b=sd**2*rho1*rho2;

do K=6 to 50 by 2;
 lambda=gamma001/sqrt(4*(tau_b+(tau_pi+sigma2/n)/J)/K);
 t0=tinv(1-a/2,K-2);
 power=1-probt(t0,K-2,lambda)+probt(-t0,K-2,lambda);
 output;
end;
run;

/*
multisite cluster randomized trial

rho: intraclass correlation within cluster conditional on site
rho=tau_pi /( tau_pi +sigma2)
sd^2=sigma2 + tau_pi: within cluster variance conditional on site  
sd^2 : tau_u1 = 1 : p
*/

data mcrt(keep=J n K gamma010 sd rho p power);
a=.05;
J=8;
n=12;
gamma010=.5;
sd=1;
rho=.3;
p=.1;

sigma2=sd**2*(1-rho);
tau_pi=sd**2*rho;
tau_u1=sd**2*p;

do K=4 to 20;
 lambda=gamma010*sqrt(K)/sqrt(4*(tau_pi + sigma2/n)/J + tau_u1);
 t0=tinv(1-a/2,K-1);
 power=1-probt(t0,K-1,lambda)+probt(-t0,K-1,lambda);
 output;
end;
run;
