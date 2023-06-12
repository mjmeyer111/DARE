* 3-level cluster randomized trial
     rho1 correlation btw obs within smaller cluster (school)
     rho2 correlation btw obs (sch means) within larger cluster (district).

NEW FILE.
INPUT  PROGRAM.

COMPUTE A=.05.
COMPUTE J=5.
COMPUTE N=12.
COMPUTE GAMMA001=.5.
COMPUTE SD=1.
COMPUTE RHO1=.3.
COMPUTE RHO2=.3.

COMPUTE SIGMA2=SD**2*(1-RHO1).
COMPUTE TAU_PI=SD**2*RHO1*(1-RHO2).
COMPUTE TAU_B=SD**2*RHO1*RHO2.

LEAVE A TO TAU_B.

LOOP K=6 TO 30 BY 2.
  COMPUTE LAMBDA=GAMMA001/SQRT(4*(TAU_B+(TAU_PI+SIGMA2/N)/J)/K).
  COMPUTE T0=IDF.T(1-A/2,K-2).
  COMPUTE POWER=1-NCDF.T(T0,K-2,LAMBDA)+NCDF.T(-T0,K-2,LAMBDA).
  END CASE.
END LOOP.
END FILE.
END INPUT PROGRAM.

DELETE VARIABLES A SD SIGMA2 TAU_PI TAU_B LAMBDA T0.
LIST.

*  multisite cluster randomized trial
      rho: intraclass correlation within cluster conditional on site
      rho=tau_pi /( tau_pi +sigma2)
      sd^2=sigma2 + tau_pi: within cluster variance conditional on site  
      sd^2 : tau_u1 = 1 : p.

NEW FILE.
INPUT  PROGRAM.

COMPUTE A=.05.
COMPUTE J=8.
COMPUTE N=12.
COMPUTE GAMMA010=.5.
COMPUTE SD=1.
COMPUTE RHO=.3.
COMPUTE P=.1.

COMPUTE SIGMA2=SD**2*(1-RHO).
COMPUTE TAU_PI=SD**2*RHO.
COMPUTE TAU_U1=SD**2*P.

LEAVE A TO TAU_U1.

LOOP K=4 TO 20.
  COMPUTE LAMBDA=GAMMA010*SQRT(K)/SQRT(4*(TAU_PI+SIGMA2/N)/J +TAU_U1).
  COMPUTE T0=IDF.T(1-A/2,K-1).
  COMPUTE POWER=1-NCDF.T(T0,K-1,LAMBDA)+NCDF.T(-T0,K-1,LAMBDA).
  END CASE.
END LOOP.
END FILE.
END INPUT PROGRAM.

DELETE VARIABLES A SD SIGMA2 TAU_PI TAU_U1 LAMBDA T0.
LIST.
