* cluster randomized trial with two arms
 default values from Englert cited in Raudenbush and Bryk 2002.
NEW FILE.
INPUT PROGRAM.

COMPUTE N=12.
COMPUTE GAMMA01=.188.
COMPUTE SD=SQRT(.258**2+.019).
COMPUTE RHO=.019/(.258**2+.019).

COMPUTE TAU=SD**2*RHO.
COMPUTE SIGMA2=SD**2*(1-RHO).
COMPUTE A=.05.

LEAVE N TO A.

LOOP J=4 TO 100.
  COMPUTE LAMBDA=GAMMA01/SQRT(4*(SIGMA2/N+TAU)/J).
  COMPUTE T0=IDF.T(1-A/2,J-2).
  COMPUTE POWER=1-NCDF.T(T0,J-2,LAMBDA)+NCDF.T(-T0,J-2,LAMBDA).
  END CASE.
END LOOP.
END FILE.
END INPUT PROGRAM.
EXECUTE.

DELETE VARIABLES SD TAU SIGMA2 A LAMBDA T0.
LIST.

GRAPH
  /LINE(SIMPLE)=VALUE(POWER) BY J.

* cluster randomized trial with three arms
HbAC1  default values from 
Sidorov et al 2000 Am J Manag Care
Rothman et al 2003 Am J Med Qual.
NEW FILE.
INPUT PROGRAM.

COMPUTE N=40.
COMPUTE D1=1.9.
COMPUTE D2=1.
COMPUTE B1=(D1+D2)/2.
COMPUTE B2=D1-D2.

COMPUTE SD=2.
COMPUTE RHO=.05.
COMPUTE TAU=SD**2*RHO.
COMPUTE SIGMA2=SD**2*(1-RHO).
COMPUTE A=.05.

LEAVE N TO A.

LOOP J=6 TO 45.
  COMPUTE LAMBDA1=B1/SQRT(4.5*(SIGMA2/N+TAU)/J).
  COMPUTE LAMBDA2=B2/SQRT(6*(SIGMA2/N+TAU)/J).

  COMPUTE LAMBDA=LAMBDA1**2+LAMBDA2**2.
  COMPUTE P=1-NCDF.F(IDF.F(1-A,2,J-3),2,J-3,LAMBDA).
  
  COMPUTE NC1=D1/SQRT(6*(TAU+SIGMA2/N)/J).
  COMPUTE NC2=(D1-D2)/SQRT(6*(TAU+SIGMA2/N)/J).

  COMPUTE T0=IDF.T(1-A/2/2,J-3).
  COMPUTE P13=1-NCDF.T(T0,J-3,NC1)+NCDF.T(-T0,J-3,NC1).
  COMPUTE P12=1-NCDF.T(T0,J-3,NC2)+NCDF.T(-T0,J-3,NC2).
  END CASE.
END LOOP.
END FILE.
END INPUT PROGRAM.
EXECUTE.

DELETE VARIABLES SD TAU SIGMA2 A LAMBDA1 LAMBDA2 LAMBDA NC1 NC2 T0.
VARIABLE LABELS P 'POWER FOR OMNIBUS TEST'
                                P13 'POWER FOR COMPARING TREATMENT 1 AND 3'
                                P12 'POWER FOR COMPARING TREATMENT 1 AND 2'.
LIST.

GRAPH
  /LINE(MULTIPLE)=VALUE(P13 P12) BY J.

*multi-site randomized trial with two treatments at site.
NEW FILE.
INPUT PROGRAM.

COMPUTE N=30.
COMPUTE GAMMA10=.5.
COMPUTE SIGMA2=1.5**2.
COMPUTE TAU1=SIGMA2/9.
COMPUTE A=.05.

LEAVE N TO A.

LOOP J=5 TO 80.
  COMPUTE LAMBDA=GAMMA10/SQRT((4*SIGMA2/N+TAU1)/J).
  COMPUTE T0=IDF.T(1-A/2,J-1).
  COMPUTE POWER=1-NCDF.T(T0,J-1,LAMBDA)+NCDF.T(-T0,J-1,LAMBDA).
  END CASE.
END LOOP.
END FILE.
END INPUT PROGRAM.
EXECUTE.

DELETE VARIABLES  TAU1 A LAMBDA T0.
LIST.

GRAPH
  /LINE(SIMPLE)=VALUE(POWER) BY J.

*multi-site randomized trial with three treatments at site
     postpartum depression psychotheray/peer support/control
     default values from 
     Dennis et al 2009 BMJ
     Stevenson et al 2010 Health Tech Assessment
     total variance=sigma2+tau=4.6^2
     tau : sigma2 = 1 :10.

NEW FILE.
INPUT PROGRAM.

COMPUTE N=45.
COMPUTE DELTA1=3.4.
COMPUTE DELTA2=1.1.
COMPUTE SIGMA2=4.6**2/1.1.
COMPUTE TAU=4.6**2/1.1*.1.

COMPUTE GAMMA10=(DELTA1+DELTA2)/2.
COMPUTE GAMMA20=DELTA1-DELTA2.

COMPUTE SC1=.5**2+.5**2+(-1)**2.
COMPUTE SC2=1**2+(-1)**2+0**2.
COMPUTE TAU1=SC1*TAU.
COMPUTE TAU2=SC2*TAU.

COMPUTE A=.05.

LEAVE N TO A.

LOOP J=6 TO 60.
  COMPUTE LAMBDA1=SQRT(J)*GAMMA10/SQRT(SC1*3*SIGMA2/N+TAU1).
  COMPUTE LAMBDA2=SQRT(J)*GAMMA20/SQRT(SC2*3*SIGMA2/N+TAU2).
  COMPUTE LAMBDA=LAMBDA1**2+LAMBDA2**2.
  COMPUTE P=1-NCDF.F(IDF.F(1-A,2,2*(J-1)),2,2*(J-1),LAMBDA).
     *power for Helmert contrasts.
  COMPUTE T0=IDF.T(1-A/2,J-1).
  COMPUTE P1=1-NCDF.T(T0,J-1,LAMBDA1)+NCDF.T(-T0,J-1,LAMBDA1).
  COMPUTE P2=1-NCDF.T(T0,J-1,LAMBDA2)+NCDF.T(-T0,J-1,LAMBDA2).
  END CASE.
END LOOP.
END FILE.
END INPUT PROGRAM.
EXECUTE.

DELETE VARIABLES  SC1 SC2 TAU1 TAU2 A LAMBDA LAMBDA1 LAMBDA2 T0.
LIST.



