﻿* POWER IN SIMPLE REGRESSION.
DATA LIST LIST/N B1 SIGMA SIGMAX.
BEGIN DATA
20 .6 12 15
END DATA.
COMPUTE LAMBDA=B1**2*SIGMAX**2*N/SIGMA**2.
COMPUTE P=1-NCDF.F(IDF.F(.95,1,N-2),1,N-2,LAMBDA).
EXECUTE.

* POWER FOR TESTING A CORRELATION.
DATA LIST LIST/N R.
BEGIN DATA
44 .40
END DATA.
COMPUTE T0=IDF.T(.975,N-2).
COMPUTE NC=R*SQRT(N)/SQRT(1-R**2).
COMPUTE P=1-NCDF.T(T0,N-2,NC)+NCDF.T(-T0,N-2,NC).
EXECUTE.

* POWER IN MULTIPLE REGRESSION.
DATA LIST LIST/N A P DF1 R2S R2F.
BEGIN DATA
90 .01 5  2  .12 .25
END DATA.
COMPUTE F2=R2S/(1-R2F).
COMPUTE DF2=N-P-1.
COMPUTE NCF=F2*N.
COMPUTE NCR=F2*(DF1+DF2+1).
COMPUTE PF=1-NCDF.F(IDF.F(1-A,DF1,DF2),DF1,DF2,NCF).
COMPUTE PR=1-NCDF.F(IDF.F(1-A,DF1,DF2),DF1,DF2,NCR).
EXECUTE.

VARIABLE LABELS PF 'Power-fixed model'
PR 'Power-random model'.
DELETE VARIABLES A TO R2F.