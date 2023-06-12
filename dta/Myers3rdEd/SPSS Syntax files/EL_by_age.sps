RECODE schoolyr (Lowest thru 3=1) (Lowest thru 4=2) (Lowest thru 5=2) (Lowest thru 6=2) (Lowest thru 7=3) (Lowest thru 8=3) INTO EL. 
EXECUTE. 
UNIANOVA tc BY EL 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /CRITERIA=ALPHA(0.05) 
  /DESIGN=EL.
UNIANOVA tc BY EL WITH age 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /CRITERIA=ALPHA(0.05) 
  /DESIGN=EL age EL*age.