
UNIANOVA
  Y  BY Test Duration Subject Item
  /RANDOM = Subject Item
  /CONTRAST (Duration)=Polynomial
  /METHOD = SSTYPE(3)
  /INTERCEPT = INCLUDE
  /PLOT = PROFILE( Duration*Test )
  /CRITERIA = ALPHA(.05)
  /DESIGN = Test Duration Subject(Test) Item(Duration) Test*Subject(Duration)
   Test*Duration Test*Item(Duration).
