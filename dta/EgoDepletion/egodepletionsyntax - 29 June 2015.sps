*Select cases that were not dropped due to performance on MSIT and Letter E task, who spoke NL as first language, and who were greater than 17 years old.

USE ALL.
COMPUTE filter_$=(Include.Overall = "TRUE" & lang = 1 & age > 17).
VARIABLE LABELS filter_$ 'Include.Overall = "TRUE" & lang = 1 & age > 17 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*compare mean differences.

T-TEST GROUPS=Task('E' 'H')
  /MISSING=ANALYSIS
  /VARIABLES=ExGauss.I.RTVar.MSIT ExGauss.I.mu.MSIT I_1_MeanRT.MSIT Acc.Overall.LetE effort difficult tired frustrated
  /CRITERIA=CI(.95).


*Select cases who spoke NL as first language, and who were greater than 17 years old. Ignores error rates.

USE ALL.
COMPUTE filter_$=(lang = 1 & age > 17).
VARIABLE LABELS filter_$ 'Include.Overall = "TRUE" & lang = 1 & age > 17 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*compare mean differences.

T-TEST GROUPS=Task('E' 'H')
  /MISSING=ANALYSIS
  /VARIABLES=ExGauss.I.RTVar.MSIT ExGauss.I.mu.MSIT I_1_MeanRT.MSIT Acc.Overall.LetE effort difficult tired frustrated
  /CRITERIA=CI(.95).
