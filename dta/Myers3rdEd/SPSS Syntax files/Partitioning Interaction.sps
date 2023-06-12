*This calculates contrasts of polynomial components using the GSR2 data.
UNIANOVA GSR BY Category Stimulus
  /LMATRIX = "Linear (C vs MS, SS)"
  	Category * Stimulus  -4 -2 0 2 4 2 1 0 -1 -2 2 1 0 -1 -2
  /LMATRIX = "Linear (MS vs SS)"						
	Category * Stimulus  0 0 0 0 0 2 1 0 -1 -2  -2 -1 0 1 2
  /LMATRIX = "Quadratic (C vs MS, SS)"
  	Category * Stimulus 4 -2 -4 -2 4 -2 1 2 1 -2 -2 1 2 1 -2
  /LMATRIX = "Quadratic (MS vs SS)"
	Category * Stimulus  0 0 0 0 0 -2 1 2 1 -2 2 -1 -2 -1 2
  /DESIGN = Category, stimulus, Category * Stimulus.