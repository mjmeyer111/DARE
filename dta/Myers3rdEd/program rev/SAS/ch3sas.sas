*sample size and power of confidence interval;
proc power;
twosamplemeans ci=diff
halfwidth=1
stddev=2
/*probtype=unconditional*/ 
probwidth=.
ntotal=74;
run;
*probtype=unconditional yields pi instead of the default pi1; 