*brinjal_BaselineCharacteristics_balance_attrition_replication_AJAE.do
/* This do file replicates Table 1 and Table 2 */

set more off
clear all  

// Setting up a path for the data file 
global data = "C:\Users\NAbedin\Dropbox (IFPRI)\Naveen\Bt Brinjal\Bt Brinjal\AJAE\AJAE R&R\AJAE Files\September 24th 2020\Data Files\"
// Setting up a place to put the results
global results = "C:\Users\NAbedin\Dropbox (IFPRI)\Naveen\Bt Brinjal\Bt Brinjal\AJAE\AJAE R&R\AJAE Files\September 24th 2020\Results\"


use "$data\Balance&Attrition.dta", clear


global BLCHAR yield_bl pesticide_cost_bl yearsedu_brinjalgrwer age_brinjalgrwer years_farmer opland wealthV2 

//Baseline Household Characteristics reported in Table 1
tabstat $BLCHAR, stats(mean sd count) by(Treatment) long column(statistics) format (%6.2f)

ttest yield_bl, by(Treatment)
ttest pesticide_cost_bl, by(Treatment)
ttest yearsedu_brinjalgrwer, by(Treatment)
ttest age_brinjalgrwer, by(Treatment)
ttest years_farmer, by(Treatment)
ttest opland, by(Treatment)
ttest wealthV2, by(Treatment)

//This regression reports the balance test in Table 2, Column 1
reg Treatment yearsedu_brinjalgrwer age_brinjalgrwer years_farmer opland wealthV2 yield_bl pesticide_cost_bl, cluster(village)
	est store Col_1
//This regression reports the atrition test in Table 2, Column 2
reg attrition Treatment yearsedu_brinjalgrwer age_brinjalgrwer years_farmer opland wealthV2 yield_bl pesticide_cost_bl, cluster(village)
	est store Col_2
outreg2 [Col_1 Col_2] using "$results/Table2" ,  bdec(3) long se nonotes nocons nor /*
	*/ label word  replace
