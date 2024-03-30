*brinjal_Pesticides_replication_AJAE.do
/* This do file replicates Table 3 (Columns 6 to 10), Table 6 and Table S2.6 */

set more off
clear all  

// Setting up a path for the data file 
global data = "C:\Users\NAbedin\Dropbox (IFPRI)\Naveen\Bt Brinjal\Bt Brinjal\AJAE\AJAE R&R\AJAE Files\September 24th 2020\Data Files\"
// Setting up a place to put the results
global results = "C:\Users\NAbedin\Dropbox (IFPRI)\Naveen\Bt Brinjal\Bt Brinjal\AJAE\AJAE R&R\AJAE Files\September 24th 2020\Results\"

use "$data\Pesticides.dta", clear

global HHCHAR1 yearsedu_brinjalgrwer age_brinjalgrwer years_farmer wealthV1 opland 

// Regressions: Table 3 (columns 6 to 10)
reg pesticide_cost_el Treatment pesticide_cost_bl, cluster(village)
	est store Col_1
reg pesticide_cost_el Treatment pesticide_cost_bl $HHCHAR1, cluster(village)
	est store Col_2
reg pesticide_cost_el_win Treatment pesticide_cost_bl_win $HHCHAR1, cluster(village)
	est store Col_3
reg pesticide_lcost_el Treatment pesticide_lcost_bl $HHCHAR1, cluster(village)
	est store Col_4
reg pesticide_cost_el_ihs Treatment pesticide_cost_bl_ihs $HHCHAR1, cluster(village)
	est store Col_5
outreg2 [Col_1 Col_2 Col_3 Col_4 Col_5] using "$results/Table3_CostofPesticides" ,  bdec(3) long se nonotes nocons nor /*
	*/ label word  replace
	
// Regressions: Table 6
reg pesticide_sprays_el Treatment pesticide_sprays_bl $HHCHAR1, cluster(village)
	est store Col_1
reg pesticide_quantity_el Treatment pesticide_quantity_bl $HHCHAR1, cluster(village)
	est store Col_2
reg feiq_el Treatment feiq_bl $HHCHAR1, cluster(village)
	est store Col_3
reg ceiq_el Treatment ceiq_bl $HHCHAR1, cluster(village)
	est store Col_4
reg weiq_el Treatment weiq_bl $HHCHAR1, cluster(village)
	est store Col_5
reg eceiq_el Treatment eceiq_bl $HHCHAR1, cluster(village)
	est store Col_6
reg puts_el Treatment puts_bl $HHCHAR1, cluster(village)
	est store Col_7
	
outreg2 [Col_1 Col_2 Col_3 Col_4 Col_5 Col_6 Col_7] using "$results/Table6" ,  bdec(3) long se nonotes nocons nor /*
	*/ label word  replace

rename pesticide_sprays_el  pesticide_sprays
rename pesticide_quantity_el pesticide_quantity
rename feiq_el feiq
rename ceiq_el ceiq
rename weiq_el weiq
rename eceiq_el eceiq
rename puts_el puts

// This regression reports the Romano-Wolf p-values that appear in the supplementary appendix, Table S2.6
rwolf pesticide_sprays pesticide_quantity feiq ceiq weiq eceiq puts, indepvar(Treatment) controls($HHCHAR1) bl(_bl) reps(1000) cluster(village) vce(cluster village) seed(20)


