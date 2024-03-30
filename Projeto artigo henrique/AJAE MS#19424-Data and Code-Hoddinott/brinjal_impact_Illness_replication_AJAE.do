*brinjal_impact_illness_replication.do

/*
This do file replicates Tables 7a, 7b, and Supplementary Tables S2.7, S2.8a and S2.8b*/
 
 
set more off
clear all  

// Setting up a place to put the results
global results "C:\Bangla_btbrinjal\AJAE\Revision2\Results"

// Setting up a path for the data file 
global data3 "C:\Bangla_btbrinjal\AJAE\Revision2\Replication\data\"



	// Open the data file and do some preliminary organizing
use "$data3/Illness.dta", 

	// Namelists
global PERCHAR1 age sex relHead_2-relHead_4 /* Control variables, individual characteristics */
global HHCHAR1 yearsedu_brinjalgrwer age_brinjalgrwer years_farmer wealthV1 opland /* Control variables, grower and hh characteristics */

global BASELINE anyIllness_Base numIllness_Base anydays_lost_Base days_lost_Base seek_treat_Base any_medexp_Base medexp_Base
global OUTCOMES anyIllness_End numIllness_End anydays_lost_End days_lost_End seek_treat_End any_medexp_End medexp_End

sum $BASELINE
sum $OUTCOMES 

// Descriptive statistics reported in Supplementary Table S2.7
tabstat $PERCHAR1 relHead_1 $BASELINE , stats(mean sd count) long column(statistics) format (%6.2f)
tabstat $BASELINE , stats(mean sd count) by(Treatment) long column(statistics) format (%6.2f)


// Regressions: Table 7a
reg anyIllness_End Treatment anyIllness_Base $PERCHAR1 $HHCHAR1, cluster(village)
	est store Col_1
reg numIllness_End Treatment numIllness_Base $PERCHAR1 $HHCHAR1,  cluster(village)
	est store Col_2
reg anydays_lost_End Treatment anydays_lost_Base $PERCHAR1 $HHCHAR1, cluster(village)
	est store Col_3
reg seek_treat_End Treatment seek_treat_Base $PERCHAR1 $HHCHAR1, cluster(village)
	est store Col_4
reg any_medexp_End Treatment any_medexp_Base $PERCHAR1 $HHCHAR1, cluster(village)
	est store Col_5
outreg2 [Col_1 Col_2 Col_3 Col_4 Col_5] using "$results/Table7a" ,  bdec(3) long se nonotes nocons nor /*
	*/ label word  replace

	
// These regressions assess robustness of Table 7a results to alternative model specifications
tobit numIllness_End numIllness_Base Treatment $PERCHAR1 $HHCHAR1,  ll(0) ul(9) vce(cluster village)
mfx
poisson numIllness_End numIllness_Base Treatment $PERCHAR1 $HHCHAR1, cluster(village) 
mfx	
reg medexp_End medexp_Base Treatment $PERCHAR1 $HHCHAR1,  cluster(village) 

// These regressions report the Romano-Wolf p-values that appear in the supplementary appendix, Table S2.8a
//rwolf anyIllness numIllness anydays_lost seek_treat any_medexp , indepvar(Treatment) controls($HHCHAR1) bl(_Base) reps(1000) cluster(village) vce(cluster village) seed(20)
rwolf anyIllness_End numIllness_End anydays_lost_End seek_treat_End any_medexp_End , indepvar(Treatment) controls($PERCHAR1 $HHCHAR1) bl(_Base) reps(1000) cluster(village) vce(cluster village) seed(20)
	
 
// Regressions: Table 7b
preserve
keep if c1_03==1 | c1_04==1 /* Either persistent respiratory or skin problems */
reg anyIllness_End Treatment anyIllness_Base $PERCHAR1 $HHCHAR1, cluster(village)
	est store Col_1
reg numIllness_End Treatment numIllness_Base $PERCHAR1 $HHCHAR1,  cluster(village)
	est store Col_2
reg anydays_lost_End Treatment anydays_lost_Base $PERCHAR1 $HHCHAR1, cluster(village)
	est store Col_3
reg seek_treat_End Treatment seek_treat_Base $PERCHAR1 $HHCHAR1, cluster(village)
	est store Col_4
reg any_medexp_End Treatment any_medexp_Base $PERCHAR1 $HHCHAR1, cluster(village)
	est store Col_5
outreg2 [Col_1 Col_2 Col_3 Col_4 Col_5] using "$results/Table7b" ,  bdec(3) long se nonotes nocons nor /*
	*/ label word replace 

	
	// Robustness checks
tobit numIllness_End numIllness_Base Treatment $PERCHAR1 $HHCHAR1,  ll(0) ul(9) vce(cluster village)
mfx
poisson numIllness_End numIllness_Base Treatment $PERCHAR1 $HHCHAR1, cluster(village) 
mfx	
reg medexp_End medexp_Base Treatment $PERCHAR1 $HHCHAR1,  cluster(village) 


// These regressions report the Romano-Wolf p-values that appear in the supplementary appendix, Table S2.8b
rwolf anyIllness_End numIllness_End anydays_lost_End seek_treat_End any_medexp_End , indepvar(Treatment) controls($PERCHAR1 $HHCHAR1) bl(_Base) reps(1000) cluster(village) vce(cluster village) seed(20)
restore	
	


// Robustness checks	
	// Base specification, ie exclude control variables 
reg anyIllness_End Treatment anyIllness_Base , cluster(village)
reg numIllness_End Treatment numIllness_Base ,  cluster(village)
reg anydays_lost_End Treatment anydays_lost_Base , cluster(village)
reg seek_treat_End Treatment seek_treat_Base , cluster(village)
reg any_medexp_End Treatment any_medexp_Base , cluster(village)


// (1) Probit for dichotmous outcomes
dprobit anyIllness_End anyIllness_Base Treatment $PERCHAR1 $HHCHAR1, cluster(village)
dprobit seek_treat_End seek_treat_Base Treatment $PERCHAR1 $HHCHAR1, cluster(village)

// (2) Check for winsorizing
tobit days_lost_win days_lost_Base_win Treatment , cluster(village) ll(0)
tobit days_lost_win days_lost_Base_win Treatment $PERCHAR1 $HHCHAR1, cluster(village) ll(0)

tobit medexp_win medexp_Base_win Treatment , cluster(village) ll(0)
tobit medexp_win medexp_Base_win Treatment $PERCHAR1 $HHCHAR1, vce (cluster village) ll(0)

//(3) CLAD
//clad days_lost days_lost_Base Treatment , ll(0) dots /* For some reason, this does not always converge */
clad medexp medexp_Base Treatment , ll(0) dots
	
// (4) Effects conditional on any days lost?
tobit days_lost_End days_lost_Base Treatment $PERCHAR1 $HHCHAR1 if days_lost >0 , cluster(village) ll(1)


// SAMPLE DISAGGREGATIONS

// By sex 
reg anyIllness_End anyIllness_Base Treatment $PERCHAR1 $HHCHAR1 if sex ==1 , cluster(village) /* Women */
	est store BT1
reg anyIllness anyIllness_Base Treatment $PERCHAR1 $HHCHAR1 if sex ==0 , cluster(village) /* Men */
	est store BT2

// By age, these are some experiments. Other age disaggregations do not produce anything different
preserve
keep if age >15 & age <41 /* Prime age adults*/
reg anyIllness_End anyIllness_Base Treatment $PERCHAR1 $HHCHAR1, cluster(village)
restore

preserve
keep if age >40 /* Older adults*/
reg anyIllness_End anyIllness_Base Treatment $PERCHAR1 $HHCHAR1, cluster(village)
restore

// By relationship to household head
preserve
keep if relHead_1==1 /* HH head */
reg anyIllness_End anyIllness_Base Treatment $PERCHAR1 $HHCHAR1, cluster(village)
restore

preserve
keep if relHead_2==1 /* Spouse of head */
reg anyIllness_End anyIllness_Base Treatment $PERCHAR1 $HHCHAR1, cluster(village)
restore


