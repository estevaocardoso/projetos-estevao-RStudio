*yield_cost_profit_tables_replication_AJAE

clear all
use "C:\Bangla_btbrinjal\AJAE\Revision2\Replication\data\impact_compiled data_yield_AJAE.dta"



global HHCHAR1 yearsedu_brinjalgrwer age_brinjalgrwer years_farmer wealthV1 opland /* Control variables */


// Code for Table 3, columns (1), (2), (3), (4) and (5)
reg net_yieldHA     Treatment net_yieldHA_b , cluster(village)
	est store Yield1
reg net_yieldHA     Treatment net_yieldHA_b $HHCHAR1 , cluster(village)
	est store Yield2
reg net_yieldHA_win Treatment net_yieldHA_b_win $HHCHAR1 , cluster(village)
	est store Yield3
reg lnet_yieldHA    Treatment lnet_yieldHA_b $HHCHAR1 , cluster(village)
	est store Yield4
reg net_yieldHA_IHS Treatment net_yieldHA_IHS_b $HHCHAR1 , cluster(village)
	est store Yield5
*outreg2 [Yield1 Yield2 Yield3 Yield4 Yield5] using "$results/net_yield_July2020" ,  bdec(2) sdec(2) long se nonotes nor /*
	**/ label word replace 


// Code for Table 4, columns (1), (2), (3), (4) and (5)
//Mechanisms
global HHCHAR1 yearsedu_brinjalgrwer age_brinjalgrwer years_farmer wealthV1 opland /* Control variables */

reg harvested           Treatment  harvested_b          $HHCHAR1 , cluster(village)
	est store BT1
reg lost                Treatment  lost_b               $HHCHAR1 , cluster(village)
	est store BT2
reg paid_out            Treatment  paid_out_b           $HHCHAR1 , cluster(village)
	est store BT3
reg retained            Treatment  retained_b           $HHCHAR1 , cluster(village)
	est store BT4
*outreg2 [BT1 BT2 BT3 BT4] using "$results/yield_mechanisms_July2020" ,  bdec(2) sdec(2) long se nonotes nor /*
*	*/ label word replace 

// I do this last regression separately because I need to show four decimal places
reg brinjal_plot_areaHA_e Treatment  brinjalarea_hectare  $HHCHAR1 , cluster(village)
	est store BT5
*outreg2 [BT5] using "$results/yield_mechanisms_July2020" ,  bdec(4) sdec(4) long se nonotes nor /*
*	*/ label word append 

// Code for Romano-Wolf multiple hypothesis test: Table S2.1

rwolf harvested lost paid_out retained , indepvar(Treatment) controls($HHCHAR1) bl(_b) reps(1000) cluster(village) vce(cluster village) seed(20)



//Code for Table 5
clear all
use "C:\Bangla_btbrinjal\AJAE\Revision2\Replication\data\Mechanisms_Table_5_AJAE.dta"

global HHCHAR1 yearsedu_brinjalgrwer age_brinjalgrwer years_farmer wealthV1 opland /* Control variables */

reg sold       Treatment sold_b $HHCHAR1 , cluster(village)
	est store Mech1
reg unit_price Treatment unit_price_b $HHCHAR1 , cluster(village)
	est store Mech2
reg revenue    Treatment  revenue_baseline $HHCHAR1 , cluster(village)
	est store Mech3
reg cashcost   Treatment cashcost_baseline $HHCHAR1 , cluster(village)
	est store Mech4
reg net_rev    Treatment net_rev_baseline $HHCHAR1 , cluster(village)
	est store Mech5
*outreg2 [Mech1 Mechd2 Mechd3 Mech4] using "$results/Mechanisms_Table5_July2020" ,  bdec(2) sdec(2) long se nonotes nor /*
	**/ label word replace 

// Code for Romano-Wolf multiple hypothesis test: Table S2.5
rwolf sold unit_price revenue cashcost net_rev , indepvar(Treatment) controls($HHCHAR1) bl(_baseline) reps(1000) cluster(village) vce(cluster village) seed(20)




//Figure S2.1. 
clear all
use "C:\Bangla_btbrinjal\AJAE\Revision2\Replication\data\impact_compiled data_yield_AJAE.dta"

g ln_yieldC = lnet_yieldHA  if Treatment ==0
g ln_yieldT = lnet_yieldHA  if Treatment ==1
lab var ln_yieldC "log net yield, Control"
lab var ln_yieldT "log net yield, Bt brinjal"

twoway kdensity ln_yieldT || kdensity ln_yieldC


//Table S2.2

*****Endline
clear all
use "C:\Bangla_btbrinjal\AJAE\Revision2\Replication\data\Supplementary_Table_S2_2.dta", 

tab h10_01_s1  Treatment , nof col   /* Who is the main buyer */                       
tab	h10_03_s1  Treatment ,	nof	col   /* Why this buyer */                
tab h10_10_s1  Treatment , nof col   /* Location of sale */



//Table S2.3. : Input costs at endline
clear all
use "C:\Bangla_btbrinjal\AJAE\Revision2\Replication\data\cost and revenue_cash cost basis_endine.dta", 
tabstat seedl_cost_ha fertilizerx_ha irr_total_ha pesticidex_ha machinex_ha hlcost_ha cashcost_ha , by (Treatment) stat (mean n)




//Table S2.4. : Labor use
clear all
use "C:\Bangla_btbrinjal\AJAE\Revision2\Replication\data\r2_plot size and irrigation.dta"
merge m:1 hhid plotid using "C:\Bangla_btbrinjal\AJAE\Revision2\Replication\data\r2_family labor days.dta"
keep if _m==3
drop _m

merge m:1 hhid using "C:\Bangla_btbrinjal\AJAE\Revision2\Replication\data\final sample_r1 r2.dta"
keep if _m==3
drop _m
keep if variety==1 |variety==2
*landp_m landp_f trans_m trans_f fert_m fert_f pest_m pest_f weed_m weed_f irrig_m irrig_f harv_m harv_f sort_m sort_f uproot_m uproot_f male_labd female_labd fam_labd

gen male_labdha= male_labd*247.1/plotarea
gen female_labdha= female_labd*247.1/plotarea
gen fam_labdha= fam_lab*247.1/plotarea
bys village_type: sum male_labdha female_labdha fam_labdha

gen landp_mha=landp_m*247.1/plotarea
gen landp_fha=landp_f*247/plotarea

gen trans_mha=trans_m*247.1/plotarea
gen trans_fha=trans_f*247.1/plotarea

gen fert_mha=fert_m*247.1/plotarea
gen fert_fha=fert_f*247.1/plotarea

gen pest_mha=pest_m*247.1/plotarea
gen pest_fha=pest_f*247.1/plotarea

gen weed_mha=weed_m*247.1/plotarea
gen weed_fha=weed_f*247.1/plotarea

gen irrig_mha=irrig_m*247.1/plotarea
gen irrig_fha=irrig_f*247.1/plotarea

gen harv_mha=harv_m*247.1/plotarea
gen harv_fha=harv_f*247.1/plotarea

gen sort_mha=sort_m*247.1/plotarea
gen sort_fha=sort_f*247.1/plotarea

gen uproot_mha=uproot_m*247.1/plotarea
gen uproot_fha=uproot_f*247.1/plotarea

bys village_type: sum landp_mha landp_fha trans_mha trans_fha fert_mha fert_fha pest_mha pest_fha weed_mha weed_fha irrig_mha irrig_fha harv_mha harv_fha sort_mha sort_fha uproot_mha uproot_fha


use "C:\Users\NZHOSSAIN\Desktop\Bt brinjal_all shared\BT Brinjal_19.8.2019\generated file_WQ work\r2_plot size and irrigation.dta", clear
merge m:1 hhid plotid using "C:\Users\NZHOSSAIN\Desktop\Bt brinjal_all shared\BT Brinjal_19.8.2019\generated file_WQ work\r2_hired labor mandays and cost.dta"
keep if _m==3
drop _m

merge m:1 hhid using "C:\Users\NZHOSSAIN\Desktop\Bt brinjal_all shared\BT Brinjal_19.8.2019\generated data\final sample_r1 r2.dta"
keep if _m==3
drop _m
keep if variety==1 |variety==2

*hlday_m hlday_f hlday

gen hlday_mha=hlday_m*247.1/plotarea
gen hlday_fha=hlday_f*247.1/plotarea
gen hldayha=hlday*247.1/plotarea
bys village_type: sum hlday_mha hlday_fha hldayha

gen hlandp_mha=hlandp_m*247.1/plotarea
gen hlandp_fha=hlandp_f*247/plotarea

gen htrans_mha=htrans_m*247.1/plotarea
gen htrans_fha=htrans_f*247.1/plotarea

gen hfert_mha=hfert_m*247.1/plotarea
gen hfert_fha=hfert_f*247.1/plotarea

gen hpest_mha=hpest_m*247.1/plotarea
gen hpest_fha=hpest_f*247.1/plotarea

gen hweed_mha=hweed_m*247.1/plotarea
gen hweed_fha=hweed_f*247.1/plotarea

gen hirrig_mha=hirrig_m*247.1/plotarea
gen hirrig_fha=hirrig_f*247.1/plotarea

gen hharv_mha=hharv_m*247.1/plotarea
gen hharv_fha=hharv_f*247.1/plotarea

gen hsort_mha=hsort_m*247.1/plotarea
gen hsort_fha=hsort_f*247.1/plotarea

gen huproot_mha=huproot_m*247.1/plotarea
gen huproot_fha=huproot_f*247.1/plotarea

bys village_type: sum hlandp_mha hlandp_fha htrans_mha htrans_fha hfert_mha hfert_fha hpest_mha hpest_fha hweed_mha hweed_fha hirrig_mha hirrig_fha hharv_mha hharv_fha hsort_mha hsort_fha huproot_mha huproot_fha

