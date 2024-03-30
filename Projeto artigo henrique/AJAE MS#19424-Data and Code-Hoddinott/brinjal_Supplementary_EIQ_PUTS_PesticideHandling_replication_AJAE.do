*brinjal_Supplementary_EIQ_PUTS_PesticideHandling_replication_AJAE.do
/* This do file replicates Table S1.2, S1.5 and S2.9*/

set more off
clear all  

// Setting up a path for the data file
global data = "C:\Users\NAbedin\Dropbox (IFPRI)\Naveen\Bt Brinjal\Bt Brinjal\AJAE\AJAE R&R\AJAE Files\September 24th 2020\Data Files\"
// Setting up a place to put the results
global results = "C:\Users\NAbedin\Dropbox (IFPRI)\Naveen\Bt Brinjal\Bt Brinjal\AJAE\AJAE R&R\AJAE Files\September 24th 2020\Results\"


//Baseline EIQ Statistics reported in Table S1.2//
use "$data\EIQ_Baseline.dta", clear
tabstat feiq_b ceiq_b weiq_b eceiq_b, stats(mean sd count) by(Treatment) long column(statistics) format (%6.2f)

//Endline EIQ Statistics reported in Table S1.2//
use "$data\EIQ_Endline.dta", clear
tabstat feiq_e ceiq_e weiq_e eceiq_e, stats(mean sd count) by(Treatment) long column(statistics) format (%6.2f)

//Baseline PUTS Statistics reported in Table S1.5//
use "$data\PUTS_Baseline.dta", clear
tabstat puts_bl, stats(mean sd min max count) by(Treatment) long column(statistics) format (%6.2f)

//Endline PUTS Statistics reported in Table S1.5//
use "$data\PUTS_Endline.dta", clear
tabstat puts_el, stats(mean sd min max count) by(Treatment) long column(statistics) format (%6.2f)

//Pesticide Handling Practices reported in Table S2.9//

use "$data\PesticideHandling.dta", clear

//Baseline Estimates
bysort Treatment: tab readlabel_bl
bysort Treatment: tab followinstr_bl
bysort Treatment: tab mixpesticide_barehands_bl
bysort Treatment: tab mixpesticide_gloves_bl
bysort Treatment: tab mixpesticide_stickbarehands_bl
bysort Treatment: tab mixpesticide_stickgloves_bl
bysort Treatment: tab spray_longsleeves_bl
bysort Treatment: tab spray_longtrousers_bl
bysort Treatment: tab spray_faceshield_bl
bysort Treatment: tab spray_headcover_bl
bysort Treatment: tab spray_eyeprotect_bl
bysort Treatment: tab spray_gloves_bl
bysort Treatment: tab spray_shoes_bl
bysort Treatment: tab spraydirection_bl
bysort Treatment: tab spraywindy_bl
bysort Treatment: tab washhands_bl
bysort Treatment: tab washface_bl
bysort Treatment: tab takebath_bl
bysort Treatment: tab changeclothes_bl

//Endline Estimates
bysort Treatment: tab readlabel_el
bysort Treatment: tab followinstr_el
bysort Treatment: tab mixpesticide_barehands_el
bysort Treatment: tab mixpesticide_gloves_el
bysort Treatment: tab mixpesticide_stickbarehands_el
bysort Treatment: tab mixpesticide_stickgloves_el
bysort Treatment: tab spray_longsleeves_el
bysort Treatment: tab spray_longtrousers_el
bysort Treatment: tab spray_faceshield_el
bysort Treatment: tab spray_headcover_el
bysort Treatment: tab spray_eyeprotect_el
bysort Treatment: tab spray_gloves_el
bysort Treatment: tab spray_shoes_el
bysort Treatment: tab spraydirection_el
bysort Treatment: tab spraywindy_el
bysort Treatment: tab washhands_el
bysort Treatment: tab washface_el
bysort Treatment: tab takebath_el
bysort Treatment: tab changeclothes_el




























