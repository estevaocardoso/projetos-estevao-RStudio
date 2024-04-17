*--------------------------------------------------------------------------
* Código para a disciplina de Macroavançada II
* Exercício: Usando microdados para calcular misallocation a nível de firma
* Autor: Paul‹o
*--------------------------------------------------------------------------
clear all

*cd "/Volumes/GoogleDrive/My Drive/Macro Avancada - Firmas e Produtividade/Replicating Yasar et al"

cd "/Volumes/GoogleDrive/My Drive/Macro Avancada - Firmas e Produtividade/GIANT IITT - TFP and Development Course/Empirical Activities/Day 3 - TFPQ Estimation"

log using "log_MA2_2022.txt", text append

*use "Compustat_1995-2002(With age).dta", clear

use "Replication.dta"

*instalar opreg antes
*findit opreg
*instalar versão _2

* Seguindo o exercicío de Yasar et al (2008)

*set memory 96m
*use opreg
* Definindo variaveis 
destring gvkey, replace

xtset gvkey year

* Montando uma base comparável

destring sic, replace
keep if sic>=2000
keep if sic<=3999


* Entry and Exit
sort firmid year
by firmid: gen count = _N
gen survivor = count == 8
gen has95 = 1 if year == 2002
sort firmid has95
by firmid: replace has95 = 1 if has95[_n-1] == 1
replace has95 = 0 if has95 == .
sort firmid year
by firmid: gen has_gaps = 1 if year[_n-1] != year-1 & _n != 1
sort firmid has_gaps
by firmid: replace has_gaps = 1 if has_gaps[_n-1] == 1
replace has_gaps = 0 if has_gaps == .
sort firmid year
by firmid: generate exit = survivor == 0 & has95 == 0 & has_gaps != 1 & _n == _N
replace exit = 0 if exit == 1 & year == 2002

* Dropar casos estranhos:
drop if sale<=0
drop if emp <=0
drop if xrent<=0
drop if ppent<=0
drop if capx<=0

sum sale emp xrent ppent capx 

* Definindo variáveis

by firmid: gen lny = log(sale)
by firmid: gen t = group(year)
by firmid: gen lnkop = log(ppent)
by firmid: gen lnl = log(emp)
by firmid: gen lninv = log(capx)
by firmid: gen lnm = log(xrent)
* Just to follow the example I'll use another intermediate input
* They use eletricity I'll use advertising and R&D expenses just to test it
by firmid: gen lnExp = log(xad + xrd)
by firmid: gen LBprodty= sale/emp 
by firmid: gen lnLBprodty= log(sale/emp) 
* Weight - MKT share
bys sic year: egen sic_sale=total(sale)
bys firmid year: gen firmshare=sale/sic_sale
