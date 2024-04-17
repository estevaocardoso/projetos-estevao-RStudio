*-------------------------------------------------------------------------------
* Codigo para a disciplina de Macroavancada II
* Exercicio: Usando microdados para calcular misallocation
* estimar produtividade ao nivel de firma
* e para decompor produtividade agregada
* Projeto: Paper do Ano 
*-------------------------------------------------------------------------------

clear all

*change directory
cd "/Volumes/GoogleDrive/My Drive/Macro Avancada - Firmas e Produtividade/Replication_Oberfield"

log using "log_MA2_Misallocation_(PAULO).txt", text append

* Abrir a base
use "Replication.dta"

* Configurar o painel, informando o ID e a Variavel
xtset gvkey year

* Limpeza de dado para calcular produtividade e misallocation

* Tirando setores como setor financeiro
destring sic, replace
keep if sic>=2000 & sic<=3999

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

* Limpeza de casos estranhos
drop if sale<=0 | missing(sale)
drop if emp <=0 | missing(emp)
drop if xrent<=0 | missing(xrent)
drop if ppent<=0 | missing(ppent)
drop if capx<=0 | missing(capx)

* Rodar sumário para ter primeira impressão dos dados

summarize sale emp xrent ppent capx exit

* Definir variáveis para regressões e fazer sumário mais organizado
gen lny = log(sale)
gen t = group(year)
gen lnkop = log(ppent)
gen lnl = log(emp)
gen lninv = log(capx)
gen lnm = log(xrent)
* Just to follow the example I'll use another intermediate input
* They use eletricity I'll use advertising and R&D expenses just to test it
gen lnExp = log(xad + xrd)
gen LBprodty= sale/emp 
gen lnLBprodty= log(sale/emp) 
* Weight - MKT share
bys sic year: egen sic_sale=total(sale)
bys firmid year: gen firmshare=sale/sic_sale

/*** Nomeando as variaveis ***/ 
label variable age "Age" 
label variable t "Trend"
label variable lnkop "Capital"
label variable lnl "Labor"
label variable lnm "Materials"
label variable lninv "Investiment"
label variable lny "Revenue"




* Desafio 2023: Replicar Bills, Klenow & Ruane 












********************************************************************************
* Exemplo de exercício empírico
********************************************************************************

* Rodando OLS por setor 

* Função Produção: Y_ist = A_ist*Ageˆb1*lnk^b2*lnl^b3*lnm^b4 
* lny= A + b1 Age + b2lnK + b3lnl + b4lnm  (Y Age lnk lnl lnm são observaveis, falta conhcer os betas para chegar a A)
* Com os betas estimados para cada setor A(estimado) = Y estimado - b1estimado*age...

* Criar uma variável de setor própria (que seja disscreta pulando de um em um)
egen sector=group(sic)
sum sector

* Gerar variável lnOLSTFP que será substituida nos loops setoriais

gen lnOLSTFP=.

* Gerando o loop
* para evitar problemas com setores com poucas empresas ou observações
* Vamos censurar para setores com mais de 50 observações
forvalues i=1(1)218{
sum lny if sector==`i'
local m = r(N)
if `m'>50{
reg lny age lnkop lnl lnm t if sector==`i'
replace lnOLSTFP= lny - _b[age]*age - _b[lnkop]*lnkop -_b[lnl]*lnl - _b[lnm]*lnm - _b[t]*t if sector==`i' 
histogram lnOLSTFP if sector==`i', kdensity graphregion(color(white)) /*** Distribuicao de produtividade geral ***/
graph export "Histogram_OLSTFP_sector_`i'.png", replace
}
}

* Gerando distribuições de produtividade

histogram lnOLSTFP if lnOLSTFP!=., kdensity

* Tirando os outliers (Trimming)
quietly sum lnOLSTFP, detail
gen filter=(lnOLSTFP>r(p1) & lnOLSTFP<r(p99))
gen filter2=(lnOLSTFP>r(p25) & lnOLSTFP<r(p75))

* Histogram sem outliers

histogram lnOLSTFP if filter==1, kdensity

histogram lnOLSTFP, kdensity graphregion(color(white)) /*** Distribuicao de produtividade geral ***/
graph export "Histogram_OLSTFP.pdf", replace

histogram lnOLSTFP if filter==1, kdensity by(year) graphregion(color(white))









* 1o Desafio: Adaptar o código do Oberfield para calcular os wedges e o misallocation de cada ano a partir do compustat

* Objetivo: Calcular o TFPR para comparar com OLSTFP

gen sigma = 3

***gerando salário médio entre as empresas que apresentaram valores
gen W=xlr/emp

*** gerando salário médio por setor
gen wmed =.

forvalues i=1(1)218{           
quietly sum W if sector==`i'
replace wmed = r(mean) if sector == `i'
}


*** Gerando gasto com labor a nível de firmas
* poderia usar labor compensation direto
gen labx = wmed*emp
label variable labx "Labor Expenditures" 

**********************************************************
*** Generate Plant Level Production Function Parameters ***
***********************************************************

* Compute Plant Specific Capital Intensity
	bysort firmid year: egen K_expenditure = total(capx)
	bysort firmid year: egen L_expenditure = total(labx)
	bysort firmid: egen KLterm = median(ln(K_expenditure/L_expenditure))
	gen aki = 1 / (1 + exp(-KLterm))
	gen ali = 1 - aki
	drop K_expenditure L_expenditure KLterm


*** Agregados por setor***

egen industryyeartag = tag(sector year)

sort year sector
by year sector: egen PYs = total(sale)
by year sector: egen WLs = total(labx)
by year sector: egen Ks = total(ppent)

	
*** Calculando als e ali resolvendo a condição de ótimo da eq. 7***

* For each industry-year, solve for the \alpha*_s using a grid search on [0,1] 
* to find the value that comes closest to solving equation (7).
* The variable testsum_i is the RHS of equation (7) for \alpha*_s = i/100.
	


bysort year sector: gen Ns = _N

gen Kterm = (Ks * aki / ppent ) ^ (aki * (sigma-1)) 
gen Lterm = (WLs * ali / labx ) ^ (ali * (sigma-1))
gen Pterm = (sale/PYs *Ns) ^ sigma
gen Coef = Kterm*Lterm * Pterm
	
preserve
	forvalues i = 0(1)100 {
	local a = `i'/100
	bysort year sector: egen testsum`i' = total((Coef * (`a'^aki * (1-`a')^ali )^(1-sigma) ) * (aki - `a'))
	}

	keep if industryyeartag
	quietly reshape long testsum, i(sector year) j(index)



* Find the value of $\alpha*_s that solves equation (7).
* It is found by taking the midpoint of alpha*_s for which testsum_i is just above and just below zero.
sort year sector index
by year sector: gen aks = (testsum[_n-1] > 0) * (testsum < 0) * (index + index[_n-1])/2 / 100
keep if aks > 0
drop if aks == .
keep year sector aks

* Merge back in with data
tempfile industrysharetemp1
save "`industrysharetemp1'", replace
restore
quietly merge m:1 sector year using "`industrysharetemp1'"
drop _merge
gen als = 1 - aks

* With industry factor shares, compute capital and labor wedges. 
gen TKi = (aki * sale / ppent)  / (aks * PYs / Ks)
gen TLi = (ali * sale / labx)  / (als * PYs / WLs)

* Winsorize

sort year
*by year: egen TKiMax = pctile(TKi), p(95)
*by year: egen TKiMin = pctile(TKi), p(5)

*by year: egen TLiMax = pctile(TLi), p(95)
*by year: egen TLiMin = pctile(TLi), p(5)

*replace TKi = min(max(TKi, TKiMin), TKiMax)
*replace TLi = min(max(TLi, TLiMin), TLiMax)

quietly sum TKi, detail 
gen filterTKi=(TKi>r(p1) & TKi<r(p99))
gen TKi_temp2= TKi if filterTKi==1

quietly sum TLi, detail 
gen filterTLi=(TLi>r(p1) & TLi<r(p99))
gen TLi_temp2= TLi if filterTLi==1

replace TKi=TKi_temp2
replace TLi=TLi_temp2

gen TFPRi = sale / ((ppent^aki)*(emp^ali))

quietly sum TFPRi, detail 
gen filterTFPRi=(TFPRi>r(p1) & TFPRi<r(p99))
gen TFPRi_temp2= TFPRi if  filterTFPRi==1
replace TFPRi=TFPRi_temp2

histogram TFPRi if filterTFPRi==1, kdensity graphregion(color(white)) /*** Distribuicao de produtividade por ano ***/
histogram TFPRi if filterTFPRi==1, kdensity by(year) graphregion(color(white)) /*** Distribuicao de produtividade por ano ***/
histogram TKi, kdensity by(year) graphregion(color(white)) /*** Distribuicao de TKi por ano ***/
histogram TLi, kdensity by(year) graphregion(color(white)) /*** Distribuicao de TLi por ano ***/

gen lnTFPRi = log(TFPRi)
gen lnTKi = log(TKi)
gen lnTLi = log(TLi)

twoway (kdensity lnTFPRi if filterTFPRi==1)(kdensity lnOLSTFP if filter==1), by(year)

*********************************************************************************

gen TFPAgg = f (y lkop lmnt ... )
gen TFPAgg2 = f(y_otimo lkop_otimo lmn_otimo ... ) 
* Por definição TFPAgg2 > TFPAgg por há dispersão de TFPRi

log close


