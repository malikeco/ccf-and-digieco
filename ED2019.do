*===============================================================================
****-  Step 1 : load data and set variables  -****
*===============================================================================

clear all
cd "F:\Research 2023-24\China cofinancing and DE\data and code"
use DE2019.dta
xtset ID YEAR

gen log_CCF=ln(CCF)
replace log_CCF=0 if missing(log_CCF)
gen log_CF=ln(cf)
replace log_CF=0 if missing(log_CF)
gen log_NCF=ln(ncf)
replace log_NCF=0 if missing(log_NCF)
gen log_AI_stock=ln(AI_STOCK)
replace log_AI_stock = 0 if missing(log_AI_stock)
gen log_ET=ln(ET)
replace log_ET = 0 if missing(log_ET)
gen log_URP=ln(URP)
gen log_GDP=ln(GDP)
// gen lag_ET=L.log_ET
gen log_FDI = ln(abs(FDI))
replace log_FDI =0 if missing(log_FDI)
*===============================================================================
****-Step 2 : TableA# Descriptive statistical results    -****
*===============================================================================

*--//Table A2 - Descriptive statistical//--*

logout,save(TableA2-Summary) word dec(3) replace: tabstat digieco log_CCF log_CF log_NCF  log_URP log_GDP log_FDI log_ET log_AI_stock , stat(count mean sd min max) col(stat) format(%10.2f)


*--//Table A3 - Correlation matrix among variables//--*

* Calculate the correlation matrix
pwcorr digieco log_CCF log_URP log_CF log_NCF log_GDP log_FDI log_ET log_AI_stock, star(1)


*===============================================================================
****-Step 3 : empirical results    -****
*===============================================================================

*--//Table 2 - Benchmark model//--*
 
reghdfe digieco log_CCF, absorb(ID) 
est store a1
reghdfe digieco log_CCF log_URP, absorb(ID) 
est store a2 
reghdfe digieco log_CCF log_URP log_FDI, absorb(ID)  
est store a3
reghdfe digieco log_CCF log_URP log_FDI log_ET, absorb(ID)  
est store a4
reghdfe digieco log_CCF log_URP log_FDI log_ET log_GDP, absorb(ID)   
est store a5
reghdfe digieco log_CF log_NCF log_URP log_FDI log_ET log_GDP, absorb(ID)   
est store a6
esttab a?, b(%6.3f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab a? using Table22-Benchmark.rtf, replace b(%6.3f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap


**# Table 3 Heterogeneity analysis: different social development,different regions.  #1



reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if BRI==1,absorb(ID)
est store b1
reghdfe digieco log_CCF log_URP log_FDI  log_GDP log_ET if BRI==0,absorb(ID)
est store b2
reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if INCOME==1,absorb(ID)
est store b3
reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if INCOME==2,absorb(ID)
est store b4
reghdfe digieco log_CCF log_URP log_FDI  log_GDP log_ET if INCOME==3,absorb(ID)
est store b5
reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if CONTINENT2==1,absorb(ID)
est store b6
reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if CONTINENT2==2,absorb(ID)
est store b7
reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if CONTINENT2==3,absorb(ID)
est store b8
reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if CONTINENT2==4,absorb(ID)
est store b9
esttab b?, b(%6.3f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab b? using Table3-Heterogeneity.rtf, replace b(%6.3f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap

*--//Table 4- Heterogeneity analysis: country characteristics//--*
*sdg shows the daviation of country 2024 score from the group mean. as SD country i- group average of sd core 2024

reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if SDG>0 ,absorb(ID)
est store f1
reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if SDG<0,absorb(ID)
est store f2
egen meian_NDGAIN = median(NDGAIN)
reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if NDGAIN>meian_NDGAIN ,absorb(ID)
est store f3
reghdfe digieco log_CCF log_URP log_FDI  log_GDP log_ET if NDGAIN<meian_NDGAIN,absorb(ID)
est store f4
egen meian_CO2020 = median(CO2020)
reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if CO2020>meian_CO2020,absorb(ID)
est store f5
reghdfe digieco  log_CCF log_URP log_FDI  log_GDP log_ET if CO2020<meian_CO2020,absorb(ID)
est store f6
esttab f?, b(%6.3f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab f? using Table4-Heterogeneity5.rtf, replace b(%6.3f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap



*--//Table 5 - Robustness tests for endogeneity issues//--* With  MM qantile regression 
****
mmqreg digieco log_CCF, absorb(ID) nols q(75)
est store g1
mmqreg digieco log_CCF log_URP, absorb(ID) nols q(75)  
est store g2
mmqreg digieco log_CCF log_URP log_FDI, absorb(ID)  nols q(75)
est store g3
mmqreg digieco log_CCF log_URP log_FDI log_ET, absorb(ID)  nols q(75)
est store g4
mmqreg digieco log_CCF log_URP log_FDI  log_ET log_GDP, absorb(ID)  nols q(75)
est store g5
mmqreg digieco log_CF log_NCF log_URP log_FDI  log_ET log_GDP, absorb(ID)  nols q(75)
est store g5
esttab g1 g2 g3 g4 g5 using Table5-Robust_MMQREG.rtf, replace b(%6.3f) t(%6.2f) star(* 0.10 ** 0.05 *** 0.01) nogap



*--//Table 6 -  Moderating role of the  AI
****
egen ave_log_CCF=mean(log_CCF)
gen dc_log_CCF=log_CCF-ave_log_CCF
gen log_AI_install=ln(AI_INSTAL)
replace log_AI_install =0 if missing(log_AI_install)
egen ave_AI=mean(log_AI_install)
gen dc_AI=log_AI_install-ave_AI
gen CCF_AI=dc_log_CCF*dc_AI

reghdfe digieco dc_log_CCF CCF_AI log_AI_install log_URP log_FDI  log_ET log_GDP, absorb(ID) 
est store g1 
reghdfe digieco log_CF log_NCF CCF_AI log_AI_install log_URP log_FDI  log_ET log_GDP, absorb(ID)  
est store g2
esttab g?, b(%6.3f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab g? using Table6-Moderating_Role_AI.rtf, replace b(%6.3f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap
************************************************************************************************************
 





























