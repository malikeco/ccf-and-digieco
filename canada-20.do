*************************************************************************************************
*** This do file creates the regressions for 
*** Decarbonization in Canadian Energy Policy, the moderating role of construction and manufacturing industry: A Dynamic ARDL and Machine Learning Analysis.
*** Khadim Hussain, Shenzhen University 
*** Zhong Jian, Shenzhen University
*** Anwar Khan, Xiamen University
***Furman Ali
***Muhammad Arif
**************************************************************************************************
*** Published in Energy & Environemt
*****************************************************************************************************
clear, all
ssc install parmest
ssc install eclplot
ssc install dynardl

use "F:\2024 submissions\Canada\Revision\data.dta", replace
tsset year


*===============================================================================
****-Step 2 : TableA# Descriptive statistical results    -****
*===============================================================================

*--//Table 2 - Descriptive statistical//--*
sum lnco2 lnnrec lncons lnmfg lngdp con_mfg

logout,save(Table2-Summary) word dec(3) replace: tabstat lnco2 lnnrec lncons lnmfg lngdp con_mfg, stat(count mean sd min max) col(stat) format(%10.2f)

correlate lnco2 lnnrec lncons lnmfg lngdp
*--//Table 3 - Descriptive statistical//--* however kpss test is perform using R studio
*Table 3 ADF
dfuller lnco2
dfuller lnco2
dfuller d.lnco2
dfuller lnnrec
dfuller d.lnnrec
dfuller d.lncons
dfuller lncons
dfuller lnmfg
dfuller d.lnmfg
dfuller lngdp
dfuller d.lngdp

*** PP test Table 3
pperron lnco2
pperron d.lnco2
pperron lnnrec
pperron d.lnnrec
pperron d.lncons
pperron lncons
pperron lnmfg
pperron d.lnmfg
pperron lngdp
pperron d.lngdp
*** DF GLS test Table 3
dfgls lnco2
dfgls d.lnco2
dfgls lnnrec
dfgls d.lnnrec
dfgls lncons
dfgls d.lncons
dfgls lnmfg
dfgls d.lnmfg
dfgls lngdp
dfgls d.lngdp

*--//Table 4 - Lags specification//--*
varsoc lnco2 lnnrec lncons lnmfg lngdp, maxlag(2)


*--//Table 5 - Results of conventional ARDL //--*
ardl lnco2 lnnrec lncons lnmfg lngdp, maxlag(2 2 2 2 2) nocons ec1 regstore(res)
est store a1
esttab a?, b(%6.3f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab a? using Table5-Results-of_conventional_ARDL.rtf, replace b(%6.3f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap

*--//Table 6 - ARDL diagnostics tests //--*
**A: Bounds testing
estat ectest
**B: LM test for autocorrelation
estimates restore res
estat bgodfrey, lags(1/4) small
**C: IM-test
estat imtest, white
**E: Cumulative sum test for parameter stability
predict res1, residuals
** Fig 5 and section E of Table 6
estat sbcusum, ols
*Supplementary data Fig S1
pnorm res1
qnorm res1

**# Table 7 and Fig 7: Results of DYNARDL  #
dynardl lnco2 lnnrec lncons lnmfg lngdp, lags(1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1) shockvar(lnnrec) nocons ec shockval(-10)
est store t1
esttab
parmby "xi:dynardl lnco2 lnnrec lncons lnmfg lngdp, lags(1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1) shockvar(lnnrec) nocons ec shockval(-10) time(10) range(30) graph change sims(5000)", label norestore

**Fig 6
sencode parm, gene(parmid)
eclplot estimate min95 max95 parmid


**# Fig 8 
*a
dynardl lnco2 lnnrec lncons lnmfg lngdp, lags(1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1) shockvar(lncons) nocons ec shockval(10) time(10) range(30) graph sims(5000)
dynardl lnco2 lnnrec lncons lnmfg lngdp, lags(1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1) shockvar(lncons) nocons ec shockval(-10) time(10) range(30) graph  sims(5000)
*b
dynardl lnco2 lnnrec lncons lnmfg lngdp, lags(1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1) shockvar(lnmfg) nocons ec shockval(10) time(10) range(30) graph  sims(5000)
dynardl lnco2 lnnrec lncons lnmfg lngdp, lags(1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1) shockvar(lnmfg) nocons ec shockval(-10) time(10) range(30) graph  sims(5000)
*c
dynardl lnco2 lnnrec lncons lnmfg lngdp, lags(1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1) shockvar(lngdp) nocons ec shockval(10) time(10) range(30) graph  sims(5000)
dynardl lnco2 lnnrec lncons lnmfg lngdp, lags(1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1) shockvar(lngdp) nocons ec shockval(-10) time(10) range(30) graph  sims(5000)
*
**************************************************************************
***# Moderating Role of MFG and construction#2
***********************************************************************
*# Table 8. 
gen REC_MFG=lnnrec*lnmfg
egen ave_log_NREC=mean(lnnrec)
gen dc_log_NREC=lnnrec-ave_log_NREC
egen ave_log_CONS=mean(lncons)
gen dc_log_CONS=lncons-ave_log_CONS
gen NREC_CONS=dc_log_NREC*dc_log_CONS
dynardl lnco2 lnnrec lncons lnmfg lngdp REC_MFG, lags(1, 1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1, 1) shockvar(REC_MFG)  ec shockval(10) time(10) range(30) graph  sims(5000)
est store j1
dynardl lnco2 lnnrec lncons lnmfg lngdp NREC_CONS, lags(1, 1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1, 1) shockvar(NREC_CONS)  ec shockval(10) time(10) range(30) graph  sims(5000)
est store j2
esttab j?, b(%6.3f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab j? using Table--Results-of_ModeratingEffects.rtf, replace b(%6.3f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap


**************************************************************************
**# Analysis of Robustness #3
***********************************************************************
**# *//Fig  9 -  Conditional(geweke) frequency domain causality
bcgcausality lnco2 lnnrec lncons lnmfg lngdp, varlag(3) condtype(geweke)
bcgcausality lnco2 lncons lnnrec  lnmfg lngdp, varlag(3) condtype(geweke)
bcgcausality lnco2 lnmfg lncons lnnrec   lngdp, varlag(3) condtype(geweke)
bcgcausality lnco2 lngdp lnmfg lncons lnnrec   , varlag(3) condtype(geweke)
*Supplementary data Table S1 
var lnco2 lnnrec lncons lnmfg lngdp,lags(1/4) exog(l5.(lnnrec lncons lnmfg lngdp) trend)
vargranger

*--//Table 9  
* The results are obtained using R with the below codes
****
set.seed(654)

gkrls_est <- gam(lnCO2 ~ s(lnNREC,lnCONS, lnMFG, lnGDP, bs = "gKRLS"),
                  data = df )
summary(gkrls_est)
calculate_effects(gkrls_est, variables = "lnNREC", continuous_type = "minmax")
calculate_effects(gkrls_est, variables = "lnCONS", continuous_type = "minmax")
calculate_effects(gkrls_est, variables = "lnMFG", continuous_type = "minmax")
calculate_effects(gkrls_est, variables = "lnGDP", continuous_type = "minmax")

**# Table S2. : Robustness analysis
log using robustness, replace
qreg lnco2 lnnrec lncons lnmfg lngdp, quantile(.30) vce(iid, kernel(parzen) chamberlain)
est store a1
qreg lnco2 lnnrec lncons lnmfg lngdp, quantile(.75) vce(iid, kernel(parzen) chamberlain)
est store a2
qreg lnco2 lnnrec lncons lnmfg lngdp, quantile(.90) vce(iid, kernel(parzen) chamberlain)
est store a3
qreg lnco2 lnnrec lncons lnmfg lngdp, quantile(.50) vce(iid, kernel(parzen) chamberlain)
est store a4
sqreg lnco2 lnnrec lncons lnmfg lngdp, quantile(.20 ) reps(100)
est store a5
esttab a1 a2 a3 a4 a5 using Table-Robust_QREG.rtf, replace b(%6.3f) t(%6.2f) star(* 0.10 ** 0.05 *** 0.01) nogap
log close

*--//Table 10
**# with change in dpendent variable #2
gen lnEF=log(efconspercap)
gen co_EF= lnco2 *lnEF
dynardl lnEF lnnrec lncons lnmfg lngdp, lags(1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1) shockvar(lnnrec) nocons ec shockval(-10)
est store r2
dynardl co_EF lnnrec lncons lnmfg lngdp, lags(1, 1, 1, 1, 1) diffs(., 1, 1, 1, 1) shockvar(lnnrec) nocons ec shockval(-10)
est store r3
esttab r?, b(%6.3f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab r? using Table-Results-Robustness_withChange-Dependt.rtf, replace b(%6.3f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap

*************************************************************************************************************************************************************************************************************************************


dfuller efconspercap
pperron efconspercap
dfgls efconspercap
kpss efconspercap

dfuller co_EF
pperron co_EF
dfgls co_EF
kpss co_EF



dfuller lnEF 
pperron lnEF 
dfgls lnEF 
kpss lnEF 
kpss lnco2

**. Testing for Heteroskedasticity: 1. Heteroskedasticity Tests (Breusch-Pagan or White's Test)
// Null Hypothesis (H₀): Homoskedasticity (constant variance of the errors).
// Alternative Hypothesis (H₁): Heteroskedasticity (variance of the errors is not constant).If p-value < 0.05 (or your chosen significance level): Reject the null hypothesis, meaning there is evidence of heteroskedasticity in your model.
*Breusch-Pagan Test:
reg efconspercap lnnrec lncons lnmfg lngdp
estat hettest
*White's Test:
reg efconspercap lnnrec lncons lnmfg lngdp
estat imtest, white

*2. Testing for Serial Correlation: Null Hypothesis (H₀): No serial correlation (errors are not autocorrelated).
// Alternative Hypothesis (H₁): Serial correlation (errors are autocorrelated).
// Interpretation:
// If p-value < 0.05 (or your chosen significance level): Reject the null hypothesis, meaning there is evidence of serial correlation in your model.
*Breusch-Godfrey LM Test 
reg efconspercap lnnrec lncons lnmfg lngdp
estat bgodfrey, lags(2)
*Durbin-Watson Test
reg efconspercap lnnrec lncons lnmfg lngdp
estat dwatson

reg lnEF lnnrec lncons lnmfg lngdp




