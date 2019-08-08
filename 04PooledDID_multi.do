set more off
cap log close
cd E:\JANUS的文档\STATA\ExportPDIFF

/*考虑多次获得补贴的情况*/
/*treatment sample selection*/
use Allvars,clear
bys id:egen m=max(SUBD)
keep if m==1
drop m
gsort id -year
rename SUBD SUBDori
gen SUBD=SUBDori
bys id:replace SUBD=SUBD[_n+1] if year-year[_n+1]==1&SUBD==0&SUBD[_n+1]==1 /*for year t+1*/
bys id:replace SUBD=SUBD[_n+1] if year-year[_n+1]==1&SUBD==0&SUBD[_n+1]==1 /*for year t+2*/
sort id year
drop if party_id==""|(SIZE==.|SIZE==0)
keep id year SUBDori SUBD
tab year
sort id year
bys id:keep if year-year[_n-1]==1 /*检查连续性*/

sort id year
bys id:egen max=max(SUBD)
bys id:egen min=min(SUBD)
drop if max==0|min==1
drop max min
bys id:gen c=_N
tab c  /*at least 1 observation in each pre-/post-period*/
drop c
tab SUBD
tab year
gen POST=SUBD
label variable POST "得到财政补贴后=1"
save treatment_multi,replace


/*pooled DID sample*/
use ctrpool,clear
gen treat=0
append using treatment_multi
keep id year treat POST
replace treat=1 if treat==.
replace POST=0 if POST==.
tab treat
tab POST
tab POST if treat==1
merge m:1 id using firmid,keepusing(firmid)
keep if _m==3
drop _m
order firmid year

/*merge with other variables*/
merge 1:1 id year using matchpanel,keepusing(party_id prodiff* NP PRICE PRODY ind prov comtype AGE)
drop if _m==2
drop _m
merge 1:1 id year using Allvars,keepusing(SIZE ROA LEV)
drop if _m==2
drop _m
merge 1:1 id year using qy_slim_TFP,keepusing(TFP_ols TFP_op)
drop if _m==2
drop _m
gen TFP=TFP_op
merge 1:1 id year using dvadata,keepusing(dva)
drop if _m==2
drop _m
gen DVA=dva 
merge 1:1 party_id year using newquality,keepusing(quality_qiv)
drop if _m==2
drop _m
gen QIV=quality_qiv
save maindata_multi,replace

use maindata_multi,clear
tab comtype
drop if comtype==99
gen PDIFF=prodiff2
egen miss=rowmiss(PDIFF TFP SIZE ROA LEV AGE)
drop if miss
drop miss
gen lnNP=ln(NP)
gen lnPRODY=ln(PRODY)
winsor2 prodiff* TFP* PDIFF NP lnNP PRICE PRODY lnPRODY QIV DVA SIZE ROA LEV AGE, cuts(1 99) replace
save maindatareg_multi,replace

/*Regression results----------------------------------------------*/
set more off
use maindatareg_multi,clear
eststo clear
eststo:reghdfe PDIFF    POST  TFP SIZE ROA LEV AGE   , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4 POST  TFP SIZE ROA LEV AGE   , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe NP       POST  TFP SIZE ROA LEV AGE   , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe PRODY    POST  TFP SIZE ROA LEV AGE   , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe QIV      POST  TFP SIZE ROA LEV AGE   , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe DVA      POST  TFP SIZE ROA LEV AGE   , absorb(firmid year) vce(cluster firmid)
esttab using SUBDmain_multi.rtf, ar2 replace star(* 0.1 ** 0.05 *** 0.01) compress nogaps onecell t(%8.2f) b(%8.4f) ///
addnote("(1)-(6)firmFE+yearFE+clustered by firm")


set more off
use maindatareg_multi,clear
eststo clear
eststo:reghdfe prodiff1   POST  TFP SIZE ROA LEV AGE   , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff2   POST  TFP SIZE ROA LEV AGE   , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff3   POST  TFP SIZE ROA LEV AGE   , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4   POST  TFP SIZE ROA LEV AGE   , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff1   POST  TFP SIZE ROA LEV AGE NP, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff2   POST  TFP SIZE ROA LEV AGE NP, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff3   POST  TFP SIZE ROA LEV AGE NP, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4   POST  TFP SIZE ROA LEV AGE NP, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff1   POST  ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff2   POST  ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff3   POST  ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4   POST  ,absorb(firmid year) vce(cluster firmid)
esttab using PDIFFmain_multi.rtf, ar2 replace star(* 0.1 ** 0.05 *** 0.01) compress nogaps onecell t(%8.2f) b(%8.4f) ///
addnote("(1)-(6)firmFE+yearFE+clustered by firm")

set more off
use maindatareg_multi,clear
eststo clear
eststo:reghdfe prodiff2   POST  TFP SIZE ROA LEV AGE if comtype==1, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff2   POST  TFP SIZE ROA LEV AGE if comtype==2, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff2   POST  TFP SIZE ROA LEV AGE if comtype==3, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff2   POST  TFP SIZE ROA LEV AGE if comtype==4, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff2   POST  TFP SIZE ROA LEV AGE if comtype==5, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4   POST  TFP SIZE ROA LEV AGE if comtype==1, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4   POST  TFP SIZE ROA LEV AGE if comtype==2, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4   POST  TFP SIZE ROA LEV AGE if comtype==3, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4   POST  TFP SIZE ROA LEV AGE if comtype==4, absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4   POST  TFP SIZE ROA LEV AGE if comtype==5, absorb(firmid year) vce(cluster firmid)
esttab using PDIFFmain_multi_comtype.rtf, ar2 replace star(* 0.1 ** 0.05 *** 0.01) compress nogaps onecell t(%8.2f) b(%8.4f) ///
addnote("(1)-(6)firmFE+yearFE+clustered by firm")
