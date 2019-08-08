set more off
cap log close
cd E:\JANUS的文档\STATA\ExportPDIFF

/*
/*added value of exports*/
use dva,clear
rename id party_id
rename firm_id id
duplicates drop id year,force 
save dvadata,replace

/*new quality data*/
use E:\JANUS的文档\Tools&Data\鲁晓东\LXD2017March\quality,clear
sort year party_id hs
bys year party_id:egen sumv=sum(value_year)
gen r=value_year/sumv
bys year party_id:egen quality_qiv=sum(qiv*r)
duplicates drop party_id year,force
keep party_id year quality_qiv
save newquality,replace
use newquality,clear
*/

/*pooled DID sample*/
use ctrpool,clear
gen treat=0
append using treatments1  /*补贴从无到有*/
keep id year treat POST firstx
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
merge 1:1 id year using qy_slim_TFP,keepusing(TFP_ols TFP_op) /*input dataset*/
drop if _m==2
drop _m
gen TFP=TFP_op
merge 1:1 id year using dvadata,keepusing(dva) /*input dataset*/
drop if _m==2
drop _m
gen DVA=dva 
merge 1:1 party_id year using newquality,keepusing(quality_qiv) /*input dataset*/
drop if _m==2
drop _m
gen QIV=quality_qiv
save maindata,replace

use maindata,clear
tab comtype
drop if comtype==99
gen PDIFF=prodiff2
egen miss=rowmiss(PDIFF TFP SIZE ROA LEV AGE)
drop if miss
drop miss
sort firmid year
bys firmid:egen maxy=max(year)
bys firmid:egen miny=min(year)
keep if treat==0|(firstx>miny&firstx<=maxy) /*requiring at least 1 obs. in both pre- and post-period*/
drop maxy maxy
gen lnNP=ln(NP)
gen lnPRODY=ln(PRODY)
winsor2 prodiff* TFP* PDIFF NP lnNP PRICE PRODY lnPRODY QIV DVA SIZE ROA LEV AGE, cuts(1 99) replace
save maindatareg,replace

use maindatareg,clear
logout, save(Descriptives) word replace: tabstat treat POST prodiff2 prodiff4 TFP SIZE ROA LEV AGE NP PRODY QIV DVA, statistics(count mean sd p25 median p75 min max) col(stat) f(%9.4f)

/*
/*descriptive statistics*/
use maindatareg,clear
pwcorr_a prodiff2 prodiff4 NP PRODY QIV DVA TFP SIZE ROA LEV AGE, star1(0.01) star5(0.05) star10(0.1)

sum PDIFF,detail
sum NP,detail
tabstat PDIFF, by(year) statistics(N mean sd p1 p25 p50 p75 p99) col(stat) f(%9.4f)

tab year
tab year if POST==1
tab treat
tab POST
tab POST if treat==1

ttest PDIFF,by(treat)
ttest PDIFF if treat==1,by(POST)
ttest NP,by(treat)
ttest NP if treat==1,by(POST)
ttest PRODY,by(treat)
ttest PRODY if treat==1,by(POST)
ttest QIV,by(treat)
ttest QIV if treat==1,by(POST)
ttest DVA,by(treat)
ttest DVA if treat==1,by(POST)


use maindatareg,clear
keep if treat==1&firstx==year
tab year /*第一次补贴的年度分布*/

*/

/*Regression results----------------------------------------------*/
set more off
use maindatareg,clear
eststo clear
eststo:reghdfe PDIFF    POST  TFP SIZE ROA LEV AGE , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4 POST  TFP SIZE ROA LEV AGE , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe NP       POST  TFP SIZE ROA LEV AGE , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe PRODY    POST  TFP SIZE ROA LEV AGE , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe QIV      POST  TFP SIZE ROA LEV AGE , absorb(firmid year) vce(cluster firmid)
eststo:reghdfe DVA      POST  TFP SIZE ROA LEV AGE , absorb(firmid year) vce(cluster firmid)
esttab using SUBDmain.rtf, ar2 replace star(* 0.1 ** 0.05 *** 0.01) compress nogaps onecell t(%8.2f) b(%8.4f) ///
addnote("(1)-(6)firmFE+yearFE+clustered by firm")


set more off
use maindatareg,clear
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
esttab using PDIFFmain.rtf, ar2 replace star(* 0.1 ** 0.05 *** 0.01) compress nogaps onecell t(%8.2f) b(%8.4f) ///
addnote("(1)-(6)firmFE+yearFE+clustered by firm")

set more off
use maindatareg,clear
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
esttab using PDIFFmain_comtype.rtf, ar2 replace star(* 0.1 ** 0.05 *** 0.01) compress nogaps onecell t(%8.2f) b(%8.4f) ///
addnote("(1)-(6)firmFE+yearFE+clustered by firm")

