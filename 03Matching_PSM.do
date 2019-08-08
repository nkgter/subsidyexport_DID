clear
set more off
cap log close
cd E:\JANUS的文档\STATA\ExportPDIFF

/*preparing 3-year average data [-3,-1]*/
use maindata,clear
sort firmid year
gen PDIFF=prodiff4  /*or altrnatively = prodiff2*/
gen Y=PDIFF
keep firmid year Y TFP SIZE ROA LEV AGE
tset firmid year

set more off
local vlist TFP SIZE ROA LEV AGE Y
gen var=.
foreach v of local vlist{
replace var=`v'
bys firmid:gen avg3`v'=(L3.`v'+L2.`v'+L1.`v')/3   /*前三年平均值*/
replace avg3`v'=(L2.`v'+L1.`v')/2 if avg3`v'==.
replace avg3`v'=L1.`v' if avg3`v'==.
replace avg3`v'=`v' if avg3`v'==.
}
keep firmid year avg3* Y TFP SIZE ROA LEV AGE
save avg3data,replace


/*calculate propensity score*/
use maindata,clear
drop if treat==1&year!=firstx  /*only keep the year receiving subsidy*/
keep firmid year treat ind prov comtype
gen ind2=int(ind/100)
merge 1:1 firmid year using avg3data
keep if _m==3
drop _m
tab year if treat==1

egen miss=rowmiss(avg3*)
drop if miss
drop miss
tab year if treat==1 /*2002-2012*/
keep if year>=2002&year<=2012
winsor2 avg3*, cuts(1 99) replace
probit treat avg3* i.year i.ind2 i.prov i.comtype,vce(cluster firmid)
predict double pscore
sum pscore,detail
ttest pscore,by(treat)
save pscore,replace


/*matching process: pscore diff<cutoff and closest pscore*/
use maindata,clear
merge 1:1 firmid year using pscore,keepusing(pscore ind2)
keep if _m==3
drop _m
gen matchby1=pscore
gen cutoff1=0.10   /*difference in matchby1 less than 10%, if set it =. then matchby1 becomes useless*/
gen matchby2=pscore
drop if matchby1==.|matchby2==.
tab treat
gsort -treat year firmid
bys treat:gen matchid=_n if treat==1
replace matchid=0 if matchid==.
keep firmid year ind2 prov comtype treat pscore matchby* cutoff1 matchid
save temp03b,replace

/*base on treatment, to find the closest control*/
use temp03b,clear
gen ind=ind2
sort year ind prov comtype treat pscore  /*从pscore较小的treatment obs.开始进行匹配*/
bys year ind prov comtype treat:gen idt=_n if treat==1
bys year ind prov comtype treat:gen idc=_n if treat==0
bys year ind prov comtype:egen maxidc=max(idc)
bys year ind prov comtype:egen maxidt=max(idt)
drop if maxidt==.|maxidc==.
tab maxidt
sum maxidt maxidc /*max.idt = 162*/
drop maxidc maxidt idc

set more off
gen matched=0
forval j=2002/2012{
forval i=1/162{
gen value1=matchby1 if idt==`i'&year==`j'
bys year ind prov comtype:egen byvalue1=max(value1)
gen value2=matchby2 if idt==`i'&year==`j'
bys year ind prov comtype:egen byvalue2=max(value2)
gen id=matchid if idt==`i'&year==`j'
bys year ind prov comtype:egen byid=max(id)
gen diff1=abs(matchby1-byvalue1)/byvalue1
gen diff2=abs(matchby2-byvalue2)
gen ok=2
replace ok=1 if diff1<cutoff1&treat==0&matched==0
sort year ind prov comtype ok diff2
bys year ind prov comtype ok:replace matchid=byid if _n==1&ok==1
bys year ind prov comtype ok:gen getmatch=byid if _n==1&ok==1
bys year ind prov comtype:egen getmatchid=max(getmatch)
replace matched=1 if (matchid!=0&treat==0)|(matchid==getmatchid&treat==1)
bys firmid:egen max=max(matched)
drop if max==1&treat==0&matched==0  /*to avoid matched control firm been matched (by other year) again*/
drop value* byvalue* id byid diff* ok getmatch getmatchid max
}
}
tab matched
save matched,replace



/*check matching performance*/
use pscore,clear
drop if pscore==.
merge 1:1 firmid year using matched,keepusing(matched)
drop if _m==2
drop _m
tab matched
ta ind2,g(hy)
ta year,g(yrs)
ta prov,g(pid)
ta comtype,g(own)

set more off
eststo clear
eststo:probit treat avg3* hy* yrs* pid* own*,vce(cluster firmid)
eststo:probit treat avg3* hy* yrs* pid* own* if matched==1,vce(cluster firmid)
esttab using PSMatchperf.rtf, ar2 replace star(* 0.1 ** 0.05 *** 0.01) compress nogaps onecell t(%8.2f) b(%8.4f) drop(hy* yrs* pid* own*) ///
addnote("indFE+yearFE+provFE+ownershipFE+clustered by firm")

/*t-test table*/
estpost ttest avg3* pscore, by(treat)
esttab ., wide
estpost ttest avg3* pscore if matched==1, by(treat)
esttab ., wide


/*matched sample*/
use matched,clear
keep if matched==1
sort matchid
duplicates drop firmid,force /*should be no duplicates*/
keep firmid year treat matchid
gen eventyr=year

expand 7
sort matchid treat year
bys firmid:replace year=year+_n-4
sort matchid treat year
gen POST=0 if year<eventyr
replace POST=1 if year>=eventyr
gen POST_treat=POST*treat

tab POST
tab treat
tab treat POST

merge 1:1 firmid year using maindata, keepusing(prodiff* TFP* NP PRICE PRODY QIV DVA SIZE ROA LEV AGE comtype)
keep if _m==3
drop _m
save PSmatchDID,replace

/*PSmatched DID -------------------------------------------------------*/
set more off
use PSmatchDID,clear
gen PDIFF=prodiff2
egen miss=rowmiss(PDIFF TFP SIZE ROA LEV AGE)
drop if miss
drop miss
sort firmid year
bys firmid:egen maxy=max(year)
bys firmid:egen miny=min(year)
keep if eventyr>miny&eventyr<=maxy /*requiring at least 1 obs. in both pre- and post-period*/
drop maxy maxy
winsor2 prodiff* TFP* PDIFF NP PRODY QIV DVA SIZE ROA LEV AGE, cuts(1 99) replace
save PSmatchDIDreg,replace


use PSmatchDIDreg,clear
set more off
eststo clear
eststo:reghdfe PDIFF    POST POST_treat  TFP SIZE ROA LEV AGE ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4 POST POST_treat  TFP SIZE ROA LEV AGE ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe NP       POST POST_treat  TFP SIZE ROA LEV AGE ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe PRODY    POST POST_treat  TFP SIZE ROA LEV AGE ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe QIV      POST POST_treat  TFP SIZE ROA LEV AGE ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe DVA      POST POST_treat  TFP SIZE ROA LEV AGE ,absorb(firmid year) vce(cluster firmid)
esttab using PSMmatchDID.rtf, ar2 replace star(* 0.1 ** 0.05 *** 0.01) compress nogaps onecell t(%8.2f) b(%8.4f) ///
addnote("(1)-(6)firmFE+yearFE+clustered by firm")


use PSmatchDIDreg,clear
set more off
eststo clear
eststo:reghdfe prodiff1 POST POST_treat  TFP SIZE ROA LEV AGE   ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff2 POST POST_treat  TFP SIZE ROA LEV AGE   ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff3 POST POST_treat  TFP SIZE ROA LEV AGE   ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4 POST POST_treat  TFP SIZE ROA LEV AGE   ,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff1 POST POST_treat  TFP SIZE ROA LEV AGE NP,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff2 POST POST_treat  TFP SIZE ROA LEV AGE NP,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff3 POST POST_treat  TFP SIZE ROA LEV AGE NP,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4 POST POST_treat  TFP SIZE ROA LEV AGE NP,absorb(firmid year) vce(cluster firmid)
esttab using PDIFF_PSM.rtf, ar2 replace star(* 0.1 ** 0.05 *** 0.01) compress nogaps onecell t(%8.2f) b(%8.4f) ///
addnote("(1)-(6)firmFE+yearFE+clustered by firm")


/*univariate tests*/
use PSmatchDID,clear
gen PDIFF=prodiff4   /*=prodiff2 or prodiff4*/
gen Y=PDIFF
keep Y firmid matchid year treat eventyr POST
sort matchid year treat
bys matchid year:gen c=_N
drop if c!=2  /*keep balance btw treatment and control*/
drop c
sort matchid year treat
bys matchid year:gen tcdiff=Y[_n+1]-Y if _n==1
gen order=year-eventyr
tab order
winsor2 Y, cuts(1 99) replace

ttest   Y if treat==1,by(POST)
ranksum Y if treat==1,by(POST)
ttest   Y if treat==0,by(POST)
ranksum Y if treat==0,by(POST)
ttest   tcdiff ,by(POST)
ranksum tcdiff ,by(POST)

/*time trend for all restatements*/
ttest Y if order==-3,by(treat)
ttest Y if order==-2,by(treat)
ttest Y if order==-1,by(treat)
ttest Y if order==0,by(treat)
ttest Y if order==1,by(treat)
ttest Y if order==2,by(treat)





/*check parallel trend and examine the dynamic effects*/
use PSmatchDIDreg,clear
gen order=year-eventyr
tab order
/*
gen PL3=0      
replace PL3=1 if order==-3 /*benchmark period*/
*/
gen PL2=0
replace PL2=1 if order==-2
gen PL1=0
replace PL1=1 if order==-1
gen PN0=0
replace PN0=1 if order==0
gen PN1=0
replace PN1=1 if order==1
gen PN2=0
replace PN2=1 if order>=2

gen treat_PL2=treat*PL2
gen treat_PL1=treat*PL1
gen treat_PN0=treat*PN0
gen treat_PN1=treat*PN1
gen treat_PN2=treat*PN2

set more off
eststo clear
eststo:reghdfe prodiff2 treat_PL2 treat_PL1 treat_PN0 treat_PN1 treat_PN2 TFP SIZE ROA LEV AGE PL* PN*,absorb(firmid year) vce(cluster firmid)
eststo:reghdfe prodiff4 treat_PL2 treat_PL1 treat_PN0 treat_PN1 treat_PN2 TFP SIZE ROA LEV AGE PL* PN*,absorb(firmid year) vce(cluster firmid)
esttab using PSMmatchDID_dynamics.rtf, ar2 replace star(* 0.1 ** 0.05 *** 0.01) compress nogaps onecell t(%8.2f) b(%8.4f) ///
addnote("(1)-(6)firmFE+yearFE+clustered by firm")
