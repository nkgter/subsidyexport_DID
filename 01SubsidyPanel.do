set more off
cap log close
cd D:\synnuts\Data&code_v20170709
/*
/*ownership type*/
use E:\JANUS的文档\STATA\HaiGuanData\hgdatall,clear
keep party_id year comtype
gsort party_id year -comtype
duplicates drop party_id year, force
sort party_id year
bys party_id:replace comtype=comtype[_n-1] if comtype==""&comtype[_n-1]!=""
rename comtype hgcomtype
tab hgcomtype
gen     comtype=99
replace comtype=1	if	hgcomtype=="国有企业"		
replace comtype=2	if	hgcomtype=="中外合资企业"|hgcomtype=="中外合作企业"
replace comtype=3	if	hgcomtype=="外商独资企业"		
replace comtype=4	if	hgcomtype=="集体企业"	
replace comtype=5	if	hgcomtype=="私营企业"
label define comtype_lab ///
         1 国有企业      ///
         2 中外合资      ///
         3 外商独资      ///
         4 集体企业      ///
         5 私营企业         
label value comtype comtype_lab
tab year  /*2000=2006*/
keep party_id year comtype
save comptype,replace

/*matchable panel data*/
use E:\JANUS的文档\STATA\HaiGuanGYQYData_New\Match\MatchablePanel,clear
merge m:1 party_id year using comptype,keepusing(comtype)
drop if _m==2
drop _m
sort id year
bys id:replace comtype=comtype[_n-1] if comtype==.&comtype[_n-1]!=.
replace comtype=99 if comtype==.   /*ownership type data not accurate待完善*/
merge 1:1 id year using E:\JANUS的文档\STATA\HaiGuanGYQYData_New\GYQY\gyqydatall
drop if _m==2
drop _m
destring idstryid,replace
gen ind=idstryid
gen prov=provcd
gen AGE=year-stupyea+1
replace AGE=0 if AGE==.|AGE<0
label variable ind "4位数行业代码"
label variable prov "省份代码"
label variable AGE "企业年龄自开业起算"
sort id year
tab year if sben!=.  /*2009 missing*/
tab year if sben>0&sben!=.
merge 1:1 cpycd year using E:\JANUS的文档\STATA\HaiGuanGYQYData_New\PDIFF\02PDIFF_ApproxN_match,keepusing(prodiff* NP)
drop if _m==2
drop _m
merge m:1 party_id year using E:\JANUS的文档\STATA\HaiGuanGYQYData_New\PDIFF\05prodquality_PRICE,keepusing(PRICE)
drop if _m==2
drop _m
merge m:1 party_id year using E:\JANUS的文档\STATA\HaiGuanGYQYData_New\PDIFF\06sophistication_PRODY,keepusing(PRODY)
drop if _m==2
drop _m
sort id year
duplicates drop id year,force
save matchpanel,replace
/*
use E:\JANUS的文档\STATA\ExportPDIFF\matchpanel,clear
*/
*/


/*********************************************/
*程序从这里开始运行,只需要4个数据集：/*input dataset*/
*(1)matchpanel.dta 
*(2)qy_slim_TFP.dta 
*(3)dvadata.dta 
*(4)newquality.dta 
/*********************************************/

/*unique firm id*/
use matchpanel,clear /*input dataset*/
keep id party_id
duplicates drop id,force
sort id
gen firmid=_n
save firmid,replace

/*define industrial policy and other control variables*/
use matchpanel,replace
keep id year obsgy party_id sben cptast totlast mopen crtlb acpabl ltlb totllb instp oppft totlpft incmtx
sum
tab year if acpabl!=.
label variable acpabl "应付账款[2004-2013]"
label variable ltlb "长期负债"

replace totlast=0 if totlast==.
gen SIZE=ln(totlast)
label variable SIZE "firm size=ln(total assets)"
replace oppft=0 if oppft==.
gen ROA=oppft/totlast
label variable ROA "=operating profit/total assets"
gen LEV=totllb/totlast
label variable LEV "=total liability/total assets"

replace incmtx=0 if incmtx==.
gen TAXR=incmtx/totlpft   /*所得税率取近三年的均值? refer to tax avoidance literature*/
label variable TAXR "=effective income tax rate"
replace instp=0 if instp==.
gen INTR=instp/totllb
label variable INTR "=effective debt interest rate"

gen positive=0
replace positive=1 if sben!=.&sben>0
tab positive  /*20% got positive subsidy*/
sum mopen,detail
gen subratio=sben/mopen if positive==1
replace subratio=0 if subratio==.
sum subratio,detail /*补贴率=补贴额/营业收入*/

gen SUBD=0
replace SUBD=1 if (subratio>=0.01&positive==1&sben>=100)|(positive==1&sben>=300)
tab SUBD           /*可观的标准：补贴率大于1%且补贴额不少于10万元；或者补贴额不少于30万元*/
label variable SUBD "subsidy dummy=可观的政府补贴"
save Allvars,replace

/*
/*按年计算出口企业收到可观补贴的公司数目*/
use Allvars,clear
drop if party_id==""|(SIZE==.|SIZE==0)
tab SUBD
tab year if SUBD==1
keep if SUBD==1
bys year:egen subttl=sum(sben)
duplicates drop year,force
*/




/*DID for government subsidy*/
/*control pool*/
use Allvars,clear
gen control=0
replace control=1 if sben==0|sben==. /*从未获得政府补贴*/
tab control
bys id:egen m=min(control)
drop if m==0
drop m
drop if party_id==""|(SIZE==.|SIZE==0)
keep id year
sort id year
bys id:drop if year-year[_n-1]>2 /*check continuity每年都有出口才会有出口产品相关指标*/
bys id:gen c=_N
tab c
keep if c>=5 /*requiring at least five years data for each firm*/
drop c
tab year
save ctrpool,replace

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
keep id year SUBD
tab year
sort id year
bys id:egen max=max(SUBD)
bys id:egen min=min(SUBD)
drop if max==0|min==1
drop max min
save temp,replace

/*补贴从无到有*/
use temp,clear
sort id year
bys id:egen miny=min(year) /*first year for each firm*/
gen x=year if SUBD==1
bys id:egen firstx=min(x)  /*first year SUBD=1*/
gen y=year if SUBD==0&year>firstx
bys id:egen firsty=min(y)  /*first year SUBD=0 in post subsidy period*/
drop x y
drop if miny==firstx
drop if year>=firsty /*firsty could be missing value*/
drop miny firsty
gen z=year if SUBD==0
bys id:egen last0=max(z)
gen gap=firstx-last0
keep if gap==1|gap==2 /*检查交界处的连续性(gap=1),允许1年空窗期(gap=2)*/
drop last0 z gap
gsort id -year
bys id:drop if year-year[_n-1]<-2&year[_n-1]<firstx /*检查补贴前期间的连续性*/
sort id year
bys id:drop if year-year[_n-1]>2&year[_n-1]>=firstx /*检查补贴后期间的连续性*/
bys id:egen max=max(SUBD)
bys id:egen min=min(SUBD)
drop if max==0|min==1
drop max min
bys id:gen c=_N
tab c  /*at least 1 observation in each pre-/post-period*/
tab SUBD
tab year
gen POST=SUBD
label variable POST "得到财政补贴后=1"
save treatments1,replace
