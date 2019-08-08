set more off
cap log close
cd D:\synnuts\Data&code_v20170709
/*
/*ownership type*/
use E:\JANUS���ĵ�\STATA\HaiGuanData\hgdatall,clear
keep party_id year comtype
gsort party_id year -comtype
duplicates drop party_id year, force
sort party_id year
bys party_id:replace comtype=comtype[_n-1] if comtype==""&comtype[_n-1]!=""
rename comtype hgcomtype
tab hgcomtype
gen     comtype=99
replace comtype=1	if	hgcomtype=="������ҵ"		
replace comtype=2	if	hgcomtype=="���������ҵ"|hgcomtype=="���������ҵ"
replace comtype=3	if	hgcomtype=="���̶�����ҵ"		
replace comtype=4	if	hgcomtype=="������ҵ"	
replace comtype=5	if	hgcomtype=="˽Ӫ��ҵ"
label define comtype_lab ///
         1 ������ҵ      ///
         2 �������      ///
         3 ���̶���      ///
         4 ������ҵ      ///
         5 ˽Ӫ��ҵ         
label value comtype comtype_lab
tab year  /*2000=2006*/
keep party_id year comtype
save comptype,replace

/*matchable panel data*/
use E:\JANUS���ĵ�\STATA\HaiGuanGYQYData_New\Match\MatchablePanel,clear
merge m:1 party_id year using comptype,keepusing(comtype)
drop if _m==2
drop _m
sort id year
bys id:replace comtype=comtype[_n-1] if comtype==.&comtype[_n-1]!=.
replace comtype=99 if comtype==.   /*ownership type data not accurate������*/
merge 1:1 id year using E:\JANUS���ĵ�\STATA\HaiGuanGYQYData_New\GYQY\gyqydatall
drop if _m==2
drop _m
destring idstryid,replace
gen ind=idstryid
gen prov=provcd
gen AGE=year-stupyea+1
replace AGE=0 if AGE==.|AGE<0
label variable ind "4λ����ҵ����"
label variable prov "ʡ�ݴ���"
label variable AGE "��ҵ�����Կ�ҵ����"
sort id year
tab year if sben!=.  /*2009 missing*/
tab year if sben>0&sben!=.
merge 1:1 cpycd year using E:\JANUS���ĵ�\STATA\HaiGuanGYQYData_New\PDIFF\02PDIFF_ApproxN_match,keepusing(prodiff* NP)
drop if _m==2
drop _m
merge m:1 party_id year using E:\JANUS���ĵ�\STATA\HaiGuanGYQYData_New\PDIFF\05prodquality_PRICE,keepusing(PRICE)
drop if _m==2
drop _m
merge m:1 party_id year using E:\JANUS���ĵ�\STATA\HaiGuanGYQYData_New\PDIFF\06sophistication_PRODY,keepusing(PRODY)
drop if _m==2
drop _m
sort id year
duplicates drop id year,force
save matchpanel,replace
/*
use E:\JANUS���ĵ�\STATA\ExportPDIFF\matchpanel,clear
*/
*/


/*********************************************/
*��������￪ʼ����,ֻ��Ҫ4�����ݼ���/*input dataset*/
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
label variable acpabl "Ӧ���˿�[2004-2013]"
label variable ltlb "���ڸ�ծ"

replace totlast=0 if totlast==.
gen SIZE=ln(totlast)
label variable SIZE "firm size=ln(total assets)"
replace oppft=0 if oppft==.
gen ROA=oppft/totlast
label variable ROA "=operating profit/total assets"
gen LEV=totllb/totlast
label variable LEV "=total liability/total assets"

replace incmtx=0 if incmtx==.
gen TAXR=incmtx/totlpft   /*����˰��ȡ������ľ�ֵ? refer to tax avoidance literature*/
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
sum subratio,detail /*������=������/Ӫҵ����*/

gen SUBD=0
replace SUBD=1 if (subratio>=0.01&positive==1&sben>=100)|(positive==1&sben>=300)
tab SUBD           /*�ɹ۵ı�׼�������ʴ���1%�Ҳ��������10��Ԫ�����߲��������30��Ԫ*/
label variable SUBD "subsidy dummy=�ɹ۵���������"
save Allvars,replace

/*
/*������������ҵ�յ��ɹ۲����Ĺ�˾��Ŀ*/
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
replace control=1 if sben==0|sben==. /*��δ�����������*/
tab control
bys id:egen m=min(control)
drop if m==0
drop m
drop if party_id==""|(SIZE==.|SIZE==0)
keep id year
sort id year
bys id:drop if year-year[_n-1]>2 /*check continuityÿ�궼�г��ڲŻ��г��ڲ�Ʒ���ָ��*/
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

/*�������޵���*/
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
keep if gap==1|gap==2 /*��齻�紦��������(gap=1),����1��մ���(gap=2)*/
drop last0 z gap
gsort id -year
bys id:drop if year-year[_n-1]<-2&year[_n-1]<firstx /*��鲹��ǰ�ڼ��������*/
sort id year
bys id:drop if year-year[_n-1]>2&year[_n-1]>=firstx /*��鲹�����ڼ��������*/
bys id:egen max=max(SUBD)
bys id:egen min=min(SUBD)
drop if max==0|min==1
drop max min
bys id:gen c=_N
tab c  /*at least 1 observation in each pre-/post-period*/
tab SUBD
tab year
gen POST=SUBD
label variable POST "�õ�����������=1"
save treatments1,replace
