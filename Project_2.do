set more off
set matsize 10000

use "C:\Users\Locky\Documents\STATA Assignment\Project 2/foxnews.dta", clear

*Calculate voteshare variables
gen RvoteShare96 = reppresvotes1996 / demreppresvotes1996
gen RvoteShare00 = reppresvotes2000 / demreppresvotes2000

label variable RvoteShare96 "Republican Vote Share 1996"
label variable RvoteShare00 "Republican Vote Share 2000"

*Calculate change in republican vote share 1996 to 2000
gen GOPChange = RvoteShare00 - RvoteShare96
label variable GOPChange "Republican Vote Share Change"

*Descriptive Statistics foreach var of varlist county_n*{ bysort: state_n egen `var'_RvoteShare00 = reppresvotes2000 / demreppresvotes2000*


*Looking at the first researcher, lets compare vote share in towns with fox News to those without*
histogram(GOPChange)
, by(foxnews2000)
summarize GOPChange
mean GOPChange if foxnews2000 == 1
mean GOPChange if foxnews2000 == 0
reg GOPChange foxnews2000, robust
est store D
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

*Looking at the second researcher*
gen ln_poptot = log(poptot2000)
gen ln_sub = log(sub2000)
xtile chandec = nochannels, nq(10)
xtile subdec = sub2000, nq(10)
xtile popdec = poptot2000, nq(10)
label variable chandec "No. of Channels by Decile"
label variable subdec "Cable Subscribers by Decile"
label variable popdec "Potential Cable Subscribers by Decile"


local controlscable nochannels chandec popdec
reg GOPChange foxnews2000 `controlscable', robust
est store F
coefplot (D, label(First Researcher) pstyle(p3)) (F, label(Second Researcher) pstyle(p4)), msymbol(S) drop(_cons) xline(0) coeflabel(nochannels = "No. of Channels" poptot2000 = "Potential Subscribers", wrap(20)) mlabel format(%9,2g) mlabposition(12) mlabgap(*2) xtitle(Average Linear Prediction on Republican Vote Share)


*create a numerical variables for state
encode(state), gen(state_n)

*create a variable for change in unemployment, family structure, income, demographics*
gen unempchange = unempl2000 - unempl1990 
label variable unempchange "Change in Unemployment"
gen famchange = married2000 - married1990 
label variable famchange "Change in Marriage Rates"
gen incochange = (income2000 - income1990)
label variable incochange "Change in Income"
gen popchange = (pop2000 - pop1990)
label variable popchange "Change in Population"
gen urbchange = urban2000 - urban1990 
label variable urbchange "Change in Urbanisation"
gen collchange = college2000 - college1990 
label variable collchange "Change in Prop. of College Educated"
gen highchange = hs2000 - hs1990 
label variable highchange "Change in Prop. of High School Educated"
gen hispchange = hisp2000 - hisp1990 
label variable hispchange "Change in Prop. of Hispanics"
gen blackchange = black2000 - black1990 
label variable hispchange "Change in Prop. of African Americans"
gen malechange = male2000 - male1990
label variable malechange "Change in Prop. of Males"
*creage a numerical variable for county
egen county_n = group(state county)
label variable county_n "County Fixed Effects"

local econfactors unempchange incochange income2000 unempl2000
local demofactors famchange popchange urbchange collchange highchange hispchange blackchange malechange married2000 pop2000 urban2000 college2000 hs2000 hisp2000 black2000 male2000
local controlscable nochannels chandec popdec


reg GOPChange foxnews2000 RvoteShare96 `controlscable' `demofactors' `econfactors', robust
outreg2 using outreg_final.doc, replace

local geoFEvar county_n
reg GOPChange foxnews2000 RvoteShare96 `controlscable' `demofactors' `econfactors' `geoFEvar', robust


*from these results, the following variables are statistically insignificant: famchange, popchange, collchange, unempchange. so for these, we will change back to static figures*
local geoFEvar county_n
local controlscable nochannels chandec popdec
local econfactors income2000 unempl2000 
local demofactors hispchange malechange married2000 urban2000 college2000 black2000 hisp2000 male2000

reg GOPChange foxnews2000 RvoteShare96 `controlscable' `demofactors' `econfactors' `geoFEvar', robust
outreg2 using outreg_final.doc, replace
coefplot, msymbol(S) drop(_cons) xline(0) coeflabel(male2000 = "Fraction Male" hisp2000 = "Fraction Hispanic" nochannels = "No. of Channels" married2000 = "Fraction married" urban2000 = "Fraction living in urban areas" college2000 = "Fraction with College Degree" black2000 = "Fraction African American" unempl2000 = "Unemployment rate" income2000 = "Median Income") mlabel format(%9,2g) mlabposition(12) mlabgap(*2) xtitle(Linear Prediction on Republican Vote Share)


*investigating reverse causality*
local econfactors unempchange incochange income2000 unempl2000
local demofactors famchange popchange urbchange collchange highchange hispchange blackchange malechange married2000 pop2000 urban2000 college2000 hs2000 hisp2000 black2000 male2000
local controlscable nochannels chandec popdec
local geoFEvar county_n


reg foxnews2000 GOPChange RvoteShare96 `controlscable' `demofactors' `econfactors' `geoFEvar', robust
outreg2 outreg_final.doc, replace 





*swap state_n and county_n to exchange state fixed effects and county fixed effect
local geoFEvar county_n
local controlscable nochannels sub2000 poptot2000 


*now lets add some fixed effects
local geoFEvar county_n
local econfactors unempchange incochange income2000 unempl2000
local demofactors famchange popchange urbchange collchange highchange hispchange blackchange malechange married2000 pop2000 urban2000 college2000 hs2000 hisp2000 black2000 male2000
local controlscable nochannels chandec popdec
reg GOPChange foxnews2000, robust
outreg2 using outreg_final.doc, replace
reg GOPChange foxnews2000 RvoteShare96, robust
outreg2 using outreg_final.doc, append
reg GOPChange foxnews2000 RvoteShare96 `controlscable', robust
outreg2 using outreg_final.doc, append
reg GOPChange foxnews2000 RvoteShare96 `controlscable' `econfactors', robust
outreg2 using outreg_final.doc, append
reg GOPChange foxnews2000 RvoteShare96 `controlscable' `demofactors', robust
outreg2 using outreg_final.doc, append
reg GOPChange foxnews2000 RvoteShare96 `controlscable' `demofactors' `econfactors', robust
outreg2 using outreg_final.doc, append
reg GOPChange foxnews2000 RvoteShare96 `controlscable' `demofactors' `econfactors' `geoFEvar', robust
outreg2 using outreg_final.doc, append see label
reg GOPChange foxnews2000 RvoteShare96 foxnews2000##c.RvoteShare96 `controlscable' foxnews2000##c.nochannels `demofactors' foxnews2000##c.hispchange foxnews##c.black2000 foxnews##c.urban2000 foxnews##c.college2000 `econfactors' foxnews##c.unempl2000 `geoFEvar', robust
outreg2 using outreg_final.doc, ctitle(Interactions) keep(foxnews2000 RvoteShare96 foxnews2000##c.RvoteShare96 `controlscable1' foxnews2000##c.nochannels `demofactors' foxnews2000##c.hispchange foxnews##c.black2000 foxnews##c.urban2000 foxnews##c.college2000 `econfactors' foxnews##c.unempl2000) addtext(Full Pop., Econ. and Dem. Controls, YES) label append



reg GOPChange foxnews2000 `geoFEvar', robust
reg GOPChange foxnews2000 RvoteShare96 `geoFEvar', robust
reg GOPChange foxnews2000 RvoteShare96 `controlscable1' `geoFEvar', robust

*while so far fixed effects dont seem to be important, still need to give it a go for econ and dem factors, but need to generate variables by county: reg GOPChange foxnews2000 RvoteShare96 `controlscable1' `demofactors' `econfactors'`geoFEvar', robustoutreg2 using reg_1, tex(frag) ctitle(Fixed Effects) keep(fox RvoteShare96) addtext(County Fixed Effects,YES) append label *
local controlscable1 nochannels sub2000 poptot2000
local geoFEvar county_n
reg GOPChange foxnews2000 RvoteShare96 `controlscable1' college2000 urban2000 black2000, robust
reg GOPChange foxnews2000 RvoteShare96 `controlscable1' college2000 urban2000 black2000 `geoFEvar', robust
outreg2 using reg_1, tex(frag) ctitle(Demographic Controls) keep(foxnews RvoteShare96 college2 unempl2 black2 urban2) addtext(County Fixed Effects,YES) append label 

local geoFEvar county_n
reg GOPChange foxnews2000 RvoteShare96 nochannels sub2000 poptot2000 pop2 income2 male2 college2 unempl2 black2000 urban2000, robust
reg GOPChange foxnews2000 RvoteShare96 nochannels sub2000 poptot2000 pop2 income2 male2 college2 unempl2 black2000 urban2000 i.`geoFEvar', robust
outreg2 using reg_1, tex(frag) ctitle(Demographic Controls) keep(foxnews2000 RvoteShare96 pop2 income2 male2 college2 unempl2 black2 urban2) addtext(County Fixed Effects,YES) append label 

*visualizations and interactions*

local geoFEvar county_n
local controlscable nochannels chandec popdec
local econfactors income2000 unempl2000 
local demofactors hispchange malechange married2000 urban2000 college2000 black2000 hisp2000 male2000

reg GOPChange foxnews2000 RvoteShare96 `controlscable' `demofactors' `econfactors' `geoFEvar', robust
outreg2 using outreg_final.doc, ctitle(Variables)

reg GOPChange i.foxnews2000 RvoteShare96 foxnews2000##c.RvoteShare96 `controlscable' foxnews2000##c.nochannels `demofactors' foxnews2000##c.hispchange foxnews##c.black2000 foxnews##c.urban2000 foxnews##c.college2000 `econfactors' foxnews##c.unempl2000, robust
outreg2 using reg_3, tex(frag) ctitle(Interactions) keep(foxnews2000 RvoteShare96 foxnews2000##c.RvoteShare96 `controlscable1' foxnews2000##c.nochannels `demofactors' foxnews2000##c.hispchange foxnews##c.black2000 foxnews##c.urban2000 foxnews##c.college2000 `econfactors' foxnews##c.unempl2000) addtext(Full Pop., Econ. and Dem. Controls, YES) label replace

margins, dydx(foxnews2000) at(RvoteShare96=(0(0.1)1)) vsquish
marginsplot, yline(0) title("Marginal Effects of Fox News - Republican Vote Share in 1996 Interaction")
graph export urban_ME.png, replace

margins, dydx(foxnews2000) at(nochannels=(0(25)150)) vsquish
marginsplot, yline(0) title("Marginal Effects of Fox News - change in no of channels interaction")
graph export urban_ME.png, replace
*this chart shows how if you increase the no. of channels, the effect of fox news on Rep vote share not only declines, but becomes negative*

margins, dydx(foxnews2000) at(hispchange=(-0.5(0.1)0.5)) vsquish
marginsplot, yline(0) title("Marginal Effects of Fox News - change in hispanic population interaction")
graph export income_ME.png, replace
*increase in hispanic population means foxnews has positive effect on GOP Share*

margins, dydx(foxnews2000) at(black2000=(0(0.1)1)) vsquish
marginsplot, yline(0) title("Marginal Effects of Fox News - African American population interaction")
graph export income_ME.png, replace
*a high proportion of African Americans in the county means foxnews actually has a negative effect on GOP Share*

coefplot (I, label(FWithout Fixed Effects) pstyle(p3)) (J, label(With Fixed Effects) pstyle(p4)), msymbol(S) drop(_cons) xline(0) coeflabel(male2000 = "Fraction Male" hisp2000 = "Fraction Hispanic" nochannels = "No. of Channels" married2000 = "Fraction married" urban2000 = "Fraction living in urban areas" college2000 = "Fraction with College Degree" black2000 = "Fraction African American" unempl2000 = "Unemployment rate" income2000 = "Median Income") mlabel format(%9,2g) mlabposition(12) mlabgap(*2) xtitle(Average Linear Prediction on Republican Vote Share)


margins, dydx(foxnews2000) at(urban2000=(0(0.1)1)) vsquish
marginsplot, yline(0) title("Marginal Effects of Fox News - Urban population interaction")
graph export income_ME.png, replace

margins, dydx(foxnews2000) at(college2000=(0(0.1)1)) vsquish
marginsplot, yline(0) title("Marginal Effects of Fox News - African American populationinteraction")
graph export income_ME.png, replace
*the marginal impact of Fox News increases on average with the fraction of the population with a cllege degree, however for areas with a very high proportion there is extreme variation*

margins, dydx(foxnews2000) at(unempl2000=(0(0.1)1)) vsquish
marginsplot, yline(0) title("Marginal Effects of Fox News - Unemployment interaction")
graph export income_ME.png, replace


margins, dydx(foxnews2000) at(urban2=(0(.1)1)) vsquish
margins, dydx(foxnews2000) at(income2=(0(2)20)) vsquish
margins, dydx(foxnews2000) at(nochannel=(0(10)100)) vsquish
margins, dydx(foxnews2000) at(pct_sub=(0(.1)1)) vsquish


*Include some basic controls for population and demographics

*-----------------------Examine the assignment of Fox News---------------------*


logit foxnews RvoteShare96

logit foxnews RvoteShare96 pop18
est store basic

logit foxnews2000 RvoteShare96 college2000 black2000 empl2000 income2000

logit foxnews2000 RvoteShare96 college2000 black2000 empl2000 income2000 urban2000 male2000 college2000 pop2000
est store demControls

logit foxnews2000 RvoteShare96 college2000 black2000 empl2000 income2000 urban2000 male2000 college2000 pop2000 nochan 
est store cableControls

outreg2 [basic demControls cableControls] using logit_1, tex(frag) label replace
