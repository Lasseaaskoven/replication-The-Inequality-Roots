import excel "D:\Forskning og undervisning\Rural inequality and political effects Denmark\Election_data_with_county.xlsx", sheet("Sheet1") firstrow



*Indicator for uncontested election*
g uncontested=1 if pv1=="Uncontested"
replace uncontested=0 if uncontested==.

g venstre_uncontested=1 if uncontested==1 & pty==24 
replace venstre_uncontested=0 if venstre_uncontested==.

*Setting uncontested elections and missing data for relevant variables to missing*
destring pev1 vot1 vv1 pv1, replace force

*Venstre votes*
g h=pv1  if pty==24 


*replace venstre vote share with 0  if no venstre candidate*
replace h=0 if h==. 




*Checking and duplicates*
g d=1 if h!=0
bys cst id: egen sum=sum(d)


list cst_n yr if sum>1 


*Average venstre vote share*
sort cst id
by cst id: egen venstre= sum(h)

replace venstre= venstre/sum if sum>0 


*Keeping only one candidate per constituency*
sort id cst
by id cst:  gen dup = cond(_N==1,0,_n)
list cst_n yr if dup>0
keep if dup<2



*Collapsing by county and election*

collapse (sum) pev1 vot1 vv1 venstre  (mean) yr uncontested venstre_uncontested , by( county id)


rename venstre pv1


*___________________________________________________________________________________*

*Merging with majorat data and initial data analysis*


*Merging with 1918 election*
append using  "D:\Forskning og undervisning\Rural inequality and political effects Denmark\1918_election_venstre.dta" 


*Merging of rural inequality datadata*

merge m:1 county using "D:\Forskning og undervisning\Rural inequality and political effects Denmark\rural_gini_1905_v2.dta"

drop _merge


*Merging with land expropriation data*
merge m:1 county using "D:\Forskning og undervisning\Rural inequality and political effects Denmark\land_expropriation.dta"

drop _merge


*Merging of population data*

merge m:1 county yr using "D:\Forskning og undervisning\Rural inequality and political effects Denmark\population_data"



*Generation of numeric county variable*
encode county, gen(county_n)



*Generation of log of population*
generate logpop= log(pop_total)

*generate urbanization & rural share*
 generate urbanization= pop_city /pop_total

generate ruralshare= pop_rural /pop_total
 
 
 *Generation of new id*
 generate newid= id*100
 


 
*setting time frame*
xtset county_n newid



*Generation of a turnout variable*
generate n_votes= vv1
replace n_votes= vot1 if vv1==0
replace n_votes=. if vot1==0 & vv1==0 

generate turnout= n_votes/pev1 



*Generation of  measure of  electoral support (share of votes)*
generate venstresupport= pv1/n_votes

* Correlation between the two measures of land inequality
corr rural_gini_1905 share_held_20hrt_estates

*Descriptive statistics*
xtsum venstresupport rural_gini_1905 logpop ruralshare majorat_land_expropriated_hectar uncontested if yr>1900 & yr<1945 & yr!=1915 & rural_gini_1905!=.



*Difference-in-difference, Liberal Party support*
generate democratization=0
replace democratization=1 if yr>1915

*Placebo democratization*
generate nondemocratization=0
replace nondemocratization=1 if yr>1906


generate landreform=0
replace landreform=1 if yr>1920

*Difference-in-difference 1901 and beyond*
xtreg venstresupport   c.rural_gini_1905##c.democratization     i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & yr<1945

xtreg venstresupport   c.rural_gini_1905##c.democratization   ruralshare  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg venstresupport   c.rural_gini_1905##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915


xtreg venstresupport   c.rural_gini_1905##c.democratization c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

sum ruralshare rural_gini_1905 majorat_land_expropriated_hectar, detail, if yr>1900 & yr<1945 & yr!=1915 & rural_gini_1905!=.

bysort newid : egen year_mean2 = mean(venstresupport)

bysort newid : egen mean_above_median = mean(venstresupport) if rural_gini_1905>  .7505238 
bysort newid : egen mean_below_median = mean(venstresupport) if rural_gini_1905<= .7505238 



*Footnote: Not excluding the 1915 election*
xtreg venstresupport   c.rural_gini_1905##c.democratization     i.newid, fe cluster (county_n), if yr>1900  & yr<1945

xtreg venstresupport   c.rural_gini_1905##c.democratization   ruralshare  i.newid, fe cluster (county_n), if yr>1900 

xtreg venstresupport   c.rural_gini_1905##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 

xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 


xtreg venstresupport   c.rural_gini_1905##c.democratization c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 






* Above and below mean for both land inequality and rural share*
bysort newid : egen highin_highrural = mean(venstresupport) if rural_gini_1905>   .7505238   & ruralshare>  .7470556 
bysort newid : egen nonhighin_highrural = mean(venstresupport) if rural_gini_1905<=    .7505238  & ruralshare>   .7470556 

bysort newid : egen highin_nonhighrural = mean(venstresupport) if rural_gini_1905>   .7505238   & ruralshare<=    .7470556 
bysort newid : egen nonhighin_nonhighrural = mean(venstresupport) if rural_gini_1905<=   .7505238   & ruralshare<=     .7470556 


twoway (line nonhighin_highrural yr,lcolor(gs8) lpattern(solid) lwidth(thick)) (line highin_highrural yr,lcolor(gs8) lpattern(dot) lwidth(thick))(line nonhighin_nonhighrural yr,lcolor(gs8) lpattern (dash) lwidth(thick)) (line highin_nonhighrural yr,lcolor(gs8) lpattern(dash_dot) lwidth(thick)),legend(lab(1 "Rural, non-high inequality") lab(2 "Rural, high inequality")lab(3 "Non-rural, non-high inequality")lab(4 "Non-rural, high inequality")) graphregion(color(white)) ytitle(Liberal Party vote share) xtitle("") xline(1915, lstyle(grid) lcolor(gs8) lpattern(longdash))  ylabel( 0.1 0.2 0.3 0.4 0.5 0.6 0.7, format(%9.1f)) xlabel(  1901 1909 1915"Democratization"  1925  1935  1943),  if yr>1900 & yr<1945 & yr!=1915



*Continouus difference-in-difference*

xtreg venstresupport   c.rural_gini_1905#i.yr  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx( rural_gini_1905) over (i.yr) noestimcheck 
marginsplot, level(90)xtitle (Year) ytitle (Effect of rural inequality on Liberal Party vote share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xline(1915, lstyle(grid) lcolor(gs8) lpattern(longdash)) xlabel(, format(%9.0f)) ylabel(, format(%9.2f)) xlabel( 1901 1909  1915 1920 1924  1929 1932 1935 1939, format(%9.0f)) 








*Boostrapped clustered standard errors*

tsset, clear


bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.rural_gini_1905##c.democratization    i.newid , absorb(county_n), if yr>1900 & yr!=1915 & yr<1945
bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.rural_gini_1905##c.democratization ruralshare     i.newid , absorb(county_n), if yr>1900  & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.rural_gini_1905##c.democratization ruralshare logpop   i.newid , absorb(county_n), if yr>1900   & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.rural_gini_1905##c.democratization##c.ruralshare ruralshare logpop  i.newid , absorb(county_n) , if yr>1900  & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.rural_gini_1905##c.democratization##c.ruralshare ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform  i.newid , absorb(county_n) , if yr>1900   & yr!=1915
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


bootstrap, cluster(county_n) rep(1000) seed(123): areg  venstresupport  c.rural_gini_1905##c.democratization ruralshare logpop   i.newid , absorb(county_n), if yr>1900   & yr!=1915 & county_n!=9

bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.rural_gini_1905##c.democratization##c.ruralshare ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform  i.newid , absorb(county_n) , if yr>1900   & yr!=1915 & county_n!=9
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))



xtset county_n newid

*Without clustered standard errors*

xtreg venstresupport   c.rural_gini_1905##c.democratization  i.newid, fe, if yr>1900 & yr!=1915 & yr<1945

xtreg venstresupport   c.rural_gini_1905##c.democratization ruralshare  i.newid, fe, if yr>1900 & yr!=1915

xtreg venstresupport   c.rural_gini_1905##c.democratization logpop ruralshare i.newid, fe, if yr>1900 & yr!=1915


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop i.newid, fe, if yr>1900 & yr!=1915

xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe, if yr>1900 & yr!=1915



*With bootstrapped standard errors*

xtreg venstresupport   c.rural_gini_1905##c.democratization   i.newid, fe    vce(bootstrap), if yr>1900 & yr!=1915 & yr<1945

xtreg venstresupport   c.rural_gini_1905##c.democratization  ruralshare i.newid, fe    vce(bootstrap), if yr>1900 & yr!=1915 & yr<1945


xtreg venstresupport   c.rural_gini_1905##c.democratization logpop ruralshare i.newid, fe    vce(bootstrap), if yr>1900 & yr!=1915


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop i.newid, fe  vce(bootstrap) , if yr>1900 & yr!=1915


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform  i.newid, fe vce(bootstrap), if yr>1900 & yr!=1915
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe vce(bootstrap), if yr>1900 & yr!=1915
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))



*Marginal effects 1900 and onwards v.1*
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


*Marginal effects 1900 and onwards v.1.1 excluding  Copenhagen*
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))



*Marginal effects 1900 and onwards v.2*
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


*Excluding Copenhagen*

xtreg venstresupport   c.rural_gini_1905##c.democratization ruralshare logpop  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))



*Excluding observations with any non-contested elections*
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915  & uncontested==0
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9 & uncontested==0


* Effect over time*
generate timesincedem= yr-1915

xtreg venstresupport   c.rural_gini_1905##c.timesincedem   ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx(rural_gini_1905 ) over(timesincedem) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Years since democratization) ytitle (Marginal effect of rural inequality) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f)) xlabel(, format(%9.0f))



*Random effects: Democratic period*
xtreg venstresupport rural_gini_1905 ruralshare logpop i.newid , re cluster (county_n), if yr>1915

xtreg venstresupport c.rural_gini_1905##c.ruralshare logpop i.newid , re cluster (county_n), if yr>1915

xtreg venstresupport rural_gini_1905 ruralshare logpop i.newid , re cluster (county_n), if yr>1915

xtreg venstresupport c.rural_gini_1905##c.ruralshare logpop i.newid , re cluster (county_n), if yr>1915


*Random effects: Excluding Copenhagen 
xtreg venstresupport c.rural_gini_1905##c.ruralshare logpop i.newid , re cluster (county_n), if yr>1915 & county_n!=9


*Excluding Copenhagen*
reg venstresupport rural_gini_1905 logpop ruralshare i.yr, cluster (county_n), if yr>1915 & county_n!=9
reg venstresupport c.rural_gini_1905##c.ruralshare logpop i.yr, cluster (county_n), if yr>1915 & county_n!=9
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332))
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))



*Mechanisms: Newspaper density in 1906
drop _merge

merge m:1 county using "D:\Forskning og undervisning\Rural inequality and political effects Denmark\venstre_newspapers_1906.dta"

generate newspaperdensity1906=.
replace newspaperdensity1906= no_venstre_newspapers_1906/logpop if yr==1906


reg newspaperdensity1906 rural_gini_190, robust 
reg newspaperdensity1906 rural_gini_190 ruralshare , robust 
reg newspaperdensity1906 rural_gini_190 logpop ruralshare , robust 
reg newspaperdensity1906 c.rural_gini_190##c.ruralshar logpop , robust 

generate newspaperdensity19062=.
replace newspaperdensity19062= no_venstre_newspapers_1906/pop_total if yr==1906

reg newspaperdensity19062 rural_gini_190, robust 
reg newspaperdensity19062 rural_gini_190 ruralshare , robust 
reg newspaperdensity19062 rural_gini_190 logpop ruralshare , robust 

reg newspaperdensity19062 rural_gini_190, robust, if county_n!=9 
reg newspaperdensity19062 rural_gini_190 ruralshare , robust , if county_n!=9 
reg newspaperdensity19062 rural_gini_190 logpop ruralshare , robust , if county_n!=9 




reg newspaperdensity19062 c.rural_gini_190##c.ruralshare logpop  , robust 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332))
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.4f))

reg newspaperdensity19062 rural_gini_190 logpop ruralshare venstresupport , robust 


reg  no_venstre_newspapers_1906 rural_gini_190 logpop  , robust , if yr==1906
reg  no_venstre_newspapers_1906 rural_gini_190 logpop ruralshare , robust , if yr==1906
reg  no_venstre_newspapers_1906 c.rural_gini_190##c.ruralshare  logpop  , robust , if yr==1906


*Further Mechanism: Turnout*
xtreg turnout  c.rural_gini_1905##c.democratization  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & yr<1945

xtreg  turnout c.rural_gini_1905##c.democratization   ruralshare  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg  turnout  c.rural_gini_1905##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg  turnout  c.rural_gini_1905##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915


xtreg turnout  c.rural_gini_1905##c.democratization ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9


xtreg turnout  c.rural_gini_1905##c.democratization##c.ruralshare  c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg turnout  c.rural_gini_1905##c.democratization##c.ruralshare  c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg  turnout  c.no_venstre_newspapers_1906##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915



generate newspaperdensity19063=.
replace newspaperdensity19063= no_venstre_newspapers_1906/pop_total 

xtreg turnout  c.newspaperdensity19063##c.democratization  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & yr<1945

xtreg  turnout c.newspaperdensity19063##c.democratization   ruralshare  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg  turnout  c.newspaperdensity19063##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg turnout  c.newspaperdensity19063##c.democratization##c.ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915


xtreg turnout  c.newspaperdensity19063##c.democratization##c.ruralshare  c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx(ruralshare ) at(newspaperdensity19063=(.0000096(0.000001)  .000099)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Liberal Party newspapers/county population) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg turnout  c.newspaperdensity19063##c.democratization##c.ruralshare  c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx(ruralshare ) at(newspaperdensity19063=(.0000096(0.000001)  .000099)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle ( Liberal Party newspapers/county population) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


tsset, clear

bootstrap, cluster(county_n) rep(1000) seed(123): areg turnout c.rural_gini_1905##c.democratization    i.newid , absorb(county_n), if yr>1900 & yr!=1915 & yr<1945
bootstrap, cluster(county_n) rep(1000) seed(123): areg turnout  c.rural_gini_1905##c.democratization ruralshare     i.newid , absorb(county_n), if yr>1900  & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg turnout  c.rural_gini_1905##c.democratization ruralshare logpop   i.newid , absorb(county_n), if yr>1900   & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg turnout  c.rural_gini_1905##c.democratization##c.ruralshare ruralshare logpop  i.newid , absorb(county_n) , if yr>1900  & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg turnout  c.rural_gini_1905##c.democratization##c.ruralshare ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform  i.newid , absorb(county_n) , if yr>1900   & yr!=1915

bootstrap, cluster(county_n) rep(1000) seed(123): areg turnout  c.rural_gini_1905##c.democratization ruralshare logpop   i.newid , absorb(county_n), if yr>1900   & yr!=1915 & county_n!=9

xtset county_n newid


* Turnout's effect*
xtreg venstresupport logpop ruralshare turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg venstresupport logpop ruralshare c.democratization##c.turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx(democratization) at(turnout=(.15(0.15) 0.9)) noestimcheck
marginsplot, level(90)xtitle (Turnout level) ytitle (Effect of democratization on Liberal Party vote share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport logpop ruralshare c.democratization##c.turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9
margins, dydx(democratization) at(turnout=(.15(0.15) 0.9)) noestimcheck
marginsplot, level(90)xtitle (Turnout level) ytitle (Effect of democratization on Liberal Party vote share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))




xtreg venstresupport logpop c.ruralshare##c.turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg venstresupport logpop ruralshare turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport logpop c.ruralshare##c.turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport logpop ruralshare c.democratization##c.turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg venstresupport logpop c.democratization##c.ruralshare##c.turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx(turnout ) at(ruralshare=(.0(0.1) 0.9)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Rural population share) ytitle (Marginal effect of turnout share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport logpop c.democratization##c.ruralshare##c.turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx(turnout ) at(ruralshare=(.0(0.1) 0.9)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Rural population share) ytitle (Marginal effect of turnout share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport logpop ruralshare c.democratization##c.turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport logpop c.democratization##c.ruralshare##c.turnout i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport logpop ruralshare turnout i.newid, fe cluster (county_n), if yr>1915

xtreg venstresupport logpop c.ruralshare##c.turnout i.newid, fe cluster (county_n), if yr>1915



*Mechanisms as difference-in-difference*
xtreg turnout  c.rural_gini_1905##c.democratization     i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & yr<1945

xtreg  turnout c.rural_gini_1905##c.democratization   ruralshare  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg  turnout  c.rural_gini_1905##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg turnout   c.rural_gini_1905##c.democratization##c.ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915


xtreg turnout  c.rural_gini_1905##c.democratization c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg turnout  c.rural_gini_1905##c.democratization##c.ruralshare c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))



xtreg turnout  c.rural_gini_1905##c.democratization##c.ruralshare c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


* Difference-in-difference using the alternative measure of land inequality*

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization     i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & yr<1945

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization   ruralshare  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization##c.ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915


xtreg venstresupport   c.share_held_20hrt_estates##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx(ruralshare ) at(share_held_20hrt_estates=(.0(0.02) 0.20)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Share of agricultural land held by large estates) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.share_held_20hrt_estates##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915
margins, dydx(ruralshare ) at(share_held_20hrt_estates=(.0(0.02) 0.20)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Share of agricultural land held by large estates) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))



xtreg venstresupport   c.share_held_20hrt_estates##c.democratization c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.share_held_20hrt_estates##c.democratization  i.newid , absorb(county_n) , if yr>1900   & yr!=1915 & yr<1945
bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.share_held_20hrt_estates##c.democratization ruralshare   i.newid , absorb(county_n) , if yr>1900   & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.share_held_20hrt_estates##c.democratization ruralshare  logpop   i.newid , absorb(county_n) , if yr>1900   & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.share_held_20hrt_estates##c.democratization##c.ruralshare  logpop   i.newid , absorb(county_n) , if yr>1900   & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.share_held_20hrt_estates##c.democratization##c.ruralshare  logpop c.majorat_land_expropriated_hectar##c.landreform  i.newid , absorb(county_n) , if yr>1900   & yr!=1915


*Excluding Copenhagen*

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9
margins, dydx(ruralshare ) at(share_held_20hrt_estates=(.0(0.02) 0.20)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Share of agricultural land held by large estates) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.share_held_20hrt_estates##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9
margins, dydx(ruralshare ) at(share_held_20hrt_estates=(.0(0.02) 0.20)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Share of agricultural land held by large estates) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))

bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.share_held_20hrt_estates##c.democratization##c.ruralshare  logpop c.majorat_land_expropriated_hectar##c.landreform  i.newid , absorb(county_n) , if yr>1900   & yr!=1915 & county_n!=9



* Difference-in-difference using the alternative measure of land inequality (bootstrapped)*

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization     i.newid, fe  vce(bootstrap), if yr>1900 & yr!=1915 & yr<1945

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization   ruralshare  i.newid, fe  vce(bootstrap), if yr>1900 & yr!=1915

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization   ruralshare logpop i.newid, fe  vce(bootstrap), if yr>1900 & yr!=1915

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization##c.ruralshare logpop i.newid, fe  vce(bootstrap), if yr>1900 & yr!=1915


xtreg venstresupport   c.share_held_20hrt_estates##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe  vce(bootstrap), if yr>1900 & yr!=1915
margins, dydx(ruralshare ) at(share_held_20hrt_estates=(.0(0.02) 0.20)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Share of agricultural land held by large estates) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.share_held_20hrt_estates##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe  vce(bootstrap), if yr>1900 & yr!=1915
margins, dydx(ruralshare ) at(share_held_20hrt_estates=(.0(0.02) 0.20)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Share of agricultural land held by large estates) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))



xtreg venstresupport   c.share_held_20hrt_estates##c.democratization c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe  vce(bootstrap), if yr>1900 & yr!=1915


*Excluding Copenhagen (bootstrapped)*

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization ruralshare logpop i.newid, fe  vce(bootstrap), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport   c.share_held_20hrt_estates##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe  vce(bootstrap), if yr>1900 & yr!=1915 & county_n!=9
margins, dydx(ruralshare ) at(share_held_20hrt_estates=(.0(0.02) 0.20)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Share of agricultural land held by large estates) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.share_held_20hrt_estates##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe  vce(bootstrap), if yr>1900 & yr!=1915 & county_n!=9
margins, dydx(ruralshare ) at(share_held_20hrt_estates=(.0(0.02) 0.20)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Share of agricultural land held by large estates) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


*Only after 1909 (the United Party)*
xtreg venstresupport  c.rural_gini_1905##c.democratization    i.newid, fe cluster (county_n), if yr>1909 & yr!=1915
xtreg venstresupport  c.rural_gini_1905##c.democratization   ruralshare  i.newid, fe cluster (county_n), if yr>1909 & yr!=1915
xtreg venstresupport  c.rural_gini_1905##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1909 & yr!=1915
xtreg venstresupport  c.rural_gini_1905##c.democratization##c.ruralshare    logpop i.newid, fe cluster (county_n), if yr>1909 & yr!=1915
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1909 & yr!=1915



tsset, clear

bootstrap, cluster(county_n) rep(1000) seed(123): areg  venstresupport c.rural_gini_1905##c.democratization   i.newid , absorb(county_n), if  yr>1909 & yr!=1915 & yr<1945
bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  c.rural_gini_1905##c.democratization ruralshare     i.newid , absorb(county_n), if  yr>1909 & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg  venstresupport c.rural_gini_1905##c.democratization ruralshare logpop   i.newid , absorb(county_n), if  yr>1909 & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare ruralshare logpop  i.newid , absorb(county_n) , if yr>1909 & yr!=1915
bootstrap, cluster(county_n) rep(1000) seed(123): areg  venstresupport  c.rural_gini_1905##c.democratization##c.ruralshare ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform  i.newid , absorb(county_n) , if yr>1909 & yr!=1915


bootstrap, cluster(county_n) rep(1000) seed(123): areg  venstresupport  c.rural_gini_1905##c.democratization ruralshare logpop   i.newid , absorb(county_n), if yr>1900   & yr!=1915 & county_n!=9
bootstrap, cluster(county_n) rep(1000) seed(123): areg   venstresupport c.rural_gini_1905##c.democratization ruralshare logpop   i.newid , absorb(county_n), if yr>1900   & yr!=1915 & county_n!=9



xtset county_n newid



*Difference in difference 1865 and beyond*

xtreg venstresupport   c.rural_gini_1905##c.democratization     i.newid, fe cluster (county_n), if yr>1865 & yr!=1915 & yr<1945

xtreg venstresupport   c.rural_gini_1905##c.democratization   ruralshare  i.newid, fe cluster (county_n), if yr>1865 & yr!=1915

xtreg venstresupport   c.rural_gini_1905##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1865 & yr!=1915


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop i.newid, fe cluster (county_n), if yr>1865 & yr!=1915


xtreg venstresupport   c.rural_gini_1905##c.democratization c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1865 & yr!=1915
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1865 & yr!=1915



*Marginal effects 1865 and onwards*
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1865  & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1865  & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck, if democratization==0
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))




*Main pooled analysis* 

reg venstresupport c.rural_gini_1905, cluster (county_n), if yr>1920 & yr<1945
reg venstresupport c.rural_gini_1905 i.yr, cluster (county_n), if yr>1920 & yr<1945
reg venstresupport c.rural_gini_1905 ruralshare logpop i.yr, cluster (county_n), if yr>1920  & yr<1945
reg venstresupport c.rural_gini_1905##c.ruralshare i.yr, cluster (county_n), if yr>1920 & yr<1945
reg venstresupport c.rural_gini_1905##c.ruralshare logpop i.yr, cluster (county_n), if yr>1920 & yr<1945
reg venstresupport c.rural_gini_1905##c.ruralshare logpop i.yr , cluster (county_n), if yr>1920 & yr<1945
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.04) .8401568))
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))

*Additional analysis*
reg venstresupport rural_gini_1905, cluster (county_n)
reg venstresupport rural_gini_1905 logpop urbanization, cluster (county_n)
reg venstresupport rural_gini_1905 logpop urbanization turnout, cluster (county_n)
reg venstresupport rural_gini_1905 logpop ruralshare, cluster (county_n)

reg venstresupport c.rural_gini_1905##c.ruralshare i.yr logpop, cluster (county_n) if yr>1920  & yr<1945
reg venstresupport c.rural_gini_1905##c.ruralshare logpop, cluster (county_n)

reg venstresupport c.rural_gini_1905##c.ruralshare logpop  turnout, cluster (county_n)

reg venstresupport c.rural_gini_1905##c.ruralshare i.yr logpop, cluster (county_n), if yr>1870  & yr<1945

reg venstresupport c.rural_gini_1905##c.ruralshare i.yr logpop, cluster (county_n), if yr>1870  & yr<1924

reg venstresupport c.rural_gini_1905##c.ruralshare logpop turnout i.yr , cluster (county_n), if yr>1920 & yr<1945
reg venstresupport c.rural_gini_1905##c.ruralshare logpop i.yr, cluster (county_n), if yr>1920
reg venstresupport c.rural_gini_1905##c.ruralshare logpop i.yr i.county_n, cluster (county_n), if yr>1920


*Marginal effects using random effects*

xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, re cluster (county_n), if yr>1900 & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)), if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, re cluster (county_n), if yr>1900 & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)), if democratization==0
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, re , if yr>1900 & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)), if democratization==1
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, re , if yr>1900 & yr!=1915 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)), if democratization==0
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))



*Excluding Copenhagen*
xtreg venstresupport   c.rural_gini_1905##c.democratization ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9
xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport   c.rural_gini_1905##c.democratization c.majorat_land_expropriated_hectar##c.landreform  ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport   c.rural_gini_1905##c.democratization##c.ruralshare logpop c.majorat_land_expropriated_hectar##c.landreform i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9


*Difference-in-difference turnout*
xtreg turnout   c.rural_gini_1905##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg turnout   c.rural_gini_1905##c.democratization##c.ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg turnout   c.rural_gini_1905##c.democratization ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9
xtreg turnout   c.rural_gini_1905##c.democratization##c.ruralshare logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9


*Each election seperately* 
reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1901

reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1903

reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1906

reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1909

reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1910
reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1913
reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1918

reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1920

reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1924
reg venstresupport c.rural_gini_1905##c.ruralshare logpop , robust, if yr==1926
reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1929
reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1932
reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1935
reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1939
reg venstresupport c.rural_gini_1905##c.ruralshare logpop  , robust, if yr==1943

*Kanslergadeforlig analyse*

generate postkanslergade=0
replace postkanslergade=1 if yr>1933

*Difference-in-difference Kanslergadeforliget*

xtreg venstresupport     c.ruralshare##postkanslergade  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg venstresupport   c.ruralshare##postkanslergade logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg venstresupport   c.ruralshare##postkanslergade logpop i.newid, fe cluster (county_n), if yr>1915

bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport c.ruralshare##postkanslergade  logpop i.newid , absorb(county_n) , if yr>1915   
margins, dydx(postkanslergade)  at(ruralshare=(.0 (0.20)0.9)) noestimcheck
marginsplot, level(90)xtitle (Share of rural population) ytitle (Effect of "Kanslergadeforliget") yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))

xtreg turnout   c.ruralshare##postkanslergade logpop i.newid, fe cluster (county_n), if yr>1915

bootstrap, cluster(county_n) rep(1000) seed(123): areg turnout c.ruralshare##postkanslergade  logpop i.newid , absorb(county_n) , if yr>1915   


xtreg venstresupport   c.ruralshare##postkanslergade logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport   ruralshare  c.no_venstre_newspapers_190##c.postkanslergade logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg venstresupport   ruralshare  c.no_venstre_newspapers_190##c.postkanslergade logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915 & county_n!=9

xtreg venstresupport   ruralshare  c.no_venstre_newspapers_190##c.postkanslergade logpop i.newid, fe cluster (county_n), if yr>1915

bootstrap, cluster(county_n) rep(1000) seed(123): areg venstresupport  ruralshare  c.no_venstre_newspapers_190##c.postkanslergade logpop i.newid , absorb(county_n) , if yr>1915   




* Secret ballot analysis*
generate secretballot=0
replace secretballot=1 if yr>1900

xtreg venstresupport   c.rural_gini_1905##c.secretballot  i.newid, fe cluster (county_n), if  yr!=1915

xtreg venstresupport   c.rural_gini_1905##c.secretballot  ruralshare logpop i.newid, fe cluster (county_n), if  yr!=1915
xtreg venstresupport   c.rural_gini_1905##c.secretballot  ruralshare logpop i.newid, fe cluster (county_n), if yr!=1915 & yr<1918 


xtreg turnout  c.rural_gini_1905##c.secretballot  i.newid, fe cluster (county_n), if  yr!=1915 & yr<1918 

xtreg turnout  c.rural_gini_1905##c.secretballot  ruralshare logpop i.newid, fe cluster (county_n), if  yr!=1915 & yr<1918 
xtreg turnout   c.rural_gini_1905##c.secretballot  ruralshare logpop i.newid, fe cluster (county_n), if yr!=1915 & yr<1918 

tsset, clear



bootstrap, cluster(county_n) rep(1000) seed(123): areg turnout  c.rural_gini_1905##c.secretballot  ruralshare logpop i.newid , absorb(county_n) , if yr!=1915 & yr<1918 
margins, dydx(secretballot) at(rural_gini_1905=(.6723197(0.02) 0.78927317841332))  noestimcheck
marginsplot, level(90)xtitle (Rural Gini) ytitle (Effect of Secret Ballot) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))



bootstrap, cluster(county_n) rep(1000) seed(123): areg  venstresupport   c.rural_gini_1905##c.secretballot  ruralshare logpop i.newid , absorb(county_n) , if yr!=1915 & yr<1918 
margins, dydx(secretballot)  at(rural_gini_1905=(.6723197(0.02) 0.78927317841332)) noestimcheck
marginsplot, level(90)xtitle (Rural Gini) ytitle (Effect of Secret Ballot) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))




bootstrap, cluster(county_n) rep(1000) seed(123): areg  venstresupport   c.rural_gini_1905##c.secretballot##c.ruralshare logpop i.newid , absorb(county_n) , if yr!=1915 & yr<1918 

