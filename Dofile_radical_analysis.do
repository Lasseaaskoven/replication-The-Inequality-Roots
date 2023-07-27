import excel "D:\Forskning og undervisning\Rural inequality and political effects Denmark\Election_data_with_county.xlsx", sheet("Sheet1") firstrow


*Removal of non-radical candidates*
keep if pty==10 


*Checking and removal of duplicates (instances of more than one candidates), since we are only interested in party-level data*
sort id cst
by id cst:  gen dup = cond(_N==1,0,_n)
list cst_n yr if dup>0
keep if dup<2

*Setting uncontested elections and missing data for relevant variables to missing*
destring pev1 vot1 vv1 pv1, replace force

*Collapsing by county and election*

collapse (sum) pev1 vot1 vv1 pv1 (mean) yr, by( county id)





*___________________________________________________________________________________*

*Merging with majorat data and initial data analysis*

*Merging with 1918 election*

append using "D:\Forskning og undervisning\Rural inequality and political effects Denmark\1918_election_radikal.dta"

*Merging of rural inequality datadata*

merge m:1 county using "D:\Forskning og undervisning\Rural inequality and political effects Denmark\rural_gini_1905.dta"

drop _merge



*Merging of population data*

merge m:1 county yr using "D:\Forskning og undervisning\Rural inequality and political effects Denmark\population_data"


*Generation of numeric county variable*
encode county, gen(county_n)



*Generation of log of population*
generate logpop= log(pop_total)

*generate urbanization*
 generate urbanization= pop_city /pop_total
generate ruralshare= pop_rural /pop_total


*generate newid*
generate newid= id*100

*setting time frame*
xtset county_n newid



*Generation of a turnout variable*
generate n_votes= vv1
replace n_votes= vot1 if vv1==0
replace n_votes=. if vot1==0 & vv1==0 

generate turnout= n_votes/pev1 



*Generation of  measure of  electoral support (share of votes)*
generate radicalsupport= pv1/n_votes


*Random effects analysis*
xtreg radicalsupport c.rural_gini_1905##c.ruralshare logpop i.newid, re cluster (county_n), if yr>1915 

*Pooled OLS*
reg radicalsupport c.rural_gini_1905##c.ruralshare logpop i.newid, cluster (county_n), if yr>1915 



*Random effects analysis excluding Copenhagen*

xtreg radicalsupport c.rural_gini_1905##c.ruralshare logpop i.newid, re cluster (county_n), if yr>1915 & county_n!=9




reg radicalsupport rural_gini_1905, cluster (county_n)
reg radicalsupport rural_gini_1905  logpop urbanization, cluster (county_n)
reg radicalsupport rural_gini_1905  logpop urbanization turnout, cluster (county_n)
reg radicalsupport c.rural_gini_1905##c.ruralshare logpop, cluster (county_n)
reg radicalsupport c.rural_gini_1905##c.ruralshare logpop  turnout, cluster (county_n)
reg radicalsupport c.rural_gini_1905##c.ruralshare logpop, cluster (county_n), if yr<1920


reg radicalsupport c.rural_gini_1905##c.ruralshare logpop i.yr, cluster (county_n), if yr>1920
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.04) .8401568))
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))


reg radicalsupport c.rural_gini_1905 ruralshare logpop i.yr, cluster (county_n), if yr>1920

*Excluding Copenhagen*
reg radicalsupport rural_gini_1905 ruralshare logpop i.yr, cluster (county_n), if yr>1920 & county_n!=9

reg radicalsupport c.rural_gini_1905##c.ruralshare logpop i.yr, cluster (county_n), if yr>1920 & county_n!=9




*Difference-in-difference*
generate democratization=0
replace democratization=1 if yr>1915



xtreg radicalsupport  c.rural_gini_1905##c.democratization     i.newid, fe cluster (county_n), if yr>1901 & yr!=1915

xtreg radicalsupport c.rural_gini_1905##c.democratization   ruralshare  i.newid, fe cluster (county_n), if yr>1901 & yr!=1915

xtreg radicalsupport c.rural_gini_1905##c.democratization   ruralshare logpop i.newid, fe cluster (county_n), if yr>1901 & yr!=1915


xtreg radicalsupport c.rural_gini_1905##c.democratization##c.ruralshare logpop i.newid, fe cluster (county_n), if yr>1901 & yr!=1915




*Difference-in-difference Kanslergadeforliget*



generate postkanslergade=0
replace postkanslergade=1 if yr>1933

xtreg   radicalsupport    c.ruralshare##postkanslergade  i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg   radicalsupport c.ruralshare##postkanslergade logpop i.newid, fe cluster (county_n), if yr>1900 & yr!=1915

xtreg  radicalsupport  c.ruralshare##postkanslergade logpop i.newid, fe cluster (county_n), if yr>1915

bootstrap, cluster(county_n) rep(1000) seed(123): areg  radicalsupport c.ruralshare##postkanslergade  logpop i.newid , absorb(county_n) , if yr>1915   
margins, dydx(postkanslergade)  at(ruralshare=(.0 (0.20)0.9)) noestimcheck
marginsplot, level(90)xtitle (Share of rural population) ytitle (Effect of "Kanslergadeforliget") yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.2f))

