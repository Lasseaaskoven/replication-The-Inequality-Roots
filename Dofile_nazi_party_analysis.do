import excel "D:\Forskning og undervisning\Rural inequality and political effects Denmark\Election_data_with_county.xlsx", sheet("Sheet1") firstrow


*Removal of non-nazi party candidates*
keep if pty==8 


*Checking and removal of duplicates (instances of more than one candidates), since we are only interested in party-level data*
sort id cst
by id cst:  gen dup = cond(_N==1,0,_n)
list cst_n yr if dup>0
keep if dup<2

*Setting uncontested elections and missing data for relevant variables to missing*
destring pev1 vot1 vv1 pv1, replace force


*Merging with 1935 election*

append using  "D:\Forskning og undervisning\Rural inequality and political effects Denmark\1935_election_DNSAP.dta"



*Collapsing by county and election*

collapse (sum) pev1 vot1 vv1 pv1 (mean) yr, by( county id)





*___________________________________________________________________________________*

*Merging with  data and initial data analysis*


*Merging of rural inequality datadata*

merge m:1 county using "D:\Forskning og undervisning\Rural inequality and political effects Denmark\rural_gini_1905_v2.dta"

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

*setting time frame*
xtset county_n id



*Generation of a turnout variable*
generate n_votes= vv1
replace n_votes= vot1 if vv1==0
replace n_votes=. if vot1==0 & vv1==0 

generate turnout= n_votes/pev1 



*Generation of  measure of  electoral support (share of votes)*
generate nazisupport= pv1/n_votes



*Random effects*
xtreg nazisupport c.rural_gini_1905##c.ruralshare  logpop i.id, re cluster ( county_n),  if yr>1915 

* Pooled OLS*
reg nazisupport c.rural_gini_1905##c.ruralshare  logpop i.id,  cluster ( county_n),  if yr>1915 


*Random effects excluding Copenhagen*
xtreg nazisupport c.rural_gini_1905##c.ruralshare  logpop i.id, re cluster ( county_n),  if yr>1915 & county_n!=8 


*Alternative measure of land inequality*
xtreg nazisupport c.share_held_20hrt_estates##c.ruralshare  logpop i.id, re cluster ( county_n),  if yr>1915 



*Random effects excluding Copenhagen*
xtreg nazisupport c.share_held_20hrt_estates##c.ruralshare  logpop i.id, re cluster ( county_n),  if yr>1915 & county_n!=8 



*Random effects no interaction
xtreg nazisupport rural_gini_1905 ruralshare  logpop i.id, re cluster ( county_n),  if yr>1915 



*Random effects no inteaction excluding Copenhagen*
xtreg nazisupport rural_gini_1905 ruralshare  logpop i.id, re cluster ( county_n),  if yr>1915 & county_n!=8 


reg nazisupport rural_gini_1905, cluster ( county_n)
reg nazisupport rural_gini_1905 logpop ruralshare, cluster ( county_n)
reg nazisupport c.rural_gini_1905##c.ruralshare logpop, cluster ( county_n)
reg nazisupport c.rural_gini_1905##c.ruralshare logpop  turnout, cluster ( county_n)

reg nazisupport c.rural_gini_1905##c.ruralshare logpop i.yr, cluster ( county_n), if yr>1920
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.04) .8401568))
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))

reg nazisupport rural_gini_1905 ruralshare logpop i.yr, cluster ( county_n), if yr>1920



xtpcse nazisupport c.rural_gini_1905##c.ruralshare logpop i.yr



*Excluding Copenhagen*
reg nazisupport rural_gini_1905 ruralshare logpop i.yr, cluster ( county_n), if yr>1920 & county_n!=8 
reg nazisupport c.rural_gini_1905##c.ruralshare logpop i.yr, cluster ( county_n), if yr>1920 & county_n!=8 
margins, dydx(ruralshare ) at(rural_gini_1905=(.6723197(0.04) .8401568))
marginsplot, level(90)xtitle (Land inequality (Gini)) ytitle (Marginal effect of rural share) yline(0, lstyle(grid) lcolor(gs8) lpattern(dash))graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") xlabel(, format(%9.2f)) ylabel(, format(%9.1f))
