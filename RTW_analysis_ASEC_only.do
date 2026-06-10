//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Analysis for "For What Benefit? State Right to Work Laws and Employer-Provided Retirement and Health Insurance Benefits"
/// Author: Alec P. Rhodes (aprhodes@purdue) 
/// 

/* Note: Code included only for the main results - based on individual-level ASEC data. Sample includes workers in new and almost RTW states
	Table 1.
	Table 2.
	Figure 2.
	Figure 4.
	Figure 5.
*/

			
* use an elegant graph scheme	
set scheme cleanplots	
	
			
* define control variables
gl demographics age age_squared racedummy2 racedummy3 ethnicity citizen educdummy1 educdummy2 educdummy3 educdummy4 educdummy5 children currenroll marrieddummy2 marrieddummy3

gl fullcontrols inddummy2 inddummy3 inddummy4 inddummy5 inddummy6 inddummy7 inddummy8 inddummy9 inddummy10 inddummy11 occdummy2 occdummy3 occdummy4 occdummy5 occdummy6 occdummy7 occdummy8 occdummy9 occdummy10 metropolitan

gl labormarket public_ly firmsizedummy1 firmsizedummy2 firmsizedummy3 firmsizedummy4




cd "[set your directory here]"


* load clean ASEC data
use "CLEAN_ASEC_DATA_RTW_ANALYSIS.dta", clear



**************************************************************************************************************************
* Figure 2. Unadjusted trends in worker compensation outcomes

gen rtwstatestatus=0 if rtw_change_==0 & rtw3==0
	replace rtwstatestatus=1 if (state==39 | state==29)
	replace rtwstatestatus=2 if rtw_change_==1
	replace rtwstatestatus=3 if rtw_change_==2
	
preserve
	
collapse (median) hrwages2_ly (mean) pensionoffer employerpaysprem emppayallprem if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0 & wageinc2_ly<212800, by(year rtwstatestatus)	

* Hourly Wages
gr twoway lpolyci hrwages2_ly year if rtwstatestatus==1, acolor(red%50) || lpolyci hrwages2_ly year if rtwstatestatus==2, lc(ltblue) acolor(ltblue%75) ||, ///
		  legend(order(2 "Almost RTW" 4 "New RTW")) legend(ring(0) position(12)) xline(2012) xtitle("") ytitle("Median Hourly Wage (in $1s)") ///
		  title("Hourly Wages")

gr save "Wage_Unadjusted_Trends_`c(current_date)'.gph", replace

* Retirement coverage
gr twoway lpolyci pensionoffer year if rtwstatestatus==1, acolor(red%50) || lpolyci pensionoffer year if rtwstatestatus==2, lc(ltblue) acolor(ltblue%75) ||, ///
		  legend(order(2 "Almost RTW" 4 "New RTW")) legend(ring(0) position(7)) xline(2012) ytitle("Share with Benefit (%)") xtitle("") ///
		  title("Retirement Coverage")

gr save "Pension_Unadjusted_Trends_`c(current_date)'.gph", replace
		  
* Health insurance coverage
gr twoway lpolyci employerpaysprem year if rtwstatestatus==1, acolor(red%50) || lpolyci employerpaysprem year if rtwstatestatus==2, lc(ltblue) acolor(ltblue%75) ||, /// 
		  legend(order(2 "Almost RTW" 4 "New RTW")) legend(ring(0) position(12)) xline(2012) ytitle("Share with Benefit (%)") xtitle("") ///
		  title("Health Insurance")

gr save "AnyHealthIns_Unadjusted_Trends_`c(current_date)'.gph", replace
		  
* Generous health ins. coverage
gr twoway lpolyci emppayallprem year if rtwstatestatus==1, acolor(red%50) || lpolyci emppayallprem year if rtwstatestatus==2, lc(ltblue) acolor(ltblue%75) ||, /// 
		  legend(order(2 "Almost RTW" 4 "New RTW")) legend(ring(0) position(12)) xline(2012) ytitle("Share with Benefit (%)") xtitle("") ///
		  title("Generous Health Ins.")

gr save "GenHealthIns_Unadjusted_Trends_`c(current_date)'.gph", replace


* Combine: All Four Outcomes
gr combine "Wage_Unadjusted_Trends_`c(current_date)'.gph" ///
		   "Pension_Unadjusted_Trends_`c(current_date)'.gph" ///
		   "AnyHealthIns_Unadjusted_Trends_`c(current_date)'.gph" ///
		   "GenHealthIns_Unadjusted_Trends_`c(current_date)'.gph", ///
		   row(2)
		   
gr save "Combined_All_Four_Outcomes_Unadjusted_Trends_`c(current_date)'.gph", replace
gr export "Combined_All_Four_Outcomes_Unadjusted_Trends_`c(current_date)'.png", replace
		   
		  
		  
restore



**************************************************************************************************************
* Table 2. TWFE Models Predicting Worker Compensation Outcomes

// main effects of RTW on worker compensation

* Log hourly wages - Main effect of RTW w/ and w/o unemployment rate in the models
glm hrwages2_ly i.rtw3 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0 & wageinc2_ly<212800, vce(cluster hhid_num) fam(gamma) link(log)

outreg2 using "TWFE_Almost_RTW_Model_Results_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label replace

* Retirement coverage - Main effect of RTW w/ and w/o unemployment rate in the models
logit pensionoffer i.rtw3 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

outreg2 using "TWFE_Almost_RTW_Model_Results_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append

* Health insurance coverage - Main effect of RTW w/ and w/o unemployment rate in the models
logit employerpaysprem i.rtw3 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

outreg2 using "TWFE_Almost_RTW_Model_Results_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append

* Generous health insurance coverage - Main effect of RTW w/ and w/o unemployment rate in the models
logit emppayallprem i.rtw3 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

outreg2 using "TWFE_Almost_RTW_Model_Results_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append


// interaction effects of RTW and state unemployment

* Log hourly wages - Core interaction between RTW and the state unemployment rate
glm hrwages2_ly i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0 & wageinc2_ly<212800, vce(cluster hhid_num) fam(gamma) link(log)

outreg2 using "TWFE_Almost_RTW_Model_Results_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append

* Retirement coverage - Core interaction between RTW and the state unemployment rate
logit pensionoffer i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

outreg2 using "TWFE_Almost_RTW_Model_Results_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append

* Health insurance coverage - Core interaction between RTW and the state unemployment rate
logit employerpaysprem i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

outreg2 using "TWFE_Almost_RTW_Model_Results_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append

* Generous health insurance coverage - Core interaction between RTW and the state unemployment rate
logit emppayallprem i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

outreg2 using "TWFE_Almost_RTW_Model_Results_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append




**************************************************************************************************************
* Figure 4. Marginal Effects Plots for the Main Interaction Effects on Worker Compensation Outcomes

* Log hourly wages - RTW and unemployment interactions
glm hrwages2_ly i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0 & wageinc2_ly<212800, vce(cluster hhid_num) fam(gamma) link(log)

margins i.rtw3, at(unemploymentrate_l2=(3(1)9)) atmeans // predicted margins
marginsplot, noci name(g1a_interaction, replace) title("") ytitle("Pred. Hourly Wage (in $1s)") xtitle("") ///
		     legend(position(1) ring(0)) legend(order(1 "non-RTW" 2 "RTW"))
			 
glm hrwages2_ly i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0 & wageinc2_ly<212800, vce(cluster hhid_num) fam(gamma) link(log)
			 
margins, dydx(rtw3) at(unemploymentrate_l2=(3(1)9)) atmeans // AME contrasts
marginsplot, name(g1b_interaction, replace) title("") ytitle("Average Marginal Effect") xtitle("") ///
			 legend(off) yline(0)
			 
gr combine g1a_interaction g1b_interaction, imargins(0) t1("Log Hourly Wages") col(2)
gr save "Combined_Wage_Interactions_`c(current_date)'.gph", replace
gr export "Combined_Wage_Interactions_`c(current_date)'.png", replace


// test of second differences
mlincom, clear

glm hrwages2_ly i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0 & wageinc2_ly<212800, vce(cluster hhid_num) fam(gamma) link(log)

margins, at(rtw3=(0 1) unemploymentrate_l2=(3 9)) post
mlincom (1-3) - (2-4)



* Retirement coverage - RTW and unemployment interactions
logit pensionoffer i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

margins i.rtw3, at(unemploymentrate_l2=(3(1)9)) atmeans // predicted margins
marginsplot, noci name(g2a_interaction, replace) title("") ytitle("Prob. Coverage (decimal)") xtitle("") ///
		     legend(position(1) ring(0)) legend(order(1 "non-RTW" 2 "RTW"))
			 
logit pensionoffer i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)
			 
margins, dydx(rtw3) at(unemploymentrate_l2=(3(1)9)) atmeans // AME contrasts
marginsplot, name(g2b_interaction, replace) title("") ytitle("Average Marginal Effect") xtitle("") ///
			 legend(off) yline(0)
			 
gr combine g2a_interaction g2b_interaction, imargins(0) t1("Retirement Coverage") col(2)
gr save "Combined_Retirement_Interactions_`c(current_date)'.gph", replace
gr export "Combined_Retirement_Interactions_`c(current_date)'.png", replace

// test of second differences
mlincom, clear

logit pensionoffer i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

margins, at(rtw3=(0 1) unemploymentrate_l2=(3 9)) post
mlincom (1-3) - (2-4)


* Health insurance coverage - RTW and unemployment interactions
logit employerpaysprem i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

margins i.rtw3, at(unemploymentrate_l2=(3(1)9)) atmeans // predicted margins
marginsplot, noci name(g3a_interaction, replace) title("") ytitle("Prob. Coverage (decimal)") xtitle("") ///
		     legend(position(1) ring(0)) legend(order(1 "non-RTW" 2 "RTW"))

logit employerpaysprem i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)			 
			 
margins, dydx(rtw3) at(unemploymentrate_l2=(3(1)9)) atmeans // AME contrasts
marginsplot, name(g3b_interaction, replace) title("") ytitle("Average Marginal Effect") xtitle("") ///
			 legend(off) yline(0)
			 
gr combine g3a_interaction g3b_interaction, imargins(0) t1("Health Insurance") col(2)
gr save "Combined_Healthins_Interactions_`c(current_date)'.gph", replace
gr export "Combined_Healthins_Interactions_`c(current_date)'.png", replace

// test of second differences
mlincom, clear

logit employerpaysprem i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

margins, at(rtw3=(0 1) unemploymentrate_l2=(3 9)) post
mlincom (1-3) - (2-4)



* Gen health ins. coverage - RTW and unemployment interactions
logit emppayallprem i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

margins i.rtw3, at(unemploymentrate_l2=(3(1)9)) atmeans // predicted margins
marginsplot, noci name(g4a_interaction, replace) title("") ytitle("Prob. Coverage (decimal)") xtitle("") ///
		     legend(position(1) ring(0)) legend(order(1 "non-RTW" 2 "RTW"))

logit emppayallprem i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)			 
			 
margins, dydx(rtw3) at(unemploymentrate_l2=(3(1)9)) atmeans // AME contrasts
marginsplot, name(g4b_interaction, replace) title("") ytitle("Average Marginal Effect") xtitle("") ///
			 legend(off) yline(0)
			 
gr combine g4a_interaction g4b_interaction, imargins(0) t1("Generous Health Ins.") col(2)
gr save "Combined_Genhealth_Interactions_`c(current_date)'.gph", replace
gr export "Combined_Genhealth_Interactions_`c(current_date)'.png", replace

		
// test of second differences
mlincom, clear

logit emppayallprem i.rtw3##c.unemploymentrate_l2 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

margins, at(rtw3=(0 1) unemploymentrate_l2=(3 9)) post
mlincom (1-3) - (2-4)

				
* Combine: All Four Outcomes		
gr combine "Combined_Wage_Interactions_`c(current_date)'.gph" "Combined_Retirement_Interactions_`c(current_date)'.gph" "Combined_Healthins_Interactions_`c(current_date)'.gph" "Combined_Genhealth_Interactions_`c(current_date)'.gph", col(1) xsize(6) ysize(9) b1("State Unemployment Rate (%)")

gr save "Combined_Interaction_Results_Four_Outcomes_`c(current_date)'.gph", replace
gr export "Combined_Interaction_Results_Four_Outcomes_`c(current_date)'.png", replace			 



		
**************************************************************************************************************************
* Table 1. Weighted Descriptive statistics

gl descriptives age age_squared racedummy1 racedummy2 racedummy3 ethnicity citizen educdummy1 educdummy2 educdummy3 educdummy4 educdummy5 educdummy6 children currenroll marrieddummy1 marrieddummy2 marrieddummy3 metropolitan ///
				inddummy1 inddummy2 inddummy3 inddummy4 inddummy5 inddummy6 inddummy7 inddummy8 inddummy9 inddummy10 inddummy11 occdummy1 occdummy2 occdummy3 occdummy4 occdummy5 occdummy6 occdummy7 occdummy8 occdummy9 occdummy10 ///
				public_ly firmsizedummy1 firmsizedummy2 firmsizedummy3 firmsizedummy4 firmsizedummy5 fulltime2_ly ///
				gsp_growth_l1 unemploymentrate_l2 ///
				govdem_l1 sharestatehouse_l1 sharestatesenate_l1 medicaid_exp_l1
				
* ID sample				
reg pensionoffer rtw3 i.state i.year $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly unemploymentrate_l2 if rtw_change_<2 & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)
gen sample=e(sample)	

gen hrwages3_ly=hrwages2_ly if wageinc2_ly<212800
gen ln_hrwages3_ly=ln_hrwages2_ly if wageinc2_ly<212800		
				
* outcomes by rtw state and year (new rtw states, non-rtw states, old rtw states; pre vs. post-rtw)
eststo newrtw_pre: estpost sum hrwages3_ly ln_hrwages3_ly pensionoffer employerpaysprem emppayallprem $descriptives [w=asecwt] if rtw_change_==1 & rtw3==0 & sample==1
eststo newrtw_post: estpost sum hrwages3_ly ln_hrwages3_ly pensionoffer employerpaysprem emppayallprem $descriptives [w=asecwt] if rtw_change_==1 & rtw3==1 & sample==1
eststo neverrtw_pre: estpost sum hrwages3_ly ln_hrwages3_ly pensionoffer employerpaysprem emppayallprem $descriptives [w=asecwt] if rtw_change_==0 & rtw3==0 & sample==1
eststo almostrtw_pre: estpost sum hrwages3_ly ln_hrwages3_ly pensionoffer employerpaysprem emppayallprem $descriptives [w=asecwt] if (state==39 | state==29) & sample==1
eststo oldrtw_post: estpost sum hrwages3_ly ln_hrwages3_ly pensionoffer employerpaysprem emppayallprem $descriptives [w=asecwt] if rtw_change_==2 & year>2000 & year<2020 & age>24 & age<65 & fedworker==0

esttab newrtw_pre newrtw_post neverrtw_pre almostrtw_pre oldrtw_post using "Descriptives_Benefits_`c(current_date)'.csv", ///
	   replace cells ("mean(pattern(1 1 1 1 1) fmt(2))") label title (Descriptives) nonumbers mtitles ("NewRTWpre" "NewRTWpost" "NotRTWpre" "AlmostRTWpre" "OldRTWpost") 
	   

	   
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Figure 5. Simulate the effects of RTW on Retirement Coverage in Indiana 
/// Interact rtw with state unemployment rate to allow effects to vary across states and years based on labor market conditions
/// Plot the trends for each individual state relative to a counterfactual of no RTW legislation

* separate out WI public
drop stateb
gen stateb=state
	replace stateb=57 if state==55 & classw>=3

drop rtw4	
gen rtw4=rtw3

gen selectindustry_ly=0
	replace selectindustry_ly=1 if construction_ly==1
	replace selectindustry_ly=2 if manuf_ly==1
	replace selectindustry_ly=3 if education_ly==1
	replace selectindustry_ly=4 if publicadmin_ly==1	

* Step 1. with RTW status as observed
logit pensionoffer c.rtw3##c.unemploymentrate_l2 i.year i.state $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)


foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_obs_in_`year' if stateb==18 & year==`year'
	egen pr_obs_in_2_`year'=mean(pr_obs_in_`year')
	drop pr_obs_in_`year'
	ren pr_obs_in_2_`year' pr_obs_in_`year' 
predict pr_obs_ky_`year' if stateb==21 & year==`year'
	egen pr_obs_ky_2_`year'=mean(pr_obs_ky_`year')
	drop pr_obs_ky_`year'
	ren pr_obs_ky_2_`year' pr_obs_ky_`year'
predict pr_obs_mi_`year' if stateb==26 & year==`year'
	egen pr_obs_mi_2_`year'=mean(pr_obs_mi_`year')
	drop pr_obs_mi_`year'
	ren pr_obs_mi_2_`year' pr_obs_mi_`year'
predict pr_obs_wi_`year' if stateb==57 & year==`year'
	egen pr_obs_wi_2_`year'=mean(pr_obs_wi_`year')
	drop pr_obs_wi_`year'
	ren pr_obs_wi_2_`year' pr_obs_wi_`year'
predict pr_obs_wv_`year' if stateb==54 & year==`year'
	egen pr_obs_wv_2_`year'=mean(pr_obs_wv_`year')
	drop pr_obs_wv_`year'
	ren pr_obs_wv_2_`year' pr_obs_wv_`year'
predict pr_obs_oh_`year' if stateb==39 & year==`year' & public_ly==1 // limit to public sector in last year (public sector bargaining law)
	egen pr_obs_oh_2_`year'=mean(pr_obs_oh_`year')
	drop pr_obs_oh_`year'
	ren pr_obs_oh_2_`year' pr_obs_oh_`year'
predict pr_obs_mo_`year' if stateb==29 & year==`year'
	egen pr_obs_mo_2_`year'=mean(pr_obs_mo_`year')
	drop pr_obs_mo_`year'
	ren pr_obs_mo_2_`year' pr_obs_mo_`year'
	
					}
					
foreach state in in ky mi wi wv oh mo {					
gen pr_obs_`state'=pr_obs_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_obs_`state'=pr_obs_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_obs_*_2*

* Step 2. Replace rtw with counterfactual RTW for states that became RTW
* 		  Allow effects to vary across states
replace rtw3=0 if stateb==18 & year>=2013
replace rtw3=0 if stateb==21 & year>=2018
replace rtw3=0 if stateb==26 & year>=2014
replace rtw3=0 if stateb==54 & year>=2018					
replace rtw3=0 if stateb==57 & year>=2012

foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_ctf_in_`year' if stateb==18 & year==`year'
	egen pr_ctf_in_2_`year'=mean(pr_ctf_in_`year')
	drop pr_ctf_in_`year'
	ren pr_ctf_in_2_`year' pr_ctf_in_`year'
predict pr_ctf_ky_`year' if stateb==21 & year==`year'
	egen pr_ctf_ky_2_`year'=mean(pr_ctf_ky_`year')
	drop pr_ctf_ky_`year'
	ren pr_ctf_ky_2_`year' pr_ctf_ky_`year'
predict pr_ctf_mi_`year' if stateb==26 & year==`year'
	egen pr_ctf_mi_2_`year'=mean(pr_ctf_mi_`year')
	drop pr_ctf_mi_`year'
	ren pr_ctf_mi_2_`year' pr_ctf_mi_`year'
predict pr_ctf_wi_`year' if stateb==57 & year==`year'
	egen pr_ctf_wi_2_`year'=mean(pr_ctf_wi_`year')
	drop pr_ctf_wi_`year'
	ren pr_ctf_wi_2_`year' pr_ctf_wi_`year'
predict pr_ctf_wv_`year' if stateb==54 & year==`year'
	egen pr_ctf_wv_2_`year'=mean(pr_ctf_wv_`year')
	drop pr_ctf_wv_`year'
	ren pr_ctf_wv_2_`year' pr_ctf_wv_`year'

	
					}

foreach state in in ky mi wi wv {					
gen pr_ctf_`state'=pr_ctf_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_ctf_`state'=pr_ctf_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_ctf_*_2*


replace rtw3=rtw4
		
* Step 3. Predict for OH + MO given the average within-state response to RTW laws
*         This is the counterfactual estimate for OH + MO
logit pensionoffer c.rtw3##c.unemploymentrate_l2 i.year i.state $demographics $labormarket $fullcontrols medicaid_exp_l1 fulltime2_ly if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

* Step 2. Replace rtw with counterfactual RTW for states that became RTW
replace rtw3=1 if stateb==39 & year>=2012
replace rtw3=1 if stateb==29 & year>=2018

foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_ctf_oh_`year' if stateb==39 & year==`year' & public_ly==1 // limit to public sector in last year (public sector bargaining law)
	egen pr_ctf_oh_2_`year'=mean(pr_ctf_oh_`year')
	drop pr_ctf_oh_`year'
	ren pr_ctf_oh_2_`year' pr_ctf_oh_`year'
predict pr_ctf_mo_`year' if stateb==29 & year==`year'
	egen pr_ctf_mo_2_`year'=mean(pr_ctf_mo_`year')
	drop pr_ctf_mo_`year'
	ren pr_ctf_mo_2_`year' pr_ctf_mo_`year'
	
					}

foreach state in oh mo {					
gen pr_ctf_`state'=pr_ctf_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_ctf_`state'=pr_ctf_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_ctf_*_2*

gen pr_observed=pr_obs_in if stateb==18
	replace pr_observed=pr_obs_ky if stateb==21
	replace pr_observed=pr_obs_mi if stateb==26
	replace pr_observed=pr_obs_wi if stateb==57
	replace pr_observed=pr_obs_wv if stateb==54
	replace pr_observed=pr_obs_oh if stateb==39
	replace pr_observed=pr_obs_mo if stateb==29
	
	drop pr_obs_*
	
gen pr_counterfactual=pr_ctf_in if stateb==18 & year>=2013
	replace pr_counterfactual=pr_ctf_ky if stateb==21 & year>=2018
	replace pr_counterfactual=pr_ctf_mi if stateb==26 & year>=2014
	replace pr_counterfactual=pr_ctf_wi if stateb==57 & year>=2012
	replace pr_counterfactual=pr_ctf_wv if stateb==54 & year>=2018
	replace pr_counterfactual=pr_ctf_oh if stateb==39 & year>=2012
	replace pr_counterfactual=pr_ctf_mo if stateb==29 & year>=2018
	
	drop pr_ctf_*
	
replace rtw3=rtw4

preserve	
collapse pr_observed pr_counterfactual if stateb==57 | stateb==18 | stateb==21 | stateb==26 | stateb==54 | stateb==39 | stateb==29, by(stateb year)

replace pr_counterfactual=pr_observed if year==2011 & (stateb==57 | stateb==39) // extend counterfactual line so that it connects
replace pr_counterfactual=pr_observed if year==2012 & (stateb==18)
replace pr_counterfactual=pr_observed if year==2013 & (stateb==26)

xtset stateb year	

la def stateblab 57"WI"18"IN"21"KY"26"MI"54"WV"39"OH"29"MO"
la val stateb stateblab
	   
gr twoway lowess pr_observed year if state==18 & year>2004 & year<2020, lp(shortdash) || lowess pr_counterfactual year if stateb==18 & year>2004 & year<2020 ||, ///
	   legend(order(1 "Observed" 2 "Counterfactual")) ///
	   legend(position(1) ring(0)) ///
	   xtitle("") ytitle("Pr(Retirement)") ///
	   title("Indiana")
	   
	   gr save "counterfactual_retirement_in.gph", replace
	  
		
restore		


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Figure 5. Simulate the effects of RTW on generous health ins in Indiana
/// Interact rtw with state unemployment rate to allow effects to vary across states and years based on labor market conditions
/// Plot the trends for each individual state relative to a counterfactual of no RTW legislation

drop pr_counterfactual pr_observed

* Step 1. with RTW status as observed
logit emppayallprem c.rtw3##c.unemploymentrate_l2 i.year i.state $demographics $labormarket $fullcontrols fulltime2_ly medicaid_exp_l1 if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)


foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_obs_in_`year' if stateb==18 & year==`year'
	egen pr_obs_in_2_`year'=mean(pr_obs_in_`year')
	drop pr_obs_in_`year'
	ren pr_obs_in_2_`year' pr_obs_in_`year' 
predict pr_obs_ky_`year' if stateb==21 & year==`year'
	egen pr_obs_ky_2_`year'=mean(pr_obs_ky_`year')
	drop pr_obs_ky_`year'
	ren pr_obs_ky_2_`year' pr_obs_ky_`year'
predict pr_obs_mi_`year' if stateb==26 & year==`year'
	egen pr_obs_mi_2_`year'=mean(pr_obs_mi_`year')
	drop pr_obs_mi_`year'
	ren pr_obs_mi_2_`year' pr_obs_mi_`year'
predict pr_obs_wi_`year' if stateb==57 & year==`year'
	egen pr_obs_wi_2_`year'=mean(pr_obs_wi_`year')
	drop pr_obs_wi_`year'
	ren pr_obs_wi_2_`year' pr_obs_wi_`year'
predict pr_obs_wv_`year' if stateb==54 & year==`year'
	egen pr_obs_wv_2_`year'=mean(pr_obs_wv_`year')
	drop pr_obs_wv_`year'
	ren pr_obs_wv_2_`year' pr_obs_wv_`year'
predict pr_obs_oh_`year' if stateb==39 & year==`year' & public_ly==1 // limit to public sector in last year (public sector bargaining law)
	egen pr_obs_oh_2_`year'=mean(pr_obs_oh_`year')
	drop pr_obs_oh_`year'
	ren pr_obs_oh_2_`year' pr_obs_oh_`year'
predict pr_obs_mo_`year' if stateb==29 & year==`year'
	egen pr_obs_mo_2_`year'=mean(pr_obs_mo_`year')
	drop pr_obs_mo_`year'
	ren pr_obs_mo_2_`year' pr_obs_mo_`year'
	
					}
					
foreach state in in ky mi wi wv oh mo {					
gen pr_obs_`state'=pr_obs_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_obs_`state'=pr_obs_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_obs_*_2*

* Step 2. Replace rtw with counterfactual RTW for states that became RTW
* 		  Allow effects to vary across states
replace rtw3=0 if stateb==18 & year>=2013
replace rtw3=0 if stateb==21 & year>=2018
replace rtw3=0 if stateb==26 & year>=2014
replace rtw3=0 if stateb==54 & year>=2018					
replace rtw3=0 if stateb==57 & year>=2012

foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_ctf_in_`year' if stateb==18 & year==`year'
	egen pr_ctf_in_2_`year'=mean(pr_ctf_in_`year')
	drop pr_ctf_in_`year'
	ren pr_ctf_in_2_`year' pr_ctf_in_`year'
predict pr_ctf_ky_`year' if stateb==21 & year==`year'
	egen pr_ctf_ky_2_`year'=mean(pr_ctf_ky_`year')
	drop pr_ctf_ky_`year'
	ren pr_ctf_ky_2_`year' pr_ctf_ky_`year'
predict pr_ctf_mi_`year' if stateb==26 & year==`year'
	egen pr_ctf_mi_2_`year'=mean(pr_ctf_mi_`year')
	drop pr_ctf_mi_`year'
	ren pr_ctf_mi_2_`year' pr_ctf_mi_`year'
predict pr_ctf_wi_`year' if stateb==57 & year==`year'
	egen pr_ctf_wi_2_`year'=mean(pr_ctf_wi_`year')
	drop pr_ctf_wi_`year'
	ren pr_ctf_wi_2_`year' pr_ctf_wi_`year'
predict pr_ctf_wv_`year' if stateb==54 & year==`year'
	egen pr_ctf_wv_2_`year'=mean(pr_ctf_wv_`year')
	drop pr_ctf_wv_`year'
	ren pr_ctf_wv_2_`year' pr_ctf_wv_`year'

	
					}

foreach state in in ky mi wi wv {					
gen pr_ctf_`state'=pr_ctf_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_ctf_`state'=pr_ctf_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_ctf_*_2*


replace rtw3=rtw4
		
* Step 3. Predict for OH + MO given the average within-state response to RTW laws
*         This is the counterfactual estimate for OH + MO
logit emppayallprem c.rtw3##c.unemploymentrate_l2 i.year i.state $demographics $labormarket $fullcontrols fulltime2_ly medicaid_exp_l1 if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

* Step 2. Replace rtw with counterfactual RTW for states that became RTW
replace rtw3=1 if stateb==39 & year>=2012
replace rtw3=1 if stateb==29 & year>=2018

foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_ctf_oh_`year' if stateb==39 & year==`year' & public_ly==1 // limit to public sector in last year (public sector bargaining law)
	egen pr_ctf_oh_2_`year'=mean(pr_ctf_oh_`year')
	drop pr_ctf_oh_`year'
	ren pr_ctf_oh_2_`year' pr_ctf_oh_`year'
predict pr_ctf_mo_`year' if stateb==29 & year==`year'
	egen pr_ctf_mo_2_`year'=mean(pr_ctf_mo_`year')
	drop pr_ctf_mo_`year'
	ren pr_ctf_mo_2_`year' pr_ctf_mo_`year'
	
					}

foreach state in oh mo {					
gen pr_ctf_`state'=pr_ctf_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_ctf_`state'=pr_ctf_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_ctf_*_2*

gen pr_observed=pr_obs_in if stateb==18
	replace pr_observed=pr_obs_ky if stateb==21
	replace pr_observed=pr_obs_mi if stateb==26
	replace pr_observed=pr_obs_wi if stateb==57
	replace pr_observed=pr_obs_wv if stateb==54
	replace pr_observed=pr_obs_oh if stateb==39
	replace pr_observed=pr_obs_mo if stateb==29
	
	drop pr_obs_*
	
gen pr_counterfactual=pr_ctf_in if stateb==18 & year>=2013
	replace pr_counterfactual=pr_ctf_ky if stateb==21 & year>=2018
	replace pr_counterfactual=pr_ctf_mi if stateb==26 & year>=2014
	replace pr_counterfactual=pr_ctf_wi if stateb==57 & year>=2012
	replace pr_counterfactual=pr_ctf_wv if stateb==54 & year>=2018
	replace pr_counterfactual=pr_ctf_oh if stateb==39 & year>=2012
	replace pr_counterfactual=pr_ctf_mo if stateb==29 & year>=2018
	
	drop pr_ctf_*
	
replace rtw3=rtw4

preserve	
collapse pr_observed pr_counterfactual if stateb==57 | stateb==18 | stateb==21 | stateb==26 | stateb==54 | stateb==39 | stateb==29, by(stateb year)

replace pr_counterfactual=pr_observed if year==2011 & (stateb==57 | stateb==39) // extend counterfactual line so that it connects
replace pr_counterfactual=pr_observed if year==2012 & (stateb==18)
replace pr_counterfactual=pr_observed if year==2013 & (stateb==26)

xtset stateb year	

la def stateblab 57"WI"18"IN"21"KY"26"MI"54"WV"39"OH"29"MO"
la val stateb stateblab

	   
gr twoway lowess pr_observed year if state==18 & year>2004 & year<2020, lp(shortdash) || lowess pr_counterfactual year if stateb==18 & year>2004 & year<2020 ||, ///
	   legend(order(1 "Observed" 2 "Counterfactual")) ///
	   legend(position(1) ring(0)) ///
	   xtitle("") ytitle("Pr(Generous Health Ins.)") ///
	   title("Indiana")
	   
	   gr save "counterfactual_genhealthins_in.gph", replace
		
restore		



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Figure 5. Simulate the effects of RTW on health insurance coverage in Indiana
/// Interact rtw with state unemployment rate to allow effects to vary across states and years based on labor market conditions
/// Plot the trends for each individual state relative to a counterfactual of no RTW legislation

drop pr_counterfactual pr_observed

* Step 1. with RTW status as observed
logit employerpaysprem c.rtw3##c.unemploymentrate_l2 i.year i.state $demographics $labormarket $fullcontrols fulltime2_ly medicaid_exp_l1 if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)


foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_obs_in_`year' if stateb==18 & year==`year'
	egen pr_obs_in_2_`year'=mean(pr_obs_in_`year')
	drop pr_obs_in_`year'
	ren pr_obs_in_2_`year' pr_obs_in_`year' 
predict pr_obs_ky_`year' if stateb==21 & year==`year'
	egen pr_obs_ky_2_`year'=mean(pr_obs_ky_`year')
	drop pr_obs_ky_`year'
	ren pr_obs_ky_2_`year' pr_obs_ky_`year'
predict pr_obs_mi_`year' if stateb==26 & year==`year'
	egen pr_obs_mi_2_`year'=mean(pr_obs_mi_`year')
	drop pr_obs_mi_`year'
	ren pr_obs_mi_2_`year' pr_obs_mi_`year'
predict pr_obs_wi_`year' if stateb==57 & year==`year'
	egen pr_obs_wi_2_`year'=mean(pr_obs_wi_`year')
	drop pr_obs_wi_`year'
	ren pr_obs_wi_2_`year' pr_obs_wi_`year'
predict pr_obs_wv_`year' if stateb==54 & year==`year'
	egen pr_obs_wv_2_`year'=mean(pr_obs_wv_`year')
	drop pr_obs_wv_`year'
	ren pr_obs_wv_2_`year' pr_obs_wv_`year'
predict pr_obs_oh_`year' if stateb==39 & year==`year' & public_ly==1 // limit to public sector in last year (public sector bargaining law)
	egen pr_obs_oh_2_`year'=mean(pr_obs_oh_`year')
	drop pr_obs_oh_`year'
	ren pr_obs_oh_2_`year' pr_obs_oh_`year'
predict pr_obs_mo_`year' if stateb==29 & year==`year'
	egen pr_obs_mo_2_`year'=mean(pr_obs_mo_`year')
	drop pr_obs_mo_`year'
	ren pr_obs_mo_2_`year' pr_obs_mo_`year'
	
					}
					
foreach state in in ky mi wi wv oh mo {					
gen pr_obs_`state'=pr_obs_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_obs_`state'=pr_obs_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_obs_*_2*

* Step 2. Replace rtw with counterfactual RTW for states that became RTW
* 		  Allow effects to vary across states
replace rtw3=0 if stateb==18 & year>=2013
replace rtw3=0 if stateb==21 & year>=2018
replace rtw3=0 if stateb==26 & year>=2014
replace rtw3=0 if stateb==54 & year>=2018					
replace rtw3=0 if stateb==57 & year>=2012

foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_ctf_in_`year' if stateb==18 & year==`year'
	egen pr_ctf_in_2_`year'=mean(pr_ctf_in_`year')
	drop pr_ctf_in_`year'
	ren pr_ctf_in_2_`year' pr_ctf_in_`year'
predict pr_ctf_ky_`year' if stateb==21 & year==`year'
	egen pr_ctf_ky_2_`year'=mean(pr_ctf_ky_`year')
	drop pr_ctf_ky_`year'
	ren pr_ctf_ky_2_`year' pr_ctf_ky_`year'
predict pr_ctf_mi_`year' if stateb==26 & year==`year'
	egen pr_ctf_mi_2_`year'=mean(pr_ctf_mi_`year')
	drop pr_ctf_mi_`year'
	ren pr_ctf_mi_2_`year' pr_ctf_mi_`year'
predict pr_ctf_wi_`year' if stateb==57 & year==`year'
	egen pr_ctf_wi_2_`year'=mean(pr_ctf_wi_`year')
	drop pr_ctf_wi_`year'
	ren pr_ctf_wi_2_`year' pr_ctf_wi_`year'
predict pr_ctf_wv_`year' if stateb==54 & year==`year'
	egen pr_ctf_wv_2_`year'=mean(pr_ctf_wv_`year')
	drop pr_ctf_wv_`year'
	ren pr_ctf_wv_2_`year' pr_ctf_wv_`year'

	
					}

foreach state in in ky mi wi wv {					
gen pr_ctf_`state'=pr_ctf_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_ctf_`state'=pr_ctf_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_ctf_*_2*


replace rtw3=rtw4
		
* Step 3. Predict for OH + MO given the average within-state response to RTW laws
*         This is the counterfactual estimate for OH + MO
logit employerpaysprem c.rtw3##c.unemploymentrate_l2 i.year i.state $demographics $labormarket $fullcontrols fulltime2_ly medicaid_exp_l1 if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0, vce(cluster hhid_num)

* Step 2. Replace rtw with counterfactual RTW for states that became RTW
replace rtw3=1 if stateb==39 & year>=2012
replace rtw3=1 if stateb==29 & year>=2018

foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_ctf_oh_`year' if stateb==39 & year==`year' & public_ly==1 // limit to public sector in last year (public sector bargaining law)
	egen pr_ctf_oh_2_`year'=mean(pr_ctf_oh_`year')
	drop pr_ctf_oh_`year'
	ren pr_ctf_oh_2_`year' pr_ctf_oh_`year'
predict pr_ctf_mo_`year' if stateb==29 & year==`year'
	egen pr_ctf_mo_2_`year'=mean(pr_ctf_mo_`year')
	drop pr_ctf_mo_`year'
	ren pr_ctf_mo_2_`year' pr_ctf_mo_`year'
	
					}

foreach state in oh mo {					
gen pr_ctf_`state'=pr_ctf_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_ctf_`state'=pr_ctf_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_ctf_*_2*

gen pr_observed=pr_obs_in if stateb==18
	replace pr_observed=pr_obs_ky if stateb==21
	replace pr_observed=pr_obs_mi if stateb==26
	replace pr_observed=pr_obs_wi if stateb==57
	replace pr_observed=pr_obs_wv if stateb==54
	replace pr_observed=pr_obs_oh if stateb==39
	replace pr_observed=pr_obs_mo if stateb==29
	
	drop pr_obs_*
	
gen pr_counterfactual=pr_ctf_in if stateb==18 & year>=2013
	replace pr_counterfactual=pr_ctf_ky if stateb==21 & year>=2018
	replace pr_counterfactual=pr_ctf_mi if stateb==26 & year>=2014
	replace pr_counterfactual=pr_ctf_wi if stateb==57 & year>=2012
	replace pr_counterfactual=pr_ctf_wv if stateb==54 & year>=2018
	replace pr_counterfactual=pr_ctf_oh if stateb==39 & year>=2012
	replace pr_counterfactual=pr_ctf_mo if stateb==29 & year>=2018
	
	drop pr_ctf_*
	
replace rtw3=rtw4

preserve	
collapse pr_observed pr_counterfactual if stateb==57 | stateb==18 | stateb==21 | stateb==26 | stateb==54 | stateb==39 | stateb==29, by(stateb year)

replace pr_counterfactual=pr_observed if year==2011 & (stateb==57 | stateb==39) // extend counterfactual line so that it connects
replace pr_counterfactual=pr_observed if year==2012 & (stateb==18)
replace pr_counterfactual=pr_observed if year==2013 & (stateb==26)

xtset stateb year	

la def stateblab 57"WI"18"IN"21"KY"26"MI"54"WV"39"OH"29"MO"
la val stateb stateblab
	
	   
gr twoway lowess pr_observed year if state==18 & year>2004 & year<2020, lp(shortdash) || lowess pr_counterfactual year if stateb==18 & year>2004 & year<2020 ||, ///
	   legend(order(1 "Observed" 2 "Counterfactual")) ///
	   legend(position(11) ring(0)) ///
	   xtitle("") ytitle("Pr(Health Insurance Cov.)") ///
	   title("Indiana")
	   
	   gr save "counterfactual_healthinscov_in.gph", replace
	  
		
restore	



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Figure 5. Simulate the effects of RTW on wages for Indiana
/// Interact rtw with state unemployment rate to allow effects to vary across states and years based on labor market conditions
/// Plot the trends for each individual state relative to a counterfactual of no RTW legislation

drop pr_counterfactual pr_observed

* Step 1. with RTW status as observed
glm hrwages2_ly c.rtw3##c.unemploymentrate_l2 i.year i.state $demographics $labormarket $fullcontrols fulltime2_ly medicaid_exp_l1 if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0 & wageinc2_ly<212800, vce(cluster hhid_num) family(gamma) link(log)


foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_obs_in_`year' if stateb==18 & year==`year'
	egen pr_obs_in_2_`year'=median(pr_obs_in_`year')
	drop pr_obs_in_`year'
	ren pr_obs_in_2_`year' pr_obs_in_`year' 
predict pr_obs_ky_`year' if stateb==21 & year==`year'
	egen pr_obs_ky_2_`year'=median(pr_obs_ky_`year')
	drop pr_obs_ky_`year'
	ren pr_obs_ky_2_`year' pr_obs_ky_`year'
predict pr_obs_mi_`year' if stateb==26 & year==`year'
	egen pr_obs_mi_2_`year'=median(pr_obs_mi_`year')
	drop pr_obs_mi_`year'
	ren pr_obs_mi_2_`year' pr_obs_mi_`year'
predict pr_obs_wi_`year' if stateb==57 & year==`year'
	egen pr_obs_wi_2_`year'=median(pr_obs_wi_`year')
	drop pr_obs_wi_`year'
	ren pr_obs_wi_2_`year' pr_obs_wi_`year'
predict pr_obs_wv_`year' if stateb==54 & year==`year'
	egen pr_obs_wv_2_`year'=median(pr_obs_wv_`year')
	drop pr_obs_wv_`year'
	ren pr_obs_wv_2_`year' pr_obs_wv_`year'
predict pr_obs_oh_`year' if stateb==39 & year==`year' & public_ly==1 // limit to public sector in last year (public sector bargaining law)
	egen pr_obs_oh_2_`year'=median(pr_obs_oh_`year')
	drop pr_obs_oh_`year'
	ren pr_obs_oh_2_`year' pr_obs_oh_`year'
predict pr_obs_mo_`year' if stateb==29 & year==`year'
	egen pr_obs_mo_2_`year'=median(pr_obs_mo_`year')
	drop pr_obs_mo_`year'
	ren pr_obs_mo_2_`year' pr_obs_mo_`year'
	
					}
					
foreach state in in ky mi wi wv oh mo {					
gen pr_obs_`state'=pr_obs_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_obs_`state'=pr_obs_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_obs_*_2*

* Step 2. Replace rtw with counterfactual RTW for states that became RTW
* 		  Allow effects to vary across states
replace rtw3=0 if stateb==18 & year>=2013
replace rtw3=0 if stateb==21 & year>=2018
replace rtw3=0 if stateb==26 & year>=2014
replace rtw3=0 if stateb==54 & year>=2018					
replace rtw3=0 if stateb==57 & year>=2012

foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_ctf_in_`year' if stateb==18 & year==`year'
	egen pr_ctf_in_2_`year'=median(pr_ctf_in_`year')
	drop pr_ctf_in_`year'
	ren pr_ctf_in_2_`year' pr_ctf_in_`year'
predict pr_ctf_ky_`year' if stateb==21 & year==`year'
	egen pr_ctf_ky_2_`year'=median(pr_ctf_ky_`year')
	drop pr_ctf_ky_`year'
	ren pr_ctf_ky_2_`year' pr_ctf_ky_`year'
predict pr_ctf_mi_`year' if stateb==26 & year==`year'
	egen pr_ctf_mi_2_`year'=median(pr_ctf_mi_`year')
	drop pr_ctf_mi_`year'
	ren pr_ctf_mi_2_`year' pr_ctf_mi_`year'
predict pr_ctf_wi_`year' if stateb==57 & year==`year'
	egen pr_ctf_wi_2_`year'=median(pr_ctf_wi_`year')
	drop pr_ctf_wi_`year'
	ren pr_ctf_wi_2_`year' pr_ctf_wi_`year'
predict pr_ctf_wv_`year' if stateb==54 & year==`year'
	egen pr_ctf_wv_2_`year'=median(pr_ctf_wv_`year')
	drop pr_ctf_wv_`year'
	ren pr_ctf_wv_2_`year' pr_ctf_wv_`year'

	
					}

foreach state in in ky mi wi wv {					
gen pr_ctf_`state'=pr_ctf_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_ctf_`state'=pr_ctf_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_ctf_*_2*


replace rtw3=rtw4
		
* Step 3. Predict for OH + MO given the average within-state response to RTW laws
*         This is the counterfactual estimate for OH + MO
glm hrwages2_ly c.rtw3##c.unemploymentrate_l2 i.year i.state $demographics $labormarket $fullcontrols fulltime2_ly medicaid_exp_l1 if (rtw_change_==1 | state==39 | state==29) & year>2000 & year<2020 & age>24 & age<65 & fedworker==0 & wageinc2_ly<212800, vce(cluster hhid_num) family(gamma) link(log)

* Step 2. Replace rtw with counterfactual RTW for states that became RTW
replace rtw3=1 if stateb==39 & year>=2012
replace rtw3=1 if stateb==29 & year>=2018

foreach year in 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
predict pr_ctf_oh_`year' if stateb==39 & year==`year' & public_ly==1 // limit to public sector in last year (public sector bargaining law)
	egen pr_ctf_oh_2_`year'=median(pr_ctf_oh_`year')
	drop pr_ctf_oh_`year'
	ren pr_ctf_oh_2_`year' pr_ctf_oh_`year'
predict pr_ctf_mo_`year' if stateb==29 & year==`year'
	egen pr_ctf_mo_2_`year'=median(pr_ctf_mo_`year')
	drop pr_ctf_mo_`year'
	ren pr_ctf_mo_2_`year' pr_ctf_mo_`year'
	
					}

foreach state in oh mo {					
gen pr_ctf_`state'=pr_ctf_`state'_2001 if year==2001
	
foreach year in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 {
	
	replace pr_ctf_`state'=pr_ctf_`state'_`year' if year==`year'
		
		}	
	
	}
	
drop pr_ctf_*_2*

gen pr_observed=pr_obs_in if stateb==18
	replace pr_observed=pr_obs_ky if stateb==21
	replace pr_observed=pr_obs_mi if stateb==26
	replace pr_observed=pr_obs_wi if stateb==57
	replace pr_observed=pr_obs_wv if stateb==54
	replace pr_observed=pr_obs_oh if stateb==39
	replace pr_observed=pr_obs_mo if stateb==29
	
	drop pr_obs_*
	
gen pr_counterfactual=pr_ctf_in if stateb==18 & year>=2013
	replace pr_counterfactual=pr_ctf_ky if stateb==21 & year>=2018
	replace pr_counterfactual=pr_ctf_mi if stateb==26 & year>=2014
	replace pr_counterfactual=pr_ctf_wi if stateb==57 & year>=2012
	replace pr_counterfactual=pr_ctf_wv if stateb==54 & year>=2018
	replace pr_counterfactual=pr_ctf_oh if stateb==39 & year>=2012
	replace pr_counterfactual=pr_ctf_mo if stateb==29 & year>=2018
	
	drop pr_ctf_*
	
replace rtw3=rtw4

preserve	
collapse pr_observed pr_counterfactual if stateb==57 | stateb==18 | stateb==21 | stateb==26 | stateb==54 | stateb==39 | stateb==29, by(stateb year)

replace pr_counterfactual=pr_observed if year==2011 & (stateb==57 | stateb==39) // extend counterfactual line so that it connects
replace pr_counterfactual=pr_observed if year==2012 & (stateb==18)
replace pr_counterfactual=pr_observed if year==2013 & (stateb==26)

xtset stateb year	

la def stateblab 57"WI"18"IN"21"KY"26"MI"54"WV"39"OH"29"MO"
la val stateb stateblab
	   
gr twoway lowess pr_observed year if state==18 & year>2004 & year<2020, lp(shortdash) || lowess pr_counterfactual year if stateb==18 & year>2004 & year<2020 ||, ///
	   legend(order(1 "Observed" 2 "Counterfactual")) ///
	   legend(position(11) ring(0)) ///
	   xtitle("") ytitle("Pr(Hourly Wage)") ///
	   title("Indiana")
	   
	   gr save "counterfactual_hourlywages_in.gph", replace
	   
		
restore	



************************************************************************************
* Graph Combine Put all outcomes for IN together into one graph

* indiana
gr combine "counterfactual_hourlywages_in.gph" "counterfactual_retirement_in.gph" "counterfactual_healthinscov_in.gph" "counterfactual_genhealthins_in.gph", ///
	       cols(2)
		   
gr export "counterfactual_all_four_outcomes_indiana.png", replace







	
		
		