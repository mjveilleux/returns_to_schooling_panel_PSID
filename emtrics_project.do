

*************************************************

*******		Housekeeping	************

*************************************************

* need to install: ssc install outreg2, replace
*ssc install xtoverid, replace
*ssc install ranktest, replace
clear all

********** Please change these directories **********************
*set log
log using "/Users/masonveilleux/Dropbox/Emetrics_Project/psid_code_data/log_emtrics_project.smcl",replace
*set working directory
cd "/Users/masonveilleux/Dropbox/Emetrics_Project/psid_code_data"
*input clean dataset from R
use "/Users/masonveilleux/Dropbox/Emetrics_Project/psid_code_data/clean_81_90.dta", clear
 
 
*************************************************\\\\************************************************* 
 
 
*add labels 
label variable yr_born "yearn born"
label variable year "time"
label variable mar "Marital Status, ref: Married"
label variable comp_edu "education complete"
label variable emp_1 "Employment Status in Last Survey, ref: Unemployed t-1"
label variable hrs_ann "hours worked yearly"
label variable income "yearly income"
label variable sex "Sex, ref: Male"
label variable age "Age"
label variable exp "Experience"
label variable wage "Wage"
label variable lnwage "Log Wage"

*create variables
gen exp2 = exp^2
gen age2 = age^2

*create for 1SLS
egen mean_emp_1 = mean(emp_1), by(id)
generate demeaned_emp_1 = emp_1- mean_emp_1
egen mean_mar = mean(mar), by(id)
generate demeaned_mar = mar- mean_mar
egen mean_exp = mean(exp), by(id)
egen mean_exp2 = mean(exp2), by(id)
egen mean_hrs_ann = mean(hrs_ann), by(id)



*set as panel 
tset id year, yearly  delta(1 year)


*************************************************

*******		ESTIMATION		************

*************************************************

*without controlling for time effects

reg lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, r
outreg2 using results, replace tex dec(3) ctitle(OLS)


*Pooled OLS 

reg lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85 year, vce(r) 
outreg2 using results, append tex dec(3) ctitle(Pooled OLS)



*Random Effects

 xtreg lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, re vce(r) theta
 outreg2 using results, append tex dec(3) ctitle(RE)

 
 
 *Fixed Effects
 xtreg lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, fe vce(r)
outreg2 using results, append tex dec(3) ctitle(FE)

 
 
*Hausman Taylor estimation
xthtaylor lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, endog(comp_edu85 hrs_ann mar occ85) vce(r)
outreg2 using results, append tex dec(3) ctitle(HT)
predict ind_effect_error_ht, u
predict idiosync_effect_error_ht, e



*When education is time variant (includes both negative and positive increases)
xthtaylor lnwage comp_edu emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, endog(comp_edu hrs_ann mar occ85) vce(r)
outreg2 using results, append tex dec(3) ctitle(HT Education TV) addnote("standard errors HC1 robust")


*AM estimation
xthtaylor lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, endog(comp_edu85 hrs_ann mar occ85) vce(r) am
outreg2 using results, append tex dec(3) ctitle(AM) addnote("standard errors HC1 robust")
predict ind_effect_error_am, u
predict idiosync_effect_error_am, e


*************************************************

*******		Robustness checks		************

*************************************************


*******Hausman test ********




*Random Effects
 quietly xtreg lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, re theta
estimates store reffects

 *Fixed Effects
 quietly xtreg lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, fe
estimates store feffects

hausman feffects reffects


**************First stage OLS to find validity of HT81 *************************
 
*1SLS for X2 

*employment status
quietly reg lnwage demeaned_emp_1 i.year
outreg2 using first_stage, replace tex dec(3) ctitle(Employment)  addstat("F-Stat",e(F),"Prob > F",e(p),"Degree of Freedom",e(df_r)) sideway


*marriage status
quietly reg lnwage demeaned_mar i.year
outreg2 using first_stage, append tex dec(3) ctitle(Single) addstat("F-Stat",e(F),"Prob > F",e(p),"Degree of Freedom",e(df_r)) sideway



*1SLS for Z2, regress school on all mean values of X1

quietly reg lnwage mean_exp mean_exp2 mean_hrs_ann i.year
outreg2 using first_stage, append tex dec(3) ctitle(Education) addstat("F-Stat",e(F),"Prob > F",e(p),"Degree of Freedom",e(df_r)) sideway



*****Check if overidentified******


*Random Effects
quietly xtreg lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, re vce(r) theta
xtoverid, r
outreg2 using overid_check, replace tex dec(3) ctitle(RE)  addstat(`r(j)',`r(jp)')


*Hausman Taylor estimation
quietly xthtaylor lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, endog(comp_edu85 hrs_ann mar occ85) vce(r)
xtoverid, r
outreg2 using overid_check, append tex dec(3) ctitle(HT) addstat(`r(j)',`r(jp)')


*AM estimation
quietly xthtaylor lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, endog(comp_edu85 hrs_ann mar occ85) vce(r) am
*xtoverid, r noi
*outreg2 using overid_check, append tex dec(3) ctitle(AM) addstat(`r(j)',`r(jp)')




****Are you BLUE? *****

*HT81 model
quietly xthtaylor lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, endog(comp_edu85 hrs_ann mar occ85) vce(r)


*test if paramenters are linear
	test (exp exp2 hrs_ann mar sex race85 region85 union_cov85 comp_edu85 occ85)
	*correlations for all variables with both idiosyncratic and individual effects error terms
	
*also check for Perfect Collinearity (mar~sex). (comp_edu85 ~ race, exp, exp2)
	cor ind_effect_error_ht idiosync_effect_error_ht lnwage comp_edu85 hrs_ann exp exp2 race85 occ85 union_cov85 region85 mar sex
	*strong indiv_effects relationship with occupation
	scatter occ85 ind_effect_error_ht, name(occ_ind1) title(Occupation ~ Individ. Effects)


*are error termsn expected value 0? (yes)
	mean(ind_effect_error_ht)
	mean(idiosync_effect_error_ht)

*Reset test
quietly xthtaylor lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, endog(comp_edu85 hrs_ann mar occ85) vce(r)
	predict yhat
	g yhat2=yhat^2
	g yhat3=yhat^3
	g yhat4=yhat^4
	xthtaylor lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85 yhat2 yhat3 yhat4, endog(comp_edu85 hrs_ann mar occ85) vce(r)
	* There appears to be some ommitted variable bias. This is strange since I am using a model nearly identical to published papers. 
	* I suspect this may be due to the method. Since it is reported that the HT81 model has some bias.	

*Heteroskedasticity test

	*no relationship
	scatter yhat idiosync_effect_error_ht, name(idio_hs_check) title(Predicted vales ~ Idiosyncratic Error)

	quietly reg yhat idiosync_effect_error_ht
	estat hettest


*Sargan-Hansen (not overidentified)
quietly xthtaylor lnwage comp_edu85 emp_1 hrs_ann sex mar exp exp2 race85 region85 occ85 union_cov85, endog(comp_edu85 hrs_ann mar occ85) vce(r)

xtoverid, r


*Weak instrument test (weakly identified)
xtoverid, r noi


*relationship with ability error term

scatter ind_effect_error_am lnwage || lfit ind_effect_error_am lnwage ,name(ind_effects_wage) title(Individual Effect ~ ln(Wage))


*strong negative correlation
scatter ind_effect_error_am yhat || lfit ind_effect_error_am yhat ,name(ind_effects_yhat) title(Individual Effect ~ Predicted Values)




*close log session
log close






































