clear

cd ~\TPV\replication_files_TPV
use Banks_and_Violent_Conflict_Prov_Year, replace

*DESCRIPTIVE
su nonstatebranchpercap nonstatedeposit_milUSDpercap statebranchpercap statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl nonstateceditperGDP laglnGTDcivilian laglnGTD_fatality laglnGTD_fatal_event laglnkibris if year>=1988 & year<=2001 & southeast_24==1 & gdp_const_tl_bil_log!=.
su nonstatebranchpercap nonstatedeposit_milUSDpercap statebranchpercap statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl nonstateceditperGDP defense_percent laglnGTDcivilian laglnGTD_fatality laglnGTD_fatal_event laglnttov_all laglnkibris if year>=2005 & year<=2018 & southeast_24==1
su nonstatebranchpercap nonstatedeposit_milUSDpercap statebranchpercap statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl nonstateceditperGDP laglnGTDcivilian laglnGTD_fatality laglnGTD_fatal_event laglnkibris if year>=1988 & year<=2001 & southeast_24==0 & gdp_const_tl_bil_log!=.
su nonstatebranchpercap nonstatedeposit_milUSDpercap statebranchpercap statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl nonstateceditperGDP laglnGTDcivilian laglnGTD_fatality laglnGTD_fatal_event laglnttov_all laglnkibris if year>=2005 & year<=2018 & southeast_24==0


*FULL MODELS FOR APPENDIX
***Non State (Private and Foreign) Owned Branch per 10.000 people
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)5)) atmeans post saving(nstate_b1, replace)
*outreg2 using main_private_full.doc, replace ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
*estimate store NS_Branch_SE_pre2001
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)5)) atmeans post saving(nstate_b2, replace)
*outreg2 using main_private_full.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
*estimate store NS_Branch_SE_post2003
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)5)) atmeans post saving(nstate_b3, replace)
*outreg2 using main_private_full.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
*estimate store NS_Branch_Non_SE_pre2001
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)4)) atmeans post saving(nstate_b4, replace)
*outreg2 using main_private_full.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
*estimate store NS_Branch_Non_SE_post2003

***Non State (Private and Foreign) Deposit in million tl per 10.000
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)5)) atmeans post saving(nstate_d1, replace)
*outreg2 using main_private_full.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
*estimate store NS_Deposit_SE_pre2001
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)4)) atmeans post saving(nstate_d2, replace)
*outreg2 using main_private_full.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
*estimate store NS_Deposit_SE_post2003
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)5)) atmeans post saving(nstate_d3, replace)
*outreg2 using main_private_full.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
*estimate store NS_Deposit_NSE_pre2001
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)4)) atmeans post saving(nstate_d4, replace)
*outreg2 using main_private_full.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
*estimate store NS_Deposit_NSE_post2003



***State Owned Branch per 10.000 people
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)5)) atmeans post saving(state_b1, replace)
*outreg2 using main_state_full.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
*estimate store S_Branch_SE_pre2001
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)5)) atmeans post saving(state_b2, replace)
*outreg2 using main_state_full.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
*estimate store S_Branch_SE_post2003
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)5)) atmeans post saving(state_b3, replace)
*outreg2 using main_state_full.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
*estimate store S_Branch_Non_SE_pre2001
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)4)) atmeans post saving(state_b4, replace)
*outreg2 using main_state_full.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
*estimate store S_Branch_Non_SE_post2003

***State Owned Deposit in million tl per 10.000
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)5)) atmeans post saving(state_d1, replace)
*outreg2 using main_state_full.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
*estimate store S_Deposit_SE_pre2001
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)4)) atmeans post saving(state_d2, replace)
*outreg2 using main_state_full.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
*estimate store S_Deposit_SE_post2003
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)5)) atmeans post saving(state_d3, replace)
*outreg2 using main_state_full.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
*estimate store S_Deposit_NSE_pre2001
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
margins, at(laglnterror=(0(1)4)) atmeans post saving(state_d4, replace)
*outreg2 using main_state_full.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
*estimate store S_Deposit_NSE_post2003


*CREDIT BRANCH RELATION 
xtreg nonstateceditperGDP nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kredi_extend.doc, replace ctitle(Credit) addtext(Year, 1988-2001, Region, Southeast) label
*estimate store NS_Credit_SE_pre2001
xtreg nonstateceditperGDP nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kredi_extend.doc, append ctitle(Credit) addtext(Year, 2005-2018, Region, Southeast) label
*estimate store NS_Credit_SE_post2003
xtreg nonstateceditperGDP nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kredi_extend.doc, append ctitle(Credit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
*estimate store NS_Credit_Non_SE_pre2001
xtreg nonstateceditperGDP nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kredi_extend.doc, append ctitle(Credit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
*estimate store NS_Credit_Non_SE_post2003
xtreg stateceditperGDP statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kredi_extend.doc, append ctitle(Credit) addtext(Year, 1988-2001, Region, Southeast) label
*estimate store S_Credit_SE_pre2001
xtreg stateceditperGDP statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kredi_extend.doc, append ctitle(Credit) addtext(Year, 2005-2018, Region, Southeast) label
*estimate store S_Credit_SE_post2003
xtreg stateceditperGDP statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kredi_extend.doc, append ctitle(Credit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
*estimate store S_Credit_NSE_pre2001
xtreg stateceditperGDP statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kredi_extend.doc, append ctitle(Credit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
*estimate store S_Credit_NSE_post2003


*COEFFICIENT PLOTS FOR MAIN TEXT
label var laglnterror "Terror (t-1)"

**NON-STATE MODELS (REDUCTED) Figure#3
coefplot (NS_Branch_SE_pre2001, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(120) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.04 .04)) xlabel(-.04(.02).04, labsize(small) nogrid) subtitle("Southeast" "(1988-2001)", size(small)) legend(off size(small) position(1)) saving(rnonsbranch1, replace)
coefplot (NS_Branch_SE_post2003, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.04 .04)) xlabel(-.04(.02).04, labsize(small) nogrid) subtitle("Southeast" "(2005-2018)", size(small)) legend(off size(small) position(1)) yscale(off) saving(rnonsbranch2, replace)
coefplot (NS_Branch_Non_SE_pre2001, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.04 .04)) xlabel(-.04(.02).04, labsize(small) nogrid) subtitle("Rest of Turkey" "(1988-2001)", size(small)) legend(off size(small) position(1))  yscale(off) saving(rnonsbranch3, replace)
coefplot (NS_Branch_Non_SE_post2003, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.04 .04)) xlabel(-.04(.02).04, labsize(small) nogrid)  subtitle("Rest of Turkey" "(2005-2018)", size(small)) legend(off size(small) position(1))  yscale(off) saving(rnonsbranch4, replace)
graph combine rnonsbranch1.gph rnonsbranch2.gph rnonsbranch3.gph rnonsbranch4.gph, r(1) iscale(*1) xsize(5.5) ysize(2) title("Non-state branch", size(small)) scheme(s1mono) saving(rnonsbranch, replace)
coefplot (NS_Deposit_SE_pre2001, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(120) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-1.6 .8)) xlabel(-1.6(.8).8, labsize(small) nogrid) subtitle("Southeast" "(1988-2001)", size(small)) legend(off size(small) position(11)) saving(rnonsdepos1, replace)
coefplot (NS_Deposit_SE_post2003, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-1.6 .8)) xlabel(-1.6(.8).8, labsize(small) nogrid) subtitle("Southeast" "(2005-2018)", size(small)) legend (off size(small) position(7)) yscale(off) saving(rnonsdepos2, replace)
coefplot (NS_Deposit_NSE_pre2001, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-1.6 .8)) xlabel(-1.6(.8).8, labsize(small) nogrid) subtitle("Rest of Turkey" "(1988-2001)", size(small)) legend(off size(small) position(7))  yscale(off) saving(rnonsdepos3, replace)
coefplot (NS_Deposit_NSE_post2003, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-1.6 .8)) xlabel(-1.6(.8).8, labsize(small) nogrid)  subtitle("Rest of Turkey" "(2005-2018)", size(small)) legend(off size(small) position(7))  yscale(off) saving(rnonsdepos4, replace)
graph combine rnonsdepos1.gph rnonsdepos2.gph rnonsdepos3.gph rnonsdepos4.gph, r(1) iscale(*1) xsize(5.5) ysize(2)  title("Non-state deposit", size(small)) scheme(s1mono) saving(rnonsdepos, replace)
graph combine rnonsbranch.gph rnonsdepos.gph, c(1) iscale(*1.1) xsize(5.5) ysize(4)  title(, size(small)) scheme(s1mono) saving(rednonstate, replace)



**STATE MODELS (REDUCTED) Figure#4
coefplot (S_Branch_SE_pre2001, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(120) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.02 .02)) xlabel(-.02(.01).02 , labsize(small) nogrid) subtitle("Southeast" "(1988-2001)", size(small)) legend(off size(vsmall) position(11)) saving(rsbranch1, replace)
coefplot (S_Branch_SE_post2003, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.02 .02)) xlabel(-.02(.01).02 , labsize(small) nogrid) subtitle("Southeast" "(2005-2018)", size(small)) legend (off size(vsmall) position(7)) yscale(off) saving(rsbranch2, replace)
coefplot (S_Branch_Non_SE_pre2001, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.02 .02)) xlabel(-.02(.01).02 , labsize(small) nogrid) subtitle("Rest of Turkey" "(1988-2001)", size(small)) legend(off size(vsmall) position(7))  yscale(off) saving(rsbranch3, replace)
coefplot (S_Branch_Non_SE_post2003, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.02 .02)) xlabel(-.02(.01).02 , labsize(small) nogrid)  subtitle("Rest of Turkey" "(2005-2018)", size(small)) legend(off size(vsmall) position(7))  yscale(off) saving(rsbranch4, replace)
graph combine rsbranch1.gph rsbranch2.gph rsbranch3.gph rsbranch4.gph, r(1) iscale(*1) xsize(5.5) ysize(2) title("State branch", size(small)) scheme(s1mono) saving(rsbranch, replace)

coefplot (S_Deposit_SE_pre2001, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(120) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.6 .4)) xlabel(-.6(.2).4 , labsize(small) nogrid) subtitle("Southeast" "(1988-2001)", size(small)) legend(off size(vsmall) position(11)) saving(rsdepos1, replace)
coefplot (S_Deposit_SE_post2003, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.6 .4)) xlabel(-.6(.2).4 , labsize(small) nogrid) subtitle("Southeast" "(2005-2018)", size(small)) legend (off size(vsmall) position(7)) yscale(off) saving(rsdepos2, replace)
coefplot (S_Deposit_NSE_pre2001, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.6 .4)) xlabel(-.6(.2).4, labsize(small) nogrid) subtitle("Rest of Turkey" "(1988-2001)", size(small)) legend(off size(vsmall) position(7))  yscale(off) saving(rsdepos3, replace)
coefplot (S_Deposit_NSE_post2003, label() msymbol(o) msize(medium) mcol(gs4)), order(laglnterror) drop(pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(80) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-.6 .4)) xlabel(-.6(.2).4, labsize(small) nogrid)  subtitle("Rest of Turkey" "(2005-2018)", size(small)) legend(off size(vsmall) position(7))  yscale(off) saving(rsdepos4, replace)
graph combine rsdepos1.gph rsdepos2.gph rsdepos3.gph rsdepos4.gph, r(1) iscale(*1) xsize(5.5) ysize(2)  title("State deposit", size(small)) scheme(s1mono) saving(rsdepos, replace)
graph combine rsbranch.gph rsdepos.gph, c(1) iscale(*1.1) xsize(5.5) ysize(4)  title(, size(small)) scheme(s1mono) saving(redstate, replace)



*CREDIT COEFFICIENT PLOTS Figure#7
**Non-state 
label var nonstatebranchpercap "Branch"
label var statebranchpercap "Branch"

coefplot (NS_Credit_SE_pre2001, label(Southeast (1988-2001)) msymbol(o) msize(medium) mcol(gs4)), order(nonstatebranchpercap) drop(laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(130) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-10 30)) xlabel(-10(10)30 , labsize(small) nogrid) subtitle("Southeast" "(1988-2001)", size(small)) saving(rnscredit1, replace)
coefplot (NS_Credit_SE_post2003, label(Southeast (1988-2001)) msymbol(o) msize(medium) mcol(gs4)), order(nonstatebranchpercap) drop(laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(100) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-10 30)) xlabel(-10(10)30 , labsize(small) nogrid) subtitle("Southeast" "(2005-2018)", size(small)) yscale(off) saving(rnscredit2, replace)
coefplot (NS_Credit_Non_SE_pre2001, label(Southeast (1988-2001)) msymbol(o) msize(medium) mcol(gs4)), order(nonstatebranchpercap) drop(laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(100) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-10 30)) xlabel(-10(10)30 , labsize(small) nogrid) subtitle("Rest of Turkey" "(1988-2001)", size(small)) yscale(off) saving(rnscredit3, replace)
coefplot (NS_Credit_Non_SE_post2003, label(Southeast (1988-2001)) msymbol(o) msize(medium) mcol(gs4)), order(nonstatebranchpercap) drop(laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(100) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-10 30)) xlabel(-10(10)30 , labsize(small) nogrid) subtitle("Rest of Turkey" "(2005-2018)", size(small)) yscale(off) saving(rnscredit4, replace)
graph combine rnscredit1.gph rnscredit2.gph rnscredit3.gph rnscredit4.gph , r(1) iscale(*1) xsize(5.5) ysize(2) title("Non-state credit", size(small)) scheme(s1mono) saving(rnscredit, replace)
**State
coefplot (S_Credit_SE_pre2001, label(Southeast (1988-2001)) msymbol(o) msize(medium) mcol(gs4)), order(statebranchpercap) drop(laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(130) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-10 30)) xlabel(-10(10)30 , labsize(small) nogrid) subtitle("Southeast" "(1988-2001)", size(small)) saving(rscredit1, replace)
coefplot (S_Credit_SE_post2003, label(Southeast (1988-2001)) msymbol(o) msize(medium) mcol(gs4)), order(statebranchpercap) drop(laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(100) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-10 30)) xlabel(-20(20)60 , labsize(small) nogrid) subtitle("Southeast" "(2005-2018)", size(small)) yscale(off) saving(rscredit2, replace)
coefplot (S_Credit_NSE_pre2001, label(Southeast (1988-2001)) msymbol(o) msize(medium) mcol(gs4)), order(statebranchpercap) drop(laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(100) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-10 30)) xlabel(-10(10)30 , labsize(small) nogrid) subtitle("Rest of Turkey" "(1988-2001)", size(small)) yscale(off) saving(rscredit3, replace)
coefplot (S_Credit_NSE_post2003, label(Southeast (1988-2001)) msymbol(o) msize(medium) mcol(gs4)), order(statebranchpercap) drop(laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend _cons) ciopts(lpatt(line line) lcol(gs1 gs10) lwidth(*1.5 *3)) scheme(s1mono) grid(none) fxsize(100) levels(95 90) xline(0, lpattern(shortdash) lwidth(vthin) lcolor(red)) xsca(titlegap(0) line r(-10 30)) xlabel(-10(10)30 , labsize(small) nogrid) subtitle("Rest of Turkey" "(2005-2018)", size(small)) yscale(off) saving(rscredit4, replace)
graph combine rscredit1.gph rscredit2.gph rscredit3.gph rscredit4.gph , r(1) iscale(*1) xsize(5.5) ysize(2) title("State credit", size(small)) scheme(s1mono) saving(rscredit, replace)
graph combine rnscredit.gph rscredit.gph, c(1) iscale(*1.1) xsize(5.5) ysize(4)  title(, size(small)) scheme(s1mono) saving(credit_red, replace)



*PREDICTED VALUES OF BANK BRANCHES BY OWNERSHIP-FIGURES FOR REGRESSION FITTED VALUES IN STATE AND NON-STATE DVs.
combomarginsplot nstate_b1 state_b1, labels("Non-state" "State") recast(line) ciopt(color(gs13) color(%50) lpattern(solid)) recastci(rarea) scheme(s2mono) graphregion(fcolor(white))  xtitle(Terror (logged), size(vsmall)) ytitle("Predicted values of branch", size(vsmall)) xsca(titlegap(3) line) ysca(titlegap(5) line r(.1 .5)) xlabel(0(1)5, labsize(vsmall) nogrid) ylabel(.1(.1).5 , labsize(vsmall) nogrid) title("1988-2001 in Southeast" " ", size(small) position(12)) legend (size(vsmall)) saving(branch1, replace)
combomarginsplot nstate_b2 state_b2, labels("Non-state" "State") recast(line) ciopt(color(gs13) color(%50) lpattern(solid)) recastci(rarea) scheme(s2mono) graphregion(fcolor(white))  xtitle(Terror (logged), size(vsmall)) ytitle("Predicted values of branch", size(vsmall)) xsca(titlegap(3) line) ysca(titlegap(5) line r(.1 .5)) xlabel(0(1)5 , labsize(vsmall) nogrid) ylabel(.1(.1).5 , labsize(vsmall) nogrid)  title("2005-2018 in Southeast" " ", size(small) position(12)) legend (size(vsmall)) saving(branch2, replace)
combomarginsplot nstate_b3 state_b3, labels("Non-state" "State") recast(line) ciopt(color(gs13) color(%50) lpattern(solid)) recastci(rarea) scheme(s2mono) graphregion(fcolor(white))  xtitle(Terror (logged), size(vsmall)) ytitle("Predicted values of branch", size(vsmall)) xsca(titlegap(3) line) ysca(titlegap(5) line r(.2 .7)) xlabel(0(1)5 , labsize(vsmall) nogrid) ylabel(.2(.1).7 , labsize(vsmall) nogrid)  title("1988-2001 in non-Southeast" " ", size(small) position(12)) legend (size(vsmall)) saving(branch3, replace)
combomarginsplot nstate_b4 state_b4, labels("Non-state" "State") recast(line) ciopt(color(gs13) color(%50) lpattern(solid)) recastci(rarea) scheme(s2mono) graphregion(fcolor(white))  xtitle(Terror (logged), size(vsmall)) ytitle("Predicted values of branch", size(vsmall)) xsca(titlegap(3) line) ysca(titlegap(5) line r(.2 .7)) xlabel(0(1)4 , labsize(vsmall) nogrid) ylabel(.2(.1).7 , labsize(vsmall) nogrid)  title("2005-2018 in non-Southeas" " ", size(small) position(12)) legend (size(vsmall)) saving(branch4, replace)
graph combine branch1.gph branch2.gph branch3.gph branch4.gph


*PREDICTED VALUES OF DEPOSIT BY REGION (Southeast vs. Rest of Turkey) - FIGURES FOR REGRESSION FITTED VALUES IN DEPOSIT-STATE OWNERSHIP 
combomarginsplot state_d1 state_d3, labels("Southeast" "Non-Southeast") recast(line) ciopt(color(gs13) color(%50) lpattern(solid)) recastci(rarea) scheme(s2mono) graphregion(fcolor(white))  xtitle(Terror (logged), size(vsmall)) ytitle("Predicted values of deposit", size(vsmall)) xsca(titlegap(3) line ) ysca(titlegap(5) line r(-.4 3.5)) xlabel(0(1)5 , labsize(vsmall) nogrid) ylabel(-0.4 "-.4" 0.4 ".4" 1.4 "1.4" 2.4 "2.4" 3.4 "3.4", labsize(vsmall) nogrid angle(90))  title("State banks in 1988-2001" " ", size(small) position(12)) legend (size(vsmall)) saving(deposit1, replace)
combomarginsplot state_d2 state_d4, labels("Southeast" "Non-Southeast") recast(line) ciopt(color(gs13) color(%50) lpattern(solid)) recastci(rarea) scheme(s2mono) graphregion(fcolor(white))  xtitle(Terror (logged), size(vsmall)) ytitle("Predicted values of deposit", size(vsmall)) xsca(titlegap(3) line ) ysca(titlegap(5) line r(.4 15.5)) xlabel(0(1)4 , labsize(vsmall) nogrid) ylabel(0.4 ".4" 4 "4.4" 8.4 "8.4" 12.4 "12.4" 15.4 "15.4", labsize(vsmall) nogrid angle(90))  title("State banks in 2005-2018" " ", size(small) position(12)) legend (size(vsmall)) saving(deposit2, replace)
combomarginsplot nstate_d1 nstate_d3, labels("Southeast" "Non-Southeast") recast(line) ciopt(color(gs13) color(%50) lpattern(solid)) recastci(rarea) scheme(s2mono) graphregion(fcolor(white))  xtitle(Terror (logged), size(vsmall)) ytitle("Predicted values of deposit", size(vsmall)) xsca(titlegap(3) line ) ysca(titlegap(5) line r(-.4 3.5)) xlabel(0(1)5 , labsize(vsmall) nogrid) ylabel(-0.4 "-.4" 0.4 ".4" 1.4 "1.4" 2.4 "2.4" 3.4 "3.4" , labsize(vsmall) nogrid angle(90))  title("Non-state banks in 1988-2001" " ", size(small) position(12)) legend (size(vsmall)) saving(deposit3, replace)
combomarginsplot nstate_d2 nstate_d4, labels("Southeast" "Non-Southeast") recast(line) ciopt(color(gs13) color(%50) lpattern(solid)) recastci(rarea) scheme(s2mono) graphregion(fcolor(white))  xtitle(Terror (logged), size(vsmall)) ytitle("Predicted values of deposit", size(vsmall)) xsca(titlegap(3) line ) ysca(titlegap(5) line r(.4 15.5)) xlabel(0(1)4 , labsize(vsmall) nogrid) ylabel( 0.4 ".4" 4 "4.4" 8.4 "8.4" 12.4 "12.4" 15.4 "15.4", labsize(vsmall) nogrid angle(90))  title("Non-state banks in 2005-2018" " ", size(small) position(12)) legend (size(vsmall)) saving(deposit4, replace)
graph combine deposit1.gph deposit2.gph deposit3.gph deposit4.gph



*2sls-IV REGRESSION MODELS
xtreg laglnterror defense_percent pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using IVreg.doc, addstat() replace ctitle(First Stage) addtext(Year, 2005-2018, Region, Southeast) label
xtivreg2 nonstatebranchpercap pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend (laglnterror = defense_percent) if year>2003 & southeast_24==1, first cluster(prov_ID) fe
*outreg2 using IVreg.doc, addstat() append ctitle(2SLS) keep() addtext(Model, Branch, Bank ownership, Non-state, Year, 2005-2018, Region, Southeast) label
xtivreg2 statebranchpercap pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend (laglnterror = defense_percent) if year>2003 & southeast_24==1, first cluster(prov_ID) fe
*outreg2 using IVreg.doc, addstat() append ctitle(2SLS) keep() addtext(Model, Branch, Bank ownership, State, Year, 2005-2018, Region, Southeast) label



*GTD (CIVILIAN TARGETED AND FATALITY NUMBERS AND FATAL EVENTS) SUB CATEGORIES
*GTD EVENTS TARGETS CIVILIANS
xtreg nonstatebranchpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, replace ctitle(Branch) addtext(Bank type, Non-state, Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Branch) addtext(Bank type, State, Year, 1988-2001, Region, Southeast) label
xtreg statebranchpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Branch) addtext(Bank type, State, Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnGTDcivilian pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDcivilian.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 2005-2018, Region, Rest of Turkey) label


*GTD NUMBER OF KILLED
xtreg nonstatebranchpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, replace ctitle(Branch) addtext(Bank type, Non-state, Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Branch) addtext(Bank type, State, 1988-2001, Region, Southeast) label
xtreg statebranchpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Branch) addtext(Bank type, State, Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Deposit) addtext(Bank type, State,  Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Deposit) addtext(Bank type, State,  Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Deposit) addtext(Bank type, State,  Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnGTD_fatality pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDkilled.doc, append ctitle(Deposit) addtext(Bank type, State,  Year, 2005-2018, Region, Rest of Turkey) label


*GTD EVENT WITH DEATH
xtreg nonstatebranchpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, replace ctitle(Branch) addtext(Bank type, Non-state, Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Branch) addtext(Bank type, State, 1988-2001, Region, Southeast) label
xtreg statebranchpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Branch) addtext(Bank type, State, Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Deposit) addtext(Bank type, State,  Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Deposit) addtext(Bank type, State,  Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Deposit) addtext(Bank type, State,  Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnGTD_fatal_event pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using GTDwdeath.doc, append ctitle(Deposit) addtext(Bank type, State,  Year, 2005-2018, Region, Rest of Turkey) label


*ALL TURKEY BY YEARS SAMPLE  
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001, fe vce(cluster prov_ID)
*outreg2 using allturkey.doc, replace ctitle(Branch) addtext(Bank type, Non-state, Year, 1988-2001, Region, Turkey) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003, fe vce(cluster prov_ID)
*outreg2 using allturkey.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001, fe vce(cluster prov_ID)
*outreg2 using allturkey.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 1988-2001, Region, Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003, fe vce(cluster prov_ID)
*outreg2 using allturkey.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001, fe vce(cluster prov_ID)
*outreg2 using allturkey.doc, append ctitle(Branch) addtext(Bank type, State, Year, 1988-2001, Region, Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003, fe vce(cluster prov_ID)
*outreg2 using allturkey.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001, fe vce(cluster prov_ID)
*outreg2 using allturkey.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 1988-2001, Region, Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003, fe vce(cluster prov_ID)
*outreg2 using allturkey.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 2005-2018, Region, Turkey) label


*NEIGHBORS
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, replace ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year>2003 &  southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap  laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend ln_lag_weig_terror_neig if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using neighbors.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label


*TTID & KIBRIS DATASETS
*TTID DATASET
xtreg nonstatebranchpercap laglnttov_all pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using ttov_all.doc, replace ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnttov_all pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using ttov_all.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnttov_all pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using ttov_all.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnttov_all pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using ttov_all.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnttov_all pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using ttov_all.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnttov_all pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using ttov_all.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnttov_all pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using ttov_all.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnttov_all pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using ttov_all.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 2005-2018, Region, Rest of Turkey) label

**KIBRIS
xtreg nonstatebranchpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, replace ctitle(Branch) addtext(Bank type, Non-state, Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Branch) addtext(Bank type, Non-state, Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Deposit) addtext(Bank type, Non-state, Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Branch) addtext(Bank type, State, Year, 1988-2001, Region, Southeast) label
xtreg statebranchpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Branch) addtext(Bank type, State, Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Branch) addtext(Bank type, State, Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnkibris pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using kibris_clash.doc, append ctitle(Deposit) addtext(Bank type, State, Year, 2005-2018, Region, Rest of Turkey) label


*ALTERNATIVE SOUTHEAST ANALYSES
**Alt 1: Tezcur 2016 (though Kurdish does not mean conflict) 24 Sivas instead of Kilis
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & SE_ALT3==1, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, replace ctitle(Branch) addtext(Bank Type, Non-state, Year, 1988-2001, Southeast region, 24 with Sivas) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & SE_ALT3==1, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Branch) addtext(Bank Type, Non-state, Year, 2005-2018, Southeast region, 24 with Sivas) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & SE_ALT3==0, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Branch) addtext(Bank Type, Non-state, Year, 1988-2001, Southeast region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & SE_ALT3==0, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Branch) addtext(Bank Type, Non-state, Year, 2005-2018, Southeast region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & SE_ALT3==1, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Deposit) addtext(Bank Type, Non-state, Year, 1988-2001, Southeast region, 24 with Sivas) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & SE_ALT3==1, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Deposit) addtext(Bank Type, Non-state, Year, 2005-2018, Southeast region, 24 with Sivas) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & SE_ALT3==0, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Deposit) addtext(Bank Type, Non-state, Year, 1988-2001, Southeast region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & SE_ALT3==0, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Deposit) addtext(Bank Type, Non-state, Year, 2005-2018, Southeast region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & SE_ALT3==1, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Branch) addtext(Bank Type, State, Year, 1988-2001, Southeast region, 24 with Sivas) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & SE_ALT3==1, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Branch) addtext(Bank Type, State, Year, 2005-2018, Southeast region, 24 with Sivas) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & SE_ALT3==0, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Branch) addtext(Bank Type, State, Year, 1988-2001, Southeast region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & SE_ALT3==0, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Branch) addtext(Bank Type, State, Year, 2005-2018, Southeast region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & SE_ALT3==1, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Deposit) addtext(Bank Type, State, Year, 1988-2001, Southeast region, 24 with Sivas) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & SE_ALT3==1, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Deposit) addtext(Bank Type, State, Year, 2005-2018, Southeast region, 24 with Sivas) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & SE_ALT3==0, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Deposit) addtext(Bank Type, State, Year, 1988-2001, Southeast region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & SE_ALT3==0, fe vce (cluster prov_ID)
*outreg2 using SE_Median.doc, append ctitle(Deposit) addtext(Bank Type, State, Year, 2005-2018, Southeast region, Rest of Turkey) label


*COUNT MODELS
**Negative binominal regression models with province fixed effect only for number of branches
***Non State (Private and Foreign) Owned Bank Branch Count
nbreg nonstatebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year<=2001 & southeast_24==1, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, replace ctitle(non-state bank branches) addtext(Year, 1988-2001, Region, Southeast) label
nbreg nonstatebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year>2003 & southeast_24==1, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(non-state bank branches) addtext(Year, 2005-2018, Region, Southeast) label
nbreg nonstatebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year<=2001 & southeast_24==0, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(non-state bank branches) addtext(Year, 1988-2001, Region, Rest of Turkey) label
nbreg nonstatebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year>2003 & southeast_24==0, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(non-state bank branches) addtext(Year, 2005-2018, Region, Rest of Turkey) label

***State Owned Bank Branch Count
nbreg statebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year<=2001 & southeast_24==1, vce(cluster prov_ID) exposure(pop_hunthousand) difficult 
*outreg2 using negat_binom.doc, append ctitle(state bank branches) addtext(Year, 1988-2001, Region, Southeast) label
nbreg statebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year>2003 & southeast_24==1, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(state bank branches) addtext(Year, 2005-2018, Region, Southeast) label
nbreg statebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year<=2001 & southeast_24==0, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(state bank branches) addtext(Year, 1988-2001, Region, Rest of Turkey) label
nbreg statebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year>2003 & southeast_24==0, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(state bank branches) addtext(Year, 2005-2018, Region, Rest of Turkey) label

**Poisson regression models with province fixed effect only for number of branches
***Non State (Private and Foreign) Owned Bank Branch Count
poisson nonstatebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year<=2001 & southeast_24==1, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, replace ctitle(non-state bank branches) addtext(Year, 1988-2001, Region, Southeast) label
poisson nonstatebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year>2003 & southeast_24==1, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(non-state bank branches) addtext(Year, 2005-2018, Region, Southeast) label
poisson nonstatebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year<=2001 & southeast_24==0, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(non-state bank branches) addtext(Year, 1988-2001, Region, Rest of Turkey) label
poisson nonstatebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year>2003 & southeast_24==0, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(non-state bank branches) addtext(Year, 2005-2018, Region, Rest of Turkey) label

***State Owned Bank Branch Count
poisson statebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year<=2001 & southeast_24==1, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(state bank branches) addtext(Year, 1988-2001, Region, Southeast) label
poisson statebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year>2003 & southeast_24==1, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(state bank branches) addtext(Year, 2005-2018, Region, Southeast) label
poisson statebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year<=2001 & southeast_24==0, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(state bank branches) addtext(Year, 1988-2001, Region, Rest of Turkey) label
poisson statebranch laglnterror gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.prov_ID if year>2003 & southeast_24==0, vce(cluster prov_ID) exposure(pop_hunthousand)
*outreg2 using negat_binom.doc, append ctitle(state bank branches) addtext(Year, 2005-2018, Region, Rest of Turkey) label


*Lagged DV
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend nonstatebranchpercap_lag if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, replace ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend nonstatebranchpercap_lag if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend nonstatebranchpercap_lag if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend nonstatebranchpercap_lag if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend nonstatedeposit_milUSDpercap_lag if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend nonstatedeposit_milUSDpercap_lag if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend nonstatedeposit_milUSDpercap_lag if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend nonstatedeposit_milUSDpercap_lag if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend statebranchpercap_lag if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend statebranchpercap_lag if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend statebranchpercap_lag if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend statebranchpercap_lag if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend statedeposit_milUSDpercap_lag if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend statedeposit_milUSDpercap_lag if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend statedeposit_milUSDpercap_lag if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend statedeposit_milUSDpercap_lag if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using lagged_dv3.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label


*DELTA MODELS
xtreg nonstatebranchpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, replace ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statebranchpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap_dlt3 laglnterror_dlt3 pop_hunthousand_dlt3 gdp_const_tl_bil_log_dlt3 Inflationconsumerpricesannu_dlt3 usdollarbuyingtl_dlt3 trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using delta3.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label


*POPULATION CHANGE
xtreg nonstatebranchpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, replace ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand pop_hunthousand_change gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using pop_change.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label


**Deposit interest rate (%) no inflation
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, replace ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  usdollarbuyingtl trend deposit_interest if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  usdollarbuyingtl trend deposit_interest if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  usdollarbuyingtl trend deposit_interest if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log usdollarbuyingtl trend deposit_interest if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using deposit_interest_no_infl.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label



*Horizontal Accountability
*Change across the years 
twoway (connected v2x_horacc year, sort lpattern(dot) msymbol(o)) if year>=1987, xtitle("Years") ytitle(Horizontal accuntability) ylabel(-1.2(0.2)0.6, labsize(small)) xla(1987(1)2018, angle(30) labsize(small)) legend(pos(11) ring(0) col(1) lab(1 "")) scheme(s1mono) saving(horacc1, replace)

xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, replace ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend v2x_horacc if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using horizontal.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label


*REGIME
**V-dem Regime with ambiguous cases 7 categories [0, 6] with gaps
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, replace ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg nonstatedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Branch) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statebranchpercap laglnterror pop_hunthousand gdp_const_tl_bil_log  Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Branch) addtext(Year, 2005-2018, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year<=2001 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year>2003 & southeast_24==1, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Southeast) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year<=2001 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Deposit) addtext(Year, 1988-2001, Region, Rest of Turkey) label
xtreg statedeposit_milUSDpercap laglnterror pop_hunthousand gdp_const_tl_bil_log Inflationconsumerpricesannu usdollarbuyingtl trend i.v2x_regime_amb if year>2003 & southeast_24==0, fe vce(cluster prov_ID)
*outreg2 using v_dem.doc, append ctitle(Deposit) addtext(Year, 2005-2018, Region, Rest of Turkey) label


