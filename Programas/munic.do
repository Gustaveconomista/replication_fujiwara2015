
* Replication file for "Voting Technology, Political Responsiveness, and Infant Health: Evidence from Brazil"
* This file generates all estimates and figure based on municipality-level data in the paper (Tables 1-3 and A1, Figures 2-3 and A2-A3)
* The unit of observation is a municipality

clear all
cd "\\sbsb2\DISOC_RIO\BMT\RAIS\pisos\Projeto_Pisos2022\Replication_Fujiwara2015\Dados"
use munic.dta

*Sets folder where all logs will be
cd "\\sbsb2\DISOC_RIO\BMT\RAIS\pisos\Projeto_Pisos2022\Replication_Fujiwara2015\logs"

* Define Variables
gen dep = voters96 - 40500
gen treat=0
 replace treat=1 if dep>0
gen deptreat = dep*treat
gen bw=dep
replace bw=dep*-1 if dep<0
gen ones=1

* Set binned variables
egen bin_voters96=cut(voters96), at(500(4000)200000) 
 replace bin_voters96=bin_voters+2000
egen bin_util94=mean(r_util94), by(bin_voters96)
egen bin_util98=mean(r_util98), by(bin_voters96)
egen bin_util02=mean(r_util02), by(bin_voters96)
egen bin_attend=mean(attend), by(bin_voters96)
egen bin_regist=mean(regist), by(bin_voters96)
egen bin_obs=sum(ones), by(bin_voters96)

label var bin_util94 "Valid Votes/Turnout - 1994 Election (Paper Only)"
label var bin_util98 "Valid Votes/Turnout - 1998 Election (Discontinuity)"
label var bin_util02 "Valid Votes/Turnout - 2002 Election (Electronic Only)"
label var bin_regist "Registered Voters/Total Population"
label var bin_attend "Turnout/Registered Voters"
label var bin_obs "Number of Municipalities in Bin"

*Figure 2
#delimit ;
twoway
scatter bin_util94 bin_voters96 if voters96<100000 & voters96>4500, mc(green) ms(square) msize(small) ||
scatter bin_util98 bin_voters96 if voters96<100000 & voters96>4500, mc(blue) ||
scatter bin_util02 bin_voters96 if voters96<100000 & voters96>4500, mc(red) ms(triangle) ||
qfit r_util94 voters96 if voters96<40500 & voters96>5000 , lc(green) ||
qfit r_util94 voters96 if voters96<100000 & voters96>40500 , lc(green) ||
qfit r_util02 voters96 if voters96<40500 & voters96>5000 , lc(red) ||
qfit r_util02 voters96 if voters96<100000 & voters96>40500 , lc(red) ||
qfit r_util98 voters96 if voters96<40500 & voters96>5000 , lc(blue) ||
qfit r_util98 voters96 if voters96<100000 & voters96>40500 , lc(blue) xtitle("Number of Registered Voters - 1996") xline(40500) legend(order(1 2 3) rows(3))  ylabel(0.6(.1)1,grid)  ; 
graph2tex, epsfile(figure_discont);
#delimit cr

*Figure 3
#delimit ;
twoway
scatter bin_regist bin_voters96 if voters96<100000 & voters96>4500 , mc(green) ||
scatter bin_attend bin_voters96 if voters96<100000 & voters96>4500 , mc(blue) ms(triangle) ||
qfit regist voters96 if voters96<40500 & voters96>5000 , lc(green) ||
qfit regist voters96 if voters96<100000 & voters96>40500 , lc(green) ||
qfit attend voters96 if voters96<40500 & voters96>5000 , lc(blue) ||
qfit attend voters96 if voters96<100000 & voters96>40500 , lc(blue) xtitle("Number of Registered Voters - 1996") xline(40500) legend(order(1 2) rows(2)) ysc(ax(1) r(0.6 1))  ylabel(0.6(.1)1,grid) ; 
graph2tex, epsfile(figure_turnout);
#delimit cr

*Figure A2
#delimit ;
twoway
scatter bin_obs bin_voters96 if voters96<100000 & voters96>15500 , mc(blue) xtitle("Number of Registered Voters - 1996") xline(40500) legend(order(1 2 3) rows(3)); 
graph2tex, epsfile(figure_manip);
#delimit cr

*Figure A3
#delimit ;
xi: reg treat i.uf;
predict pred, xb;
egen bin_pred=mean(pred), by(bin_voters96);
label var bin_pred "Predicted Treatment from State Dummies";
twoway
scatter bin_pred bin_voters96 if voters96<100000 & voters96>4500, mc(blue)  ||
qfit pred voters96 if voters96<40500 & voters96>5000 , lc(blue) ||
qfit pred voters96 if voters96<100000 & voters96>40500 , lc(blue) xtitle("Number of Registered Voters - 1996") xline(40500) legend(order(1))  ysc(r(0 0.15)) ylabel(0(.03)0.15,grid) ; 
graph2tex, epsfile(predict_uf);
#delimit cr

*Table 1
foreach var in income gini latitude longitude illiter less4 less8 population91 population00 urbanization {
sum `var'
rdbwselect `var' dep, kernel(uni) bwselect(IK)
 reg `var' treat dep deptreat if bw<e(h_IK), rob
 reg `var' treat dep deptreat if bw<20000, rob
 reg `var' treat dep deptreat if bw<10000, rob
 reg `var' treat dep deptreat if bw<5000, rob
} 

*Table 2, Panels A and B
foreach var in  r_util98 attend regist r_util94 r_util02 {
sum `var'
rdbwselect `var' dep, kernel(uni) bwselect(IK)
 reg `var' treat dep deptreat if bw<e(h_IK), rob
reg `var' treat dep deptreat if bw<10000, rob
reg `var' treat dep deptreat if bw<5000, rob
} 

*Table 2, Panel C
local y right
local x right_meas right_state
sum `y'
reg `y' treat `x' dep deptreat  if dep>-20000 & dep<20000  , cl(uf)
reg `y' treat `x' dep deptreat  if dep>-10000 & dep<10000 ,   cl(uf)
reg `y' treat `x' dep deptreat  if dep>-5000 & dep<5000   ,  cl(uf)

*Table 3
local y r_util98
local x 
local med 25.43
sum r_util98 if illiter>`med' 
sum r_util98 if illiter<`med' 
rdbwselect `y' dep, kernel(uni) bwselect(IK)
qui reg `y' treat dep deptreat  if dep<e(h_IK) & dep>-e(h_IK) & illiter>`med' 
  estimates store a
rdbwselect `y' dep, kernel(uni) bwselect(IK)  
qui reg `y' treat dep deptreat  if dep<e(h_IK) & dep>-e(h_IK) & illiter<`med' 
  estimates store b
suest a b, rob
  lincom [a_mean]treat-[b_mean]treat
foreach bw in 20000 10000 5000 {  
local y r_util98
local med 25.43
qui reg `y' treat dep deptreat   if dep<`bw' & dep>-`bw' & illiter>`med' 
  estimates store a
qui reg `y' treat dep deptreat   if dep<`bw' & dep>-`bw' & illiter<`med' 
  estimates store b
suest a b, rob
  lincom [a_mean]treat-[b_mean]treat
}  

*Table A1
foreach var in null_est_share blank_est_share util_fed_share util_sen_share  util_gov_share util_pres_share  {
sum `var'
rdbwselect `var' dep, kernel(uni) bwselect(IK)
 reg `var' treat dep deptreat if bw<e(h_IK), rob
 reg `var' treat dep deptreat if bw<20000, rob
 reg `var' treat dep deptreat if bw<10000, rob
 reg `var' treat dep deptreat if bw<5000, rob
} 
