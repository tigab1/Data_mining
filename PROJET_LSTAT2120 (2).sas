

FILENAME myfile '/home/u48508240/PROJET_LSTAT2120_2021/carseats.csv';

/*importer les données*/
PROC IMPORT DATAFILE=myfile
	DBMS=CSV
	OUT=WORK.CAR
	replace;
	delimiter=';' ;
	GETNAMES=YES;
RUN;

proc print data= WORK.CAR;
run;


/*histogramme des variables*/

ods graphics on;
proc univariate data=CAR;
   var Education CompPrice Income Population Price;
   histogram;
run;

/* statistiques descriptives*/
proc means data=CAR N mean std var skewness kurtosis maxdec=2 ; 
run;


/* on remarque une corrélation assez importante entre Price et CompPrice (0.58),
et une corrélation (non-négligeable) négative entre Sales/Price(-0.44) et positive entre
 Sales/Advertising(0.269) */ 

proc corr data=CAR; 
run;
ods graphics off;

data CAR1;
set CAR;
if ShelveLoc='Bad' then Shel_Bad=1; else Shel_Bad=0; 
if ShelveLoc='Good' then Shel_Good=1; else Shel_Good=0;
if ShelveLoc='Medium' then Shel_Med=1; else Shel_Med=0;  

if Urban='Yes' then Urb_Yes=1; else Urb_Yes=0;
if Urban='No' then Urb_No =1; else Urb_No =0; 

if US='Yes' then US_Yes =1; else US_Yes =0;
if US='No' then US_No =1; else US_No =0;

run;


/* CAR2= sous-ensemble de 300 observations pour le modèle*/
/* CAR_PRED = sous-ensemble de 100 observations reservées pour la prédiction*/


data temp2;
set CAR1;
n=ranuni(8);
run;
proc sort data=temp2;

  by n;
run;

data CAR2 CAR_PRED;

set temp2 nobs=nobs;
if _n_<=.75*nobs then output CAR2;
else output CAR_PRED;
run;


/* création des sous-ensembles test(30%) et train(70%)*/
data temp;
set CAR2;
n=ranuni(8);
run;

proc sort data=temp;

  by n;
run;

data train test;

set temp nobs=nobs;
if _n_<=.7*nobs then output train;
else output test;
run;
 

ods graphics on;

/* stepwise*/
proc reg data=train outest=sss tableout plots=none;
model Sales= CompPrice Income Advertising Population Price Age Education Shel_Bad  Shel_Good Urb_Yes US_Yes / selection=stepwise vif collinoint spec ;
run; quit;


proc print data=sss; run; quit; 

/* ridge regression*/

proc reg data=train ridge= 0 to 1 by 0.1 outest=ridgest;
model Sales= CompPrice Income Advertising Population Price Age Education Shel_Bad  Shel_Good Urb_Yes US_Yes / vif spec dw;
run;


/*Lasso*/
proc glmselect data=CAR2 plots(stepaxis=normb)=all seed=123 outdesign=lassgest;
class ShelveLoc Urban US;
model Sales= CompPrice Income Advertising Population Price Age Education ShelveLoc Urban US / selection=lasso(stop=none choose=cvex) cvmethod=random(5)  ;

run;

ods graphics off;

/* Test linear combination Comprice & Price, and some variables*/

proc reg data=train plots=NONE;
model Sales= CompPrice Income Advertising Population Price Age Education Shel_Bad  Shel_Good Urb_Yes US_Yes;
test CompPrice + Price =0;
test CompPrice + Price+Shel_Good =1;
test Income = 0, Price = 0, Shel_Good = 0;

run;


/*prediction interval*/

proc reg data=test plots=none;
  model Sales= CompPrice Income Advertising Price Age Shel_Bad  Shel_Good/ vif spec stb clb  ;
  output out=stdres p=predicted r=resid ucl=upper_pred lcl=lower_pred;
run;

/*test de normalité des résidus (Shapiro-Wilk)*/
proc univariate data=stdres normal;
var resid;
run;

proc print data=test;
var Sales predicted  lower_pred upper_pred;
run;


/*test d'homoscdasticité*/
proc reg data=train;
model Sales= CompPrice Income Advertising Price Age Shel_Bad  Shel_Good / spec;
run;

ods graphics on;
proc model data=train;
parms b0 b1 b2 b3 b4 b5 b6 b7;
Sales= b0 + b1*CompPrice + b2*Income + b3*Advertising + b4*Price + b5*Age + b6*ShelBad + b7*ShelGood;
fit sales/white;
run;

ods graphics off;


/*outliers*/

/*DFFITS*/

/* |DFFITS| = 2*sqrt(8/210) = 0.41*/



proc reg data=train plots(only)=dffits; 
model Sales= CompPrice Income Advertising Price Age Shel_Bad  Shel_Good /spec; 
/*plot h.*obs.; */ 
output out=outliers dffits=df; 
run;

proc print data=outliers; 
var lev; 
where lev >0.076; 
run;

proc print data=outliers;
  var df;
  where df > 0.34 or df < -0.34; 
run;

  
