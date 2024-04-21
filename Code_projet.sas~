/* Prise en main de la base de données Diabète */

libname malib "/home/u63585891/Diabete_project/BD";
options fmtsearch=(malib);

PROC IMPORT out=malib.Diabete(drop= S) 
			datafile = "/home/u63585891/Diabete_project/4. Diabète.xlsx" 
			dbms=XLSX 
			replace;
			sheet="Données brutes" ; 
RUN;

ods pdf file="/home/u63585891/Diabete_project/Rapport.pdf" ;
/* 403 individus et 19 (18 normalement, ya une variable "S" en trop) variables actuellement */

/* On convertit les pounds et inches en kg et m : */

data malib.Diabete;
set malib.DIABETE;
weight = round(weight*0.4536,0.01);
height = round(height*0.0254, 0.01);
waist = round(waist*0.0254, 0.01);
hip = round(hip*0.0254, 0.01);
run;

/* Traitement des données manquantes */
title "Nombre de valeurs manquantes pour chaque variable";
proc means data=malib.diabete NMISS;
title;

run;
/*enormement de données manquantes pour les variables bp_2S et bp_2D 

On compare les valeurs pour voir si il y a un changement dans les 2e prises de pression*/

data malib.pression;
	set malib.Diabete(keep= bp_1s bp_2s bp_1d bp_2d);
	if cmiss(of _all_) then delete;
run;
title "Valeurs maximums et minimums pour les variables de pression sanguine";
proc means data=malib.pression min max maxdec=2 ;
var  bp_1S bp_2S bp_1D bp_2D;
run;
title;

proc sgplot data=malib.pression;
	title "Comparaison de la distribution des pressions systoliques pour les données non manquantes";
	density bp_1s / type= kernel legendlabel= "systolique 1 ";
	density bp_2s / type= kernel legendlabel= "systolique 2";
run; title;

proc sgplot data=malib.pression;
	title "Comparaison de la distribution des pressions systoliques pour les données non manquantes";
	density bp_1d / type= kernel legendlabel= "diastolique 1 ";
	density bp_2d / type= kernel legendlabel= "diastolique 2";
run; title;

/* Les valeurs sont quasi similaires (plus de disparité pour les pressions diastoliques)
donc la suppression des 2e pressions n'impliquera pas une grande perte 
d'informations */

data malib.missing(keep= miss_n);
  set malib.Diabete(drop=bp_2s bp_2d) ;
  miss_n = cmiss(of chol -- time_ppn);
run;

title "Décompte des nombre de valeurs manquantes chez chaque individus";
proc freq data=malib.missing; /* voir proc mi qui donne un meilleur résumé ?*/
run; 
title;
/*Sans les variables bp_2s bp_2d concernées par des valeurs manquantes. On decide donc d'omettre ces 
données */
/* On supprime les individus dont le temps à jeun est inférieur à 60 mn, ainsi que 
les variables indiquant la seconde mesure de pression arterielle. 
Les individus comportant des données manquantes après suppression des 2 variables sont aussi supprimés.*/
 

data malib.Diabete_clear(drop= id);
set malib.DIABETE(drop = bp_2s bp_2d );
/*if time_ppn < 60 then delete;*/
if cmiss(of _all_) then delete;
id_char = put(id, 8.); /*conversion de la variable id en character */
run;
title "Info base de données";
proc contents data=malib.Diabete_clear;
title;

/* Nous avons maintenant un jeu de données de 375 individus 16 variables */
title "Resumé statistiques des variables quantitatives de la table de données 375*16";
proc means data=malib.Diabete_clear(keep= _numeric_) maxdec= 2 min q1 median mean q3 max std;
run;
title;
/*=============================================================================*/
/* ================== DECISION POUR LA VARIABLE TEMPS A JEUN ???? ============== */
/*Ayant que des patients atteints de diabète typer II, on ne s'interessera pas au temps de jeune de chacun */

PROC SGPLOT DATA = malib.Diabete_clear; SCATTER Y = glyhb X = time_ppn;
run;

/*=============================================================================*/

/* Création des variables IMC, Ratio Tour de Taille/Hanche,   */

data malib.Diabete_Fin;
set malib.diabete_clear;
IMC = round(weight/(height**2),0.01);
Ratio_WH = round(waist/hip,0.01);
ratio = round(ratio,0.01);
glyhb = round(glyhb,0.01);
run;

/* Création des variables _Code pour l'IMC, la tension arterielle, et Age ainsi que leur format */

PROC FORMAT LIB = malib;
	Value Decode_IMC
	1 = "Anorexie"
	2 = "Maigreur"
	3 = "Corpulence normale"
	4 = "Surpoids"
	5 = "Obésité modérée"
	6 = "Obésité sévère"
	7 = "Obésité morbide";
run;

PROC FORMAT LIB = malib;
	Value Decode_BP
	1 = "Tension artérielle normale"
	2 = "Tension artérielle élevée"
	3 = "Hypertension artérielle de stade 1"
	4 = "Hypertension artérielle de stade 2";
run;

PROC FORMAT LIB = malib;
	Value Decode_time
	1 = "moins 1 heure"
	2 = "entre 1h 2h "
	3 = "entre 2h et 4h"
	4 = "entre 4h et 8h"
	5 = "entre 8h et 16h"
	6 = "plus de 16 heures";
run;
/*=============================================================================*/

/* ANALYSE DESCRIPTIVES DE VARIABLES QUANTITATIVES */

data malib.Diabete_Fin;
set malib.Diabete_Fin;

if IMC <= 16.5 then IMC_Code = 1;
if 16.5 < IMC <= 18.5 then IMC_Code = 2;
if 18.5 < IMC <= 25 then IMC_Code = 3;
if 25 < IMC <= 30 then IMC_Code = 4;
if 30 < IMC <= 35 then IMC_Code = 5;
if 35 < IMC <= 40 then IMC_Code = 6;
if 40 < IMC  then IMC_Code = 7;

if (bp_1s < 120) and (bp_1d < 80) then Bp_Code = 1;
if (120 <= bp_1s <= 129) and (bp_1d < 80) then Bp_Code = 2;
if (130 <= bp_1s <= 139) or (80 <= bp_1d <= 89) then Bp_Code = 3;
if (bp_1s >= 140) or (bp_1d >= 90) then Bp_Code = 4;

if age <= 30 then age_Code = 1;
if 30 < age <= 40 then age_Code = 2;
if 40 < age <= 50 then age_Code = 3;
if 50 < age <= 60 then age_Code = 4;
if 60 < age <= 70 then age_Code = 5;
if 70 < age then age_Code = 6;

if time_ppn <= 60 then time_Code = 1;
if 60 < time_ppn <= 120 then time_Code = 2;
if 120 < time_ppn <= 240 then time_Code = 3;
if 240 < time_ppn <= 480 then time_Code = 4;
if 480 < time_ppn <= 960 then time_Code = 5;
if 960 < time_ppn then time_Code = 6;

format IMC_Code Decode_IMC. Bp_Code Decode_BP.  age_code Decode_age. time_code Decode_time.;

run;


/* --------- Base de données 375 individus et 21 variables ------------------*/
title "Info base de données";
proc contents data=malib.Diabete_Fin;
run; 
title;
/*Analyse descriptive des variables qualitatives */
/*Ordinales*/
title "Diagramme en baton pour la variable IMC codée";
proc freq data=malib.diabete_fin;
tables IMC_code;
run;

proc sgplot data=malib.diabete_fin;
	vbar imc_code ;
run; /*plus de personne en situation de surpoid voire obesité */
title;
/*---------*/
title "Diagramme en baton pour la variable Age codée";
proc freq data=malib.diabete_fin;
tables age_code;
run;

proc sgplot data=malib.diabete_fin;
	vbar age_code ;
run; /* une repartition équilibrée : quasi même effectif dans chaque tranche d'age */
title;
/*---------*/
title "Diagramme en baton pour la variable Hypertension codée";
proc freq data=malib.diabete_fin;
tables bp_code;
run;

proc sgplot data=malib.diabete_fin;
	vbar bp_code ; /*Enormement d'hypertension arterielle de stade 2 */
run;
title;
/* nominale */
proc freq data=malib.Diabete_Fin;
tables gender;
run;

proc sgplot data=malib.diabete_fin;
	title "Diagramme en baton pour la variable ";
	vbar gender ; /*Enormement d'hypertension arterielle de stade 2 */
run; /* plus de femmes que d'hommes */

proc freq data=malib.Diabete_Fin;
tables location;
run;

proc sgplot data=malib.diabete_fin;
	title "Diagramme en baton pour la variable ";
	vbar location ; /*Enormement d'hypertension arterielle de stade 2 */
run; /* même proportion*/

/*=============================================================================*/

/* ANALYSE DESCRIPTIVES DE VARIABLES QUANTITATIVES */
proc contents data=malib.Diabete_Fin;
/*tentative d'un boxplot pour toutes les variables (même methode que dans R stage Anne) */
proc sort data=malib.Diabete_Fin(keep= stab_glu hdl bp_1s bp_1d id_char) out=malib.diabete_sorted;
	by id_char;
run; 

proc transpose data=malib.Diabete_sorted out=malib.diabete_t;
by id_char;
run;

data malib.diabete_t;
set malib.diabete_t;
label _name_ = "Variable";
label col1 = "Value";
run;

title "Boite à moustache";
proc sgplot data=malib.diabete_t;
yaxis label= "Valeur ";
vbox col1 / group=_name_ ;
run;title;


title "Proportion des individus en fonction du genre et de la classe IMC";
proc sgpanel data=malib.diabete_fin;
panelby gender;
vbar imc_code ;
run;
title; 
/*présence d'obésité / surpoids plus important chez les femmes que chez les hommes */


title "Proportion des individus en fonction du genre et de la classe de pression";
proc sgpanel data=malib.diabete_fin;
panelby gender;
vbar bp_code ;
run;
title; 
/*présence d'obésité / surpoids plus important chez les femmes que chez les hommes */

/*representation de la distribution */
title "Resumé statistiques des variables quantitatives de la table de données 375*16";
proc means data=malib.Diabete_clear(keep= _numeric_) maxdec= 2 min q1 median mean q3 max std;
run;
title;

PROC UNIVARIATE DATA = malib.Diabete_Fin NOPRINT; VAR stab_glu;
HISTOGRAM stab_glu;
RUN;
PROC UNIVARIATE DATA = malib.Diabete_Fin NOPRINT; VAR hdl;
HISTOGRAM hdl;
RUN;
PROC UNIVARIATE DATA = malib.Diabete_Fin NOPRINT; VAR bp_1s;
HISTOGRAM bp_1s;
RUN;
PROC UNIVARIATE DATA = malib.Diabete_Fin NOPRINT; VAR bp_1d;
HISTOGRAM bp_1d;
RUN;

PROC UNIVARIATE DATA = malib.Diabete_Fin NOPRINT; VAR ratio;
HISTOGRAM ratio;
RUN;

PROC UNIVARIATE DATA = malib.Diabete_Fin NOPRINT; VAR ratio_wh;
HISTOGRAM ratio_wh ;
RUN;

PROC UNIVARIATE DATA = malib.Diabete_Fin NOPRINT; VAR glyhb;
HISTOGRAM glyhb;
RUN;


proc sgpanel data=malib.diabete_t;
panelby _name_ ;
histogram col1 ;
run;

/* 
- Variable glucose : moins de 25% avec glucose important sinon moins de 75% avec glucose inf à 100mg/dl

-Variable HDL : 25% avec un faible taux hdl < 40  et +25% dans la norme (40-60)

Peut-être faire une variable catégorielle pour temps à jeun et analyser dessus 

- Variable glyhb : pas de dépendance au jeune car bilan sur 3 mois du glucose, suprenant car seulement
un faible pourcentage (moins de 25%) ont un bilan d'hémoglobine glyquée superieur à 6%

Peut-être que la présence de diabète est plus dû au problème cardiovasculaire car il y a un effectif
importante de personne en surpoid / obésité 
*/

/*=============================================================================*/

/* ANALYSE DESCRIPTIVES DE VARIABLES QUANTITATIVES ET QUALITATIVES*/
title "distribution du taux de glucose selon le temps de jeune";
proc sgpanel data=malib.diabete_fin;
panelby time_code;
histogram stab_glu ;
run;
title;

title "Distribution du taux HDL selon le temps de jeune";
proc sgpanel data=malib.diabete_fin;
panelby time_code;
histogram hdl ;
run;
title;

/* Prevésible mais plus le temps à jeun est grand et plus les taux de glucose sont moins important
neanmoins il y a présence d'individus avec des taux importants et des périodes de jeune préconisées (8h>)*/


title "Distribution du ratio taille - hanche en fonction du genre";
proc sgpanel data=malib.diabete_fin;
panelby gender;
histogram ratio_wh ;
run;
title;

title "Proportion des individus en fonction du genre et de la classe IMC";
proc sgpanel data=malib.diabete_fin;
panelby gender;
vbar imc_code ;
run;
title; 
/*présence d'obésité / surpoids plus important chez les femmes que chez les hommes */

title "Distribution de l'hémoglobine glyquée en fonction des tranches d'age";
proc sgpanel data=malib.diabete_fin;
panelby age_code / columns= 1;
histogram glyhb ;
run;
title;
/* Le passage dans les tranches d'âge de plus de 40 ans montre la présence de glyhb > 7% 
Le diabète type 2 a l'air d'être lié à l'age 
*/

PROC SGPLOT DATA = malib.Diabete_Fin; SCATTER Y = glyhb X = age;
run;

Proc sgpanel data=malib.Diabete_Fin nocycleattrs; 
panelby age_code / layout=columnlattice onepanel colheaderpos=bottom novarname; 
vbox glyhb;  run;

/* Test de comparaison de moyenne 
ANOVA ? 

proc sort data=malib.diabete_fin out=malib.age_sorted;
by age_code;
run;
proc corr data=malib.age_sorted;
var glyhb;
by age_code;
run;
IMPOSSIBLE DE FAIRE LE RAPPORT DE CORRELATION AVEC PROC CORR !!!!*/

title "verification des hypothèse d'équilibre";
proc means data=malib.diabete_fin std mean ;
class age_code;
var glyhb;
run; /*Pas de plan equilibré */
title;

title "Distribution de l'hémoglobine glyquée en fonction des tranches d'age et test de normalité";
proc sgpanel data=malib.diabete_fin;
panelby age_code / columns= 1;
histogram glyhb ;
density glyhb;
run;
title; /* Pas de normalité des sous échantillon donc test de Test de Kruskal et Wallis*/

/*La statistique h va suivre un khi2(5) car on a 6 classes */
proc npar1way data=malib.diabete_fin;
	class age_code;
	var glyhb;
run;
/* la pvalue < .001 < .05 (seuil), on ne peut accepter l'hypothèse nulle (moyennes des classes ==)
Donc l'âge afflut sur l'apparition d'un diabète de type 
 */


/* Pour influence une analyse des variances sur l'obesité en fonction de l'age */
Proc sgpanel data=malib.Diabete_Fin nocycleattrs;
panelby age_code / layout=columnlattice onepanel colheaderpos=bottom novarname; 
vbox imc / group= bp_code; run;

title "verification des hypothèse d'équilibre";
proc means data=malib.diabete_fin std mean ;
class age_code;
var imc;
run; /*Pas de plan equilibré */
title;

title "Distribution de l'IMC en fonction des tranches d'age et test de normalité";
proc sgpanel data=malib.diabete_fin;
panelby age_code / columns= 1;
histogram IMC ;
density IMC;
run;
title;

/* Si il n y a pas egalité des variances on utilise Welch statistique ! */

proc anova data=malib.diabete_fin;
class age_code;
model imc = age_code; MEANS age_code / HOVTEST=LEVENE(TYPE=ABS) tukey  CLDIFF;
run; /* Pas de différence significatives entre les groupes */


/* Analyse de variance pour l'imc en fonction de la tension arterielle */
Proc sgpanel data=malib.Diabete_Fin nocycleattrs;
panelby bp_code / layout=columnlattice onepanel colheaderpos=bottom novarname; 
vbox imc ; run;

title "verification des hypothèse d'équilibre";
proc means data=malib.diabete_fin std mean ;
class age_code;
var imc;
run; /*Pas de plan equilibré */
title;

title "Distribution de l'IMC en fonction des catégories de tension arterielle et test de normalité";
proc sgpanel data=malib.diabete_fin;
panelby age_code / columns= 1;
histogram IMC ;
density IMC;
run;
title;

proc anova data=malib.diabete_fin;
class bp_code;
model imc = bp_code; MEANS bp_code / HOVTEST=LEVENE(TYPE=ABS) TUKEY  CLDIFF;
run; /* Pas de différence significatives entre les groupes
		Une seule différence notable (voir le dernier tableau) c'est celle avec la tension de stade2
 */



/*=============================================================================*/

/* ANALYSE DESCRIPTIVES DEUX VARIABLES QUANTITATIVES*/

/* On regarde les corrélations de l'hémoglobine avec certaines variables */

proc corr data=malib.diabete_fin;
var glyhb hdl bp_1s bp_1d ratio_wh IMC;
run;
/* D'un point de vue maladie diabète : corr significative avec le HDL, pression_S, ratio_WH ++, IMC
	en revanche la pression_D n'est pas corr 
- HDL a pas de corr signif avec les pressions */


/* Graphe qui montre la présence d'aucune relation (linéaire ou pas) entre HDL et tension */
proc sgplot data=malib.diabete_fin; 
scatter X = bp_1s Y = hdl ;
run;

proc sgplot data=malib.diabete_fin; 
scatter X = bp_1d Y = hdl ;
run;

/*------------*/
proc sgplot data=malib.diabete_fin; 
scatter X = bp_1s Y = glyhb ;
run;

proc sgplot data=malib.diabete_fin; 
scatter X = hdl Y = glyhb ;
run;


proc sgplot data=malib.diabete_fin; 
scatter X = ratio_wh Y = glyhb / group=gender ;
run; /* Interpretation : dès que le ratio est superieur à .8
		il y a une présence d'individus avec un fort pourcentage d'hemoglobine
		Dommage on peut pas tracer de vline x = .8*/



/*=============================================================================*/

/* ANALYSE DESCRIPTIVES DEUX VARIABLES QUALITATIVES*/
/* 
JE TE LAISSE FAIRE L'ETUDE POUR 2 VARIABLES QUALITATIVES
*/


/* Différents tableaux de contingences pour les variables qualitatives + khi 2 d'indépendance */

proc freq data=malib.diabete_fin;
tables gender*IMC_code / chisq plots = freqplot;
tables gender*Bp_code / chisq plots = freqplot;
tables IMC_code*Bp_code/ chisq plots = freqplot;
run;

/* data malib.diabete_fin_bp_code */

proc logistic data=malib.diabete_fin desc ;
format Bp_code;

   class Bp_code ;
   class gender (ref = 'male') / PARAM = GLM;
   model Bp_code = chol hdl stab_glu glyhb age gender IMC ratio_WH / selection=backward ;
   
run;

proc reg data=malib.diabete_fin;
model glyhb = chol hdl stab_glu age ratio_WH height weight bp_1s bp_1d / selection=backward;

run;

ods pdf close;







































