/* Prise en main de la base de données Diabète */

libname malib "/home/u63585891/Diabete_project/BD";
options fmtsearch=(malib);

PROC IMPORT out=malib.Diabete(drop= S) 
			datafile = "/home/u63585891/Diabete_project/4. Diabète.xlsx" 
			dbms=XLSX 
			replace;
			sheet="Données brutes" ; 
RUN;

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

proc means data=malib.diabete NMISS;
run;
/*enormement de données manquantes pour les variables bp_2S et bp_2D 

On compare les valeurs pour voir si il y a un changement dans les 2e prises de pression*/

proc means data=malib.Diabete min max ;
var  bp_1S bp_2S bp_1D bp_2D;
run;

data malib.pression;
	set malib.Diabete(keep= bp_1s bp_2s bp_1d bp_2d);
	if cmiss(of _all_) then delete;
run;


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

/* Les valeurs sont quasi similaires donc la suppression des 2e pressions n'impliquera pas une perte 
d'informations */

data malib.missing(keep= miss_n);
  set malib.Diabete(drop=bp_2s bp_2d) ;
  miss_n = cmiss(of chol -- time_ppn);
run;

proc freq data=malib.missing; /* voir proc mi qui donne un meilleur résumé ?*/
run; 

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
proc contents data=malib.Diabete_clear;

/* Nous avons maintenant un jeu de données de 375 individus 16 variables */

proc means data=malib.Diabete_clear maxdec= 2 min q1 median mean q3 max std;
run;

/*=============================================================================*/
/* ================== DECISION POUR LA VARIABLE TEMPS A JEUN ============== */
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
	Value Decode_Age
	1 = "moins de 30ans"
	2 = "Entre 30ans et 40ans "
	3 = "Entre 40ans et 50ans"
	4 = "Entre 50ans et 60ans"
	5 = "Entre 60ans et 70ans"
	6 = "Plus de 70ans";
run;

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
if 70 < IMC then IMC_Code = 6;

format IMC_Code Decode_IMC. Bp_Code Decode_BP.  age_code Decode_age.;

run;


/* --------- Base de données 375 individus et 21 variables ------------------*/
proc contents data=malib.Diabete_Fin;
run; 

/*Analyse descriptive des variables qualitatives */
/*Ordinales*/

proc freq data=malib.diabete_fin;
tables IMC_code;
run;

proc sgplot data=malib.diabete_fin;
	title "Diagramme en baton pour la variable IMC codée";
	vbar imc_code ;
run; /*plus de personne en situation de surpoid voire obesité */

/*---------*/
proc freq data=malib.diabete_fin;
tables age_code;
run;

proc sgplot data=malib.diabete_fin;
	title "Diagramme en baton pour la variable Age codée";
	vbar age_code ;
run; /* une repartition équilibrée : quasi même effectif dans chaque tranche d'age */

/*---------*/
proc freq data=malib.diabete_fin;
tables bp_code;
run;

proc sgplot data=malib.diabete_fin;
	title "Diagramme en baton pour la variable Hypertension codée";
	vbar bp_code ; /*Enormement d'hypertension arterielle de stade 2 */
run;

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
proc sort data=malib.Diabete_Fin(keep= stab_glu hdl bp_1s bp_1d imc  id_char) out=malib.diabete_sorted;
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
vbox col1 / group=_name_ ;
run;title;

/*representation de la distribution */
PROC UNIVARIATE DATA = malib.Diabete_Fin NOPRINT; VAR stab_glu;
HISTOGRAM stab_glu;
RUN;










































