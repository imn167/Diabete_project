libname malib "/home/u63672872/sasuser.v94/Ma Librairie";
options fmtsearch=(malib);

PROC IMPORT out=malib.Diabete 
			datafile = "/home/u63672872/sasuser.v94/Cours/Etude_de_cas/Projet EDC/4. Diabète.xlsx" 
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

/* On supprime les individus dont le temps à jeun est inférieur à 60 mn, ainsi que les variables indiquant
la seconde mesure de pression arterielle. Les individus comportant des données manquantes après suppression des 2 variables sont aussi supprimés.*/

data malib.Diabete_clear;
set malib.DIABETE(drop = bp_2s bp_2d S);
if time_ppn < 60 then delete;
if cmiss(of _all_) then delete;
run;

/* 330 individus 16 variables */

proc summary data = malib.Diabete_clear;
var weight height waist;
output out =summary_quanti;
run;

proc print data = summary_quanti;
run;

/* Création des variables IMC, Ratio Tour de Taille/Hanche,   */

data malib.Diabete_Fin;
set malib.diabete_clear;
IMC = round(weight/(height**2),0.01);
Ratio_WH = round(waist/hip,0.01);
ratio = round(ratio,0.01);
glyhb = round(glyhb,0.01);
run;

/* Création des variables _Code pour l'IMC, et la tension arterielle ainsi que leur format */

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

format IMC_Code Decode_IMC. Bp_Code Decode_BP.;

run;








