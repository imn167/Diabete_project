############# Datasets Diabète ############
library(readxl)
##### Importation données #####
diabete <- read_excel("4. Diabète.xlsx", sheet = "Données brutes")

diab <- read_excel("Projet Etude de cas .xlsx", sheet = "Données brutes")

write.csv(diab, "donnes_brutes.csv", row.names = FALSE)

#### traitement données manquantes ####
colSums(is.na(diabete))

boxplot(diabete$age ~diabete$gender)

#13 données manquantes pour la variable glyhb
#2e mesures de pression avec enormement de données manquantes:
#comment mettre 
index = which(is.na(diabete$bp.2s) == F)
plot(c(0, 403), c(0,240), type = "n")
segments(index,  rep(0, length(index)),y1= diabete$bp.1s[index], lwd = .7)
segments(index+.6,  rep(0, length(index)),y1= diabete$bp.2s[index], col = 'blue', lwd = .7 )
# similaritées dans les valeurs non manquantes donc pas de différences flagrantes 

df <- diabete[, -c(14, 15)]
names(df)

#=========================================================@
### Traitement conversion dans les bonnes unités 
#inches --> cm : 1inch = 0.0254m
df[, c(15,14, 10)] <-  apply(df[, c(15,14, 10)], 2, function(x) x*0.0254)
#pounds --> kg : 1 pounds = 0.4536 kg 
df$weight <- df$weight * 0.4536
#=========================================================@

summary(df)
#=========================================================@
##### Etude des données manquantes #####
NA_rows = df[!complete.cases(df),]
NA_rows
nrow(NA_rows) #28 individus concernées --> 375 indiv si na.omit

countNA(df) #total de 39 données manquantes 

library(VIM)
summary(aggr(df, col=c('yellow', 'green')))
#combinaison : 
# 13 indiv sans glyhb (pas de combinaison)
# 1 indiv avec données cholesterol manquantes (3var de cholesterol ratio LDH cholesterol)
# 1 indiv avec 3 données manquantes : sans pression (s et d) et sans mesure de taille (hauteur)
# 1 indiv sans le poids 
# 1 indiv sans temps, ni bp.1S et bp.2s
# 1 indiv sans le temps 


df <- na.omit(df)
df$id = as.character(df$id)
#===========================================@
library(tidyverse)

#### Tests de coherence ####
#resumé general pour les variable quant 
apply(df[,sapply(df, class) == "numeric"], 2, summary)
boxplot(df[,sapply(df, class) == "numeric"], col = "lightblue") #mauvaise idée, on voit rien 
par(mfrow = c(2,2))
for (i in 1:length(df[,sapply(df, class) == "numeric"])) {
  boxplot(df[,sapply(df, class) == "numeric"][i], col = "lightblue", 
          main = names(df[,sapply(df, class) == "numeric"])[i])
}


### Analyse descriptives univarié ####
### cardiovasculaire ####
#inutile d'avoir la taille et le poids sep --> IMC 
#tour de taille et tour de hanche --> rapport hanche/taille 
#utilisation du ration chol/hdl 
df$IMC <- df$weight / (df$height**2)
boxplot(df$IMC, col = "lightblue")
summary(df$IMC) #seulement 25% des personnes dans la norme sinon classement en surpoids !!
####@
df$hiphop <- df$waist / df$hip
boxplot(df$hiphop, col = "lightblue") #indicateur de risque cardio vasculaire ? 
summary(df$hiphop) #  
df %>% ggplot(aes(IMC)) + geom_density()  +theme_minimal()
df %>% ggplot(aes(hiphop)) + geom_density()  +theme_minimal()

####@
boxplot(df$hdl, col = "lightblue") #25% avec un taux trop faible de HDL (<38mg/dL)
summary(df$hdl)
####@ UNE MEILLEUR ANALYSE AVEC UN TABLEAU CROISE SUR LES CATAGORIE DE PRESSION 
summary(df$bp.1s)
summary(df$bp.1d)

######## glycémie #######
#stab.glu --> niveau de glucose 
#la moitié été à jeun à plus de 4 et 25% à plus de 8h (ce qui est la norme)
#On considère donc les normes non a jeun ? 
sum(df$time.ppn >= 480) #99 individus à jeun à plus de 8h 

####@
summary(df$stab.glu[which(df$time.ppn >= 480)]) 
sum(df$stab.glu[which(df$time.ppn >= 480)] >= 126)
#17 personne avec un seuil de glycémie critique à jeun --> diabète 

summary(df$stab.glu[which(df$time.ppn < 480)]) 
sum(df$stab.glu[which(df$time.ppn < 480)] >= 200) 
#23 personne avec un seuil de glycémie critique à jeun --> diabète 

plot(density(df$stab.glu[which(df$time.ppn >= 480)]), main = "Glycemie à jeun")
plot(density(df$stab.glu[which(df$time.ppn < 480)]), main = "Glycemie non à jeun")

#Queue de distribution importantes !
summary(df$glyhb[which(df$time.ppn >= 480)]) 
summary(df$glyhb[which(df$time.ppn < 480)]) 
plot(density(df$glyhb[which(df$time.ppn >= 480)]), main = "Hémoglobine à jeun")
plot(density(df$glyhb[which(df$time.ppn < 480)]), main = "Hémoglobine non à jeun")

#hemoglobine glyqué > 7% 
sum(df$glyhb > 7) #seulement 58 !? 



library(ggplot2)


### Analyse bivariée ####

#### Distribution selon le genre #####
#changement en fonction du genre 
df %>% ggplot(aes(glyhb, color = gender, group = gender)) + geom_density() +
  theme_minimal()
df %>% ggplot(aes(y = glyhb, group =gender, color = gender)) + geom_boxplot()+ theme_minimal()

df %>% ggplot(aes(y = stab.glu, group =gender, color = gender)) + geom_boxplot()+ theme_minimal()

#difference pour les proprièté poids / cardiovasculaire 
df %>% ggplot(aes(x = hdl, group =gender, color = gender)) + geom_density()+ theme_minimal() +
  labs(title = "hdl")
#Homme avec de plus faible taux de HDL --> pas bon 
df %>% ggplot(aes(x = hiphop, group =gender, color = gender)) + geom_density()+ theme_minimal()
# + Femme avec de + faible ratio 
df %>% ggplot(aes(x = IMC, group =gender, color = gender)) + geom_density()+ theme_minimal()
#même conclusion que hiphop pour l'IMC 

#Repartion entre hdl et IMC/hiphop 
df %>% ggplot(aes(hdl, IMC, color = gender)) + geom_point() + theme_minimal()
#+ le hdl est faible + l'IMC est important 
df %>% ggplot(aes(hdl, hiphop, color = gender)) + geom_point() + theme_minimal()
#certaine femme ont de bon hdl cependant un ratio hanche /taille important ptt une question de morphologie ?



df$Code_bps <- as.character(sapply(df$bp.1s, 
                      function(x) 1 * (x<120) + 2 * (129>x & x>=120) + 3 * (139>x & x>=129) + 4 * (x>= 139)))
df$Code_bpd <- as.character(sapply(df$bp.1d, 
                      function(x) 1 * (x<80) + 2 * (89>x & x>=80) + 3 * (x>= 89)))
table(df$Code_bps) #+ de personnes avec de fortes tensions systoliques
table(df$Code_bpd)
round(table(df$Code_bpd, df$Code_bps)/ rowSums(table(df$Code_bpd, df$Code_bps)), 2)


#repartition d'age 
df$age_code <- as.character(sapply(df$age, function(x) 1 * (x<30) + 2 * (40>x & x>=30) + 
                                     3 * (50>x & x>=40) + 4 * (60>x & x>= 50) +
         5*(70>x & x>= 60) + 6*(x>= 70)))

barplot(table(df$age_code))

df %>% ggplot(aes(y=hiphop, age_code, color = gender)) + geom_boxplot() + theme_minimal()
df %>% ggplot(aes(y=hiphop, age_code, color = Code_bpd)) + geom_boxplot() + theme_minimal()
df %>% ggplot(aes(y=hiphop, age_code, color = Code_bps)) + geom_boxplot() + theme_minimal()

df %>% ggplot(aes(y=IMC, age_code, color = gender)) + geom_boxplot() + theme_minimal()
#chez les -30ans + d'obesité chez les femmes 

df %>% ggplot(aes(y=ratio, age_code, color = gender)) + geom_boxplot() + theme_minimal()
#

df %>% ggplot(aes(y=hdl, chol, color = age_code)) + geom_point() + theme_minimal()
#lorsque le chol augmente il y a séparation du nuage en 2 parties 
#(trop de chol pour un faible HDL / fort taux dans les 2)
df %>% ggplot(aes(y=hdl, chol, color = Code_bps)) + geom_point() + theme_minimal()
df %>% ggplot(aes(y=hdl, chol, color = Code_bpd)) + geom_point() + theme_minimal()

df %>% ggplot(aes(y=glyhb, age_code, color = gender)) + geom_boxplot() + theme_minimal()
#on retrouve le risque de diabète de type 2 avec l'age, sauf qu'il y a certains cas extreme avec l'age 

df %>% ggplot(aes(y=stab.glu, age_code, color = gender)) + geom_boxplot() + theme_minimal()

df %>% ggplot(aes(y=glyhb, ratio, color = gender)) + geom_point() + 
  geom_hline(yintercept = 7)+ facet_wrap(~age_code) + theme_minimal() 
#donc a priori l'obesite n'est pas tjrs la cause de diabète mais favorise 

#hypertension et obesité 
df %>% ggplot(aes(hdl, bp.1s, color = gender)) + geom_point() + theme_minimal()
df %>% ggplot(aes(y=hdl, bp.1d, color = gender)) + geom_point() + theme_minimal()


#### inference ####
#regression linéaire avec stepwise 
reg = lm(glyhb~., data = df[,sapply(df, class) == "numeric"], )
stepwise = step(reg, direction = "backward", k = log(nrow(df)))
summary(stepwise)

#regression ridge --> robuste à la multicolinearité or pas de selection de variable
library(glmnet)
X <- model.matrix(glyhb~., data =  df[,sapply(df, class) == "numeric"])[,-1] #sans intercept 
cv_ridge = cv.glmnet(X, df$glyhb, alpha =1)
coef(cv_ridge,s=cv_ridge$lambda.min)[-1]

#regression PLS 
library(pls)










