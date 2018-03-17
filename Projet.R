#Importer la librairie FactoMiner
library(FactoMineR)
#Importer la librairie ggplot2
library(ggplot2)
#Importer la librairie corrplot
library(corrplot)
#Importer la librairie dplyr
library(dplyr)
#On charge le fichier
cereal=read.delim("/Users/jzk/Downloads/BD_Projet_2017_2018_INFO/Binome5/276-
                  cereals.txt",header = T)
#Cette instruction nous permet d'avoir le nom des colonnes contenu dans notre liste
colnames(cereal)
# Cette instruction nous donne le nombre de ligne unique pour ce jeu données, c'est à dire le
nombre de barre céréalières différentes.
nrow(unique(cereal))
#Le nombre de variables dans ce jeu de données
ncol(cereal)
#On souhaite récupérer le type de chaque variable
#On constate que nom est "integer" etc...
sapply(cereal,typeof)
#Récupérer les informations sur les variables et discuter
sapply(cereal,class)
#On remplace les valeurs manquantes par NA afin de ne pas être inclus dans les calculs
cereal[cereal==-1 | cereal==-1.0]=NA
#Aperçu des 10 premières lignes
head(cereal,10)
# ACP
resultatPCA = PCA(cereal,scale.unit = T,graph = T,quali.sup = 1:3)
resultatPCA$eig
resultatPCA$var$coord
resultatPCA$var$contrib
#Matrice de corrélation
numeric_var <- names(cereal)[which(sapply(cereal, is.numeric))]
cor(cereal[,numeric_var], use="complete.obs", method="kendall") %>%
  corrplot(method="square", type="lower", tl.cex= 0.7)#On utilise cette fonction attach afin de diminuer la longueur des variables au cours du code.
attach(cereal)
# Un histogramme des plusieurs compagnies présentes, on constate que G,K sont très
présents et que A très peu.
table(MANUF)
barplot(table(MANUF))
#Faire une régression linéaire entre rating calories, sugar : lm1=lm(rating~cal*sug)
gg1=ggplot(data=cereal,aes(x=SUGARS,y=CALORIES,col=RATING))+
  geom_jitter(data=cereal,aes(,CALORIES,col=RATING))+
  labs(x="Sugar",y="Calories")+
  geom_smooth(method="lm",se=FALSE,col='black')+
  theme_bw()
gg1
lm1=lm(RATING~CALORIES*SUGARS)
summary(lm1)
###Régression linéaire plus précise
lm2=lm(RATING~CALORIES*SUGARS*POTASS*FIBER)
summary(lm2)
#On enlève les noms variable inutile pour la corrélation
utile=cereal
head(utile)
#On enlève la colonne NOM
utile=utile[,-1]
#Conversion des LEVELS MUNUFACTEUR
utile$MANUF=as.numeric(utile$MANUF)
#Conversion des LEVELS TYPE C OU H
utile$TYPE=as.numeric(utile$TYPE)
#Matrice de corrélation entre les variables
MatrixCor=cor(utile,use = "na")
#Faire un rating par compagnie pour expliquer une influence.
#Boxplot de toutes les compagnies
plot(MANUF,RATING)
#Histogramme entre nombre de barre de céréales et calories (fréquences)
# 110 calories pour une barre
barplot(table(CALORIES))
###Test de Student pour vérifier que cela peut se modéliser comme une loi normale
t.test(CALORIES, mu=106)#Décrire pour chaque colonne la valeur la plus basse, la plus haute ainsi que
médiane/moyenne.
summary(CALORIES); boxplot(CALORIES, main="Répartition des calories") #Description
des calories
summary(RATING); boxplot(RATING, main="Répartition des notes") #Description des votes
summary(SUGARS); boxplot(SUGARS, main="Répartition du sucre") #Description du taux de
sucres dans les barres céréalières, enlever valeurs -1
summary(CARBO); boxplot(CARBO, main="Répartition des glucides") #Description des
glucides, enlever valeurs -1
summary(FAT); boxplot(FAT, main="Répartition des lipides") #Description des lipides.
summary(PROTEIN); boxplot(PROTEIN, main="Répartition des proteines") #Description des
protéines.
summary(SODIUM); boxplot(SODIUM, main="Répartition de la quantité de sel")
#Description de sodium
summary(FIBER); boxplot(FIBER, main="Répartition des fibres") #Description de fiber.
summary(POTASS); boxplot(POTASS, main="Répartition de la quantité de potassium")
#Description de potass
summary(WEIGHT); boxplot(WEIGHT, main="Répartition des poids") #Description de potass
boxplot((cereal[,c(-1,-2,-3,-13,-14,-15)])) ####Affichage de la répartition des variables
boxplot(log(cereal[,c(-1,-2,-3,-13,-14,-15)])) ###Affichage du log de toutes les valeurs
numériques importantes pour voir les disparités
#Commenter sur les recommandations de la FDA suivant les valeurs des nutriments
barplot(table(VITAMINS)) #Histogramme
table(VITAMINS) #Valeurs
#CAH sur les barres de céréales, on distingue 6 groupes
#centrage réduction des données
#pour éviter que variables à forte variance pèsent indûment sur les résultats
cah.cereal <- scale(cereal[,-1:-3],center=T,scale=T)
#matrice des distances entre individus
cah.mat <- dist(cah.cereal)
#CAH - critère de Ward
#method = ward.D2 correspond au vrai critère de Ward
#utilisant le carré de la distance
cah.ward <- hclust(cah.mat,method="ward.D2")
#affichage dendrogramme
plot(cah.ward,xlab = "Alias of Names",sub = "CAH")