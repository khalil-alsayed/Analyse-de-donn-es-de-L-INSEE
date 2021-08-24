#-----------------------------------------------------exo1-------------------------------------------------------------------------------
library(FactoMineR)
library(raster)
library(Factoshiny)
library(ade4)
library(factoextra)
#il faut télécharger le file ODD_DEP2.csv que je vous enverrai par mail avec mon projet dans votre Desktop, c'est la seul truc car j'ai modifier cette fichier original par excel (pas nettoyage mais seulement je les transformé en ligne au lieu de colonne)

ODD<-read.csv("C:/Users/khali/Desktop/ODD_DEP2.csv",header = TRUE)
#-----------------------------------------------------exo 2------------------------------------------------------------------------------

# construire le matrice A:

#construction de vecteur c qui contient tous les noms des départements
c<-c(1:101)
for (i in 0:100)
  c[i+1]<-ODD[7*i+1,2]
f<-c(1:101)
A<-data.frame(row.names=c,taux.pvt<-f,part.actifs.stables<-f,part.depl.dom.trav<-f,moy.dist.min.hp<-f,moy.dist.km<-f,ges<-f)
for (j in 1:6)
  for (i in 1:101)
    A[i,j]=0
# construire le variable taux de pauvreté total (2016)
for (i in 0:100)
A[i+1,1]<-ODD[1+7*i,28]
# construire la part des actifs stables parmi les actifs ayant un emploi (2016)
for (i in 0:100)
  A[i+1,2]<-ODD[39794+3*i,28]
# construire la parts modales des déplacements domicile-travail en voiture et en transports en commun (2016) (j'ai fait la somme pour le voiture et le transports en commun)
for (i in 0:100)
  A[i+1,3]<-ODD[39089+7*i,28]+ODD[39089+7*i+2,28]
# construire la durée moyenne des navettes domicile-travail pour les actifs occupés (minutes) (2016)
for (i in 0:100)
  A[i+1,4]<-ODD[38277+2*i,28]
# construire la distance moyenne des navettes domicile-travail pour les actifs occupés (km) (2016)
for (i in 0:100)
  A[i+1,5]<-ODD[38479+2*i,28]
# construire l'émissions en équivalent CO2 par gaz (2016) (j'ai fait la somme des 7 gaz méthane, co2 origine biomasse et non biomasse, hydrofluorocarbures, 
#protoxyde d'azote, perfluorocarbures et hexafluorure de soufre)
# j'utilise dans cette variable les données de l'année la plus proche (2012) car Grace à mon observation de l'année 2007 à l'année 2012, il n y a pas un grand changement dans cette variable.
o=matrix(nrow = 101,ncol=7)
for (j in 1:7)
  for (i in 1:101)
    o[i,j]<-0
for (i in 0:100)
  o[i+1,]<-c(ODD[61680+7*i,24],ODD[61680+1+7*i,24],ODD[61680+2+7*i,24],ODD[61680+3+7*i,24],ODD[61680+4+7*i,24],ODD[61680+5+7*i,24],ODD[61680+6+7*i,24])
  
for (i in 0:95)
  A[i+1,6]<-sum(o[i+1,],na.rm =TRUE)
for (i in 96:100)
  A[i+1,6]<-sum(o[i+1,])
#-------------------------------------exo3--------------------------------------
# étude descriptive pour le variable taux de pauvreté total (2016):

#calcule de la moyenne,médiane, max, min :
summary(A[,1],na.rm=TRUE)

#calcule de la variance et de l'écart-type:
var(A[,1],na.rm=TRUE)
sd(A[,1],na.rm=TRUE)

#diagramme en bâtons de la variable taux de pauvreté total (2016):
s<-c(1:101)
for (i in 0:100)
  s[i+1]<-ODD[1+7*i,2]
barplot(A[,1],xlab = "département",ylab = "taux de pauvreté total en %",ylim = c(0,40), main = "diagramme en bâtons",names.arg =s)

#histogramme de la variable taux de pauvreté total (2016):
hist(A[,1],xlab ="taux de pauvreté total en %",ylab = "frequence",main = "histogramme" )

#boîte à moustaches de la variable taux de pauvreté total (2016):
boxplot(A[,1],main="boîte à moustaches",ylab="taux de pauvreté total en %")

#carte de france métropolitaine pour le variable taux de pauvreté total (2016):
FranceFormes <- getData(name="GADM", country="FRA", level=2)
idx <- match(FranceFormes$NAME_2, c)
concordance <- A[idx,1]
FranceFormes$taux_pv_tot <- concordance
couleurs <- colorRampPalette(c('white', 'red'))
spplot(FranceFormes,"taux_pv_tot" ,col.regions=couleurs(30),  main=list(label="taux de pauvreté total en % (2016)",cex=.8))
#------------------------------------------------------------------------------------
# étude descriptive pour le part des actifs stables parmi les actifs ayant un emploi (2016):

#calcule de la moyenne,médiane, max, min :
summary(A[,2],na.rm=TRUE)

#calcule de la variance et de l'écart-type:
var(A[,2],na.rm=TRUE)
sd(A[,2],na.rm=TRUE)

#diagramme en bâtons de la part des actifs stables parmi les actifs ayant un emploi (2016) :
barplot(A[,2],xlab = "département",ylab = "part des actifs stables en %",ylim = c(0,70), main = "diagramme en bâtons",names.arg =s)

#histogramme de la part des actifs stables parmi les actifs ayant un emploi (2016):
hist(A[,2],xlab ="part des actifs stables en %",ylab = "frequence",main = "histogramme" )

#boîte à moustaches de la part des actifs stables parmi les actifs ayant un emploi (2016):
boxplot(A[,2],main="boîte à moustaches",ylab="part des actifs stables en %")

#carte de france métropolitaine pour la part des actifs stables parmi les actifs ayant un emploi (2016) :
FranceFormes <- getData(name="GADM", country="FRA", level=2)
idx <- match(FranceFormes$NAME_2, c)
concordance <- A[idx,2]
FranceFormes$part_act_sta <- concordance
couleurs <- colorRampPalette(c('white', 'blue'))
spplot(FranceFormes,"part_act_sta" ,col.regions=couleurs(30),  main=list(label=" part actif stable parmi les actifs ayant un emploi en % (2016)",cex=.8))

#-----------------------------------------------------------------------------------------
# étude descriptive pour la parts modales des déplacements domicile-travail en voiture et en transports en commun (2016):

#calcule de la moyenne,médiane, max, min :
summary(A[,3],na.rm=TRUE)

#calcule de la variance et de l'écart-type:
var(A[,3],na.rm=TRUE)
sd(A[,3],na.rm=TRUE)

#diagramme en bâtons de la parts modales des déplacements domicile-travail en voiture et en transports en commun (2016) :
barplot(A[,3],xlab = "département",ylab = "parts modales des déplacements en %",ylim = c(0,100), main = "diagramme en bâtons",names.arg =s)

#histogramme de la parts modales des déplacements domicile-travail en voiture et en transports en commun (2016):
hist(A[,3],xlab ="parts modales des déplacements en %",ylab = "frequence",main = "histogramme" )

#boîte à moustaches de la parts modales des déplacements domicile-travail en voiture et en transports en commun (2016):
boxplot(A[,3],main="boîte à moustaches",ylab="parts modales des déplacements en %")

#carte de france métropolitaine pour le variable taux de pauvreté total (2016):
FranceFormes <- getData(name="GADM", country="FRA", level=2)
idx <- match(FranceFormes$NAME_2, c)
concordance <- A[idx,3]
FranceFormes$part_m_d <- concordance
couleurs <- colorRampPalette(c('white', 'brown'))
spplot(FranceFormes,"part_m_d" ,col.regions=couleurs(30),  main=list(label="parts modales des déplacements domicile-travail en voiture et en transports en commun en % (2016)",cex=.8))
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# étude descriptive pour la durée moyenne des navettes domicile-travail pour les actifs occupés (minutes) (2016):

#calcule de la moyenne,médiane, max, min :
summary(A[,4],na.rm=TRUE)

#calcule de la variance et de l'écart-type:
var(A[,4],na.rm=TRUE)
sd(A[,4],na.rm=TRUE)

#diagramme en bâtons de la durée moyenne des navettes domicile-travail pour les actifs occupés (minutes) (2016) :
barplot(A[,4],xlab = "département",ylab = "durée moyenne des navettes en minutes",ylim = c(0,40), main = "diagramme en bâtons",names.arg =s)

#histogramme de la durée moyenne des navettes domicile-travail pour les actifs occupés (minutes) (2016):
hist(A[,4],xlab ="durée moyenne des navettes en minutes",ylab = "frequence",main = "histogramme" )

#boîte à moustaches de la durée moyenne des navettes domicile-travail pour les actifs occupés (minutes) (2016):
boxplot(A[,4],main="boîte à moustaches",ylab="durée moyenne des navettes en minutes")

#carte de france métropolitaine pour la durée moyenne des navettes domicile-travail pour les actifs occupés (minutes) (2016)
FranceFormes <- getData(name="GADM", country="FRA", level=2)
idx <- match(FranceFormes$NAME_2, c)
concordance <- A[idx,4]
FranceFormes$m_du_n <- concordance
couleurs <- colorRampPalette(c('white', 'orange'))
spplot(FranceFormes,"m_du_n" ,col.regions=couleurs(30),  main=list(label="durée moyenne des navettes domicile-travail pour les actifs occupés en minutes (2016)",cex=.8))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# étude descriptive pour la distance moyenne des navettes domicile-travail pour les actifs occupés (km) (2016):

#calcule de la moyenne,médiane, max, min :
summary(A[,5],na.rm=TRUE)

#calcule de la variance et de l'écart-type:
var(A[,5],na.rm=TRUE)
sd(A[,5],na.rm=TRUE)

#diagramme en bâtons de la distance moyenne des navettes domicile-travail pour les actifs occupés (km) (2016) :
barplot(A[,5],xlab = "département",ylab = "distance moyenne des navettes en km",ylim = c(0,30), main = "diagramme en bâtons",names.arg =s)

#histogramme de la distance moyenne des navettes domicile-travail pour les actifs occupés (km) (2016) :
hist(A[,5],xlab ="distance moyenne des navettes en km",ylab = "frequence",main = "histogramme" )

#boîte à moustaches de la distance moyenne des navettes domicile-travail pour les actifs occupés (km) (2016):
boxplot(A[,5],main="boîte à moustaches",ylab="distance moyenne des navettes en km")

#carte de france métropolitaine pour la distance moyenne des navettes domicile-travail pour les actifs occupés (km) (2016)
FranceFormes <- getData(name="GADM", country="FRA", level=2)
idx <- match(FranceFormes$NAME_2, c)
concordance <- A[idx,5]
FranceFormes$m_di_n <- concordance
couleurs <- colorRampPalette(c('white', 'purple'))
spplot(FranceFormes,"m_di_n" ,col.regions=couleurs(30),  main=list(label="distance moyenne des navettes domicile-travail pour les actifs occupés en km (2016)",cex=.8))
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# étude descriptive pour l'émissions en équivalent CO2 par gaz (2016):

#calcule de la moyenne,médiane, max, min :
summary(A[,6],na.rm=TRUE)

#calcule de la variance et de l'écart-type:
var(A[,6],na.rm=TRUE)
sd(A[,6],na.rm=TRUE)

#diagramme en bâtons de l'émissions en équivalent CO2 par gaz (tonnes) (2016) :
barplot(A[,6],xlab = "département",ylab = "l'émissions en équivalent CO2 par gaz (tonnes)",ylim = c(0,26677088), main = "diagramme en bâtons",names.arg =s)

#diagramme en bâtons de l'émissions en équivalent CO2 par gaz (tonnes) (2016) :
hist(A[,6],xlab ="l'émissions en équivalent CO2 par gaz (tonnes)",ylab = "frequence",main = "histogramme" )

#boîte à moustaches de l'émissions en équivalent CO2 par gaz (tonnes) (2016):
boxplot(A[,6],main="boîte à moustaches",ylab="l'émissions en équivalent CO2 par gaz (tonnes)")

#carte de france métropolitaine pour l'émissions en équivalent CO2 par gaz (tonnes) (2016)
FranceFormes <- getData(name="GADM", country="FRA", level=2)
idx <- match(FranceFormes$NAME_2, c)
concordance <- A[idx,6]
FranceFormes$co <- concordance
couleurs <- colorRampPalette(c('white', 'black'))
spplot(FranceFormes,"co" ,col.regions=couleurs(30),  main=list(label="l'émissions en équivalent CO2 par gaz en tonnes (2016)",cex=.8))
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------exo4------------------------------------------------------------------------------
#analyse factorielle ACP:
# Les données étant exprimées dans des unités différentes, nous avons intérêt à réaliser une ACP normée (centrée et réduite) sur les 6 variables 
B<-as.matrix(A)
for (j in 1:6) 
 {cc<-mean(B[,j],na.rm=TRUE)
  b<-sd(B[,j],na.rm = TRUE)
  for (i in 1:101)
    {B[i,j]<-(B[i,j]-cc)/b}
}
B[97,1]<-0
B[99,1]<-0
B[101,1]<-0
for (i in 97:101)
  B[i,6]<-0
#j'utilise le library factominR pour réaliser l'ACP
acp3<-PCA(B,graph = FALSE)
dimdesc(acp3)
plot.PCA(acp3)
plot.PCA(acp3,choix = "var")
summary.PCA(acp3,nbelements = 101)

#------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------exo5---------------------------------------------------------------

#j'utilise le library dudi pour la classification en (ACP) et j'utilise le critère de Ward
acp2<-dudi.pca(B,scannf = FALSE)
#classification sur les départements
disto<-dist.dudi(acp2,amongrow = TRUE)
cah1<-hclust(disto^2,method = "ward.D")
plot(cah1,hang=-1)
rect.hclust(cah1,h=35)
classe<-cutree(cah1,h=35)
print(classe)
s.class(dfxy = acp2$li,fac=as.factor(classe))
#classification sur les variables
disto.var<-dist.dudi(acp2,amongrow = FALSE)
cah2<-hclust(disto.var^2,method = "ward.D")
plot(cah2,hang=-1)
rect.hclust(cah2,h=2)


