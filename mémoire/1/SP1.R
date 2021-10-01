# Suivi Projet 1

# 1)
# 2)a)b)c)
# 3)a)b)


# c) Créer des données :



# objet                       |Nom<-instructions 
# vecteur                     |Nom<-c(valeurs) 
# Base de données 
# (individus*variable)  
#                             |Nom<-data.frame(nom.variable1=c(valeurs), 
#                             |nom.variable2=c(valeurs),row.names=c(valeurs))


# Pour supprimer un objet: 
# rm(nom)


# Pour créer la base de données suivante appelée vente, la commande sera la suivante :

vente<-data.frame(produit=c("robe","pantalon","chaussures"),
                  prix=c(70,45,50),
                  volume=c(4,5,7),
                  row.names=c("client1","client2","client3"))


vente

# d) Importer des données : (import Dataset) ou :

###################################################
# Read tab separated values
read.delim(file.choose())
# Read comma (",") separated values
read.csv(file.choose())
# Read semicolon (";") separated values
read.csv2(file.choose())

#####################################################




# e) Appariement de données : Combiner 2 tables

#Création des 2 bases :  
X1<-data.frame(Id=1:4,SEXE=c("H","F","F","H"),Poids=c(75,68,48,72)) 

X1

X3<-data.frame(Id=c(2,1,4,3),SEXE=c("F","H","H","F"),Taille=c(165,182,178,160))

X3

Z<-merge(X1,X3)

Z


# Pour plus d’informations se référer à l’aide en ligne : 
help(merge)


# f)

# 4) a) 

str(X1)
# sexe <- as.factor(sexe)

# b)


# c)




vente$recette = vente$prix * vente$volume

vente$recette

vente$robe = vente$produit == "robe"



vente$trecette[vente$recette<275] <- "LT 275"
vente$trecette[vente$recette >=275 & vente$recette<=300] <- "[275-300]"
vente$trecette[vente$recette>300] <- "GT 300"


# d)


# 5) Décrire les données 

# Importer la base de données Initiation.


# a)

# Moyenne                                               mean(x)  
# Écart-type                                            sd(x)  
# Minimum, maximum                                      min(x), max(x)  
# Étendue                                               range(x)  
# Médiane, quantile                                     median(x), quantile(x)  
# Résumé́  d’une variable                                summary(x)  
# Histogramme                                           hist(x)  
# Fonction de densité́                                   density(x)  
# Fonction de répartition empirique                     ecdf(x)  
# Boite à  moustache                                    boxplot(x)  
# Tableau de fréquence                                  table(x)  
# Tableau répartition en %                              prop.table(x)  
# Nuage de points                                       plot(x,y)  
# Coefficient de corrélation                            cor(x,y) ou cor.test(x,y)

#  (moi)

plot(Initiation$poids, Initiation$taille)


cor.test(Initiation$poids, Initiation$taille)


# Exemples :

summary(Initiation$poids)

hist(Initiation$poids, main = "Poids des étudiants",
     xlab = "Poids", ylab = "Effectif")

table(Initiation$genre)

plot(Initiation$poids,Initiation$taille,main = "Poids et taille des é tudiants",
     xlab = "Poids", ylab = "taille") 




# b)



library(abind, pos=17)
library(e1071, pos=18)


numSummary(Initiation[,"poids", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))



# 6) Les commandes sur R pour effectuer la régression linéaire

# L’estimation par les MCO s’effectuent sous R à  l’aide de la commande :
# Nomregression <- lm(y~x1 x2, data=nom)


RegModel.1 = lm(poids~Nb_frere+Nb_soeur+taille, data = Initiation)


summary(RegModel.1)

# 7)
# 8)


