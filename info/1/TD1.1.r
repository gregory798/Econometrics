#Exo 1

classe = data.frame(Genre=c("Homme","Femme","Homme","Femme","Femme","Femme"),
                    Poids=c(90,50,80,50,54,NA),
                    Taille=c(1.70,1.67,1.83,1.62,1.65,1.68),
                    row.names=c("Pablo", "Lara", "Pierre", "Morgane", "Myriam","Emna")
)


classe

classe$IMC = classe$Poids/(classe$Taille^2)

str(classe)

classe$Femme = classe$Genre == "Femme"

classe$IMC = round(classe$IMC,1)

i = mean(classe$IMC, na.rm = TRUE)
i

ggplot(classe, aes(x=Poids, y=Taille, colour=Genre)) + geom_point()

hist(classe$IMC, probability = TRUE,
     main = "IMC de la classe",
     xlab = "IMC",
     ylab = "Proportion",
     col = "green"
)

ggplot(classe, aes(x=Taille, fill=Genre, color=Genre)) +
        geom_histogram()


plot(classe$Taille,
     classe$Poids,  
     xlab="Taille (m)",
     ylab="Poids (kg)",
     main="Poids en fonction de la taille")

ggplot(classe, aes(x=Taille, y=Poids, colour=Genre)) + geom_point()


classe$Indice[ classe$IMC >= 16.5 & classe$IMC <= 18.5] = "maigreur"
classe$Indice[ classe$IMC > 18.5 & classe$IMC <= 25] = "corpulence normale"
classe$Indice[ classe$IMC > 25 & classe$IMC <= 30] = "surpoids"
classe$Indice[ classe$IMC > 30 & classe$IMC <= 35] ="obésité modérée"


table(classe$Indice,classe$Genre)

tapply(classe$Poids,classe$Genre,summary)


################################################

#Exo 2

# Read tab separated values
read.delim(file.choose())
# Read comma (",") separated values
read.csv(file.choose())
# Read semicolon (";") separated values
read.csv2(file.choose())

ISF_2014 = read.csv2(file.choose())

head(ISF_2014)

ggplot(ISF_2014, aes(x = patrimoine_moy, y = impot_moy, colour = region)) + geom_point() + guides(color = FALSE)

#Nb commune totale :
length(ISF_2014$codecom)
str((ISF_2014$codecom))
#423

#Nb communes uniques :
str(unique(ISF_2014$codecom))
#381

nr = read.csv2(file.choose())

head(nr)

ISF_2014v2 = merge(nr,ISF_2014)

tapply(ISF_2014v2$patrimoine_moy,ISF_2014v2$nouvelle_region,summary)

tapply(ISF_2014v2$patrimoine_moy,ISF_2014v2$region,summary)
