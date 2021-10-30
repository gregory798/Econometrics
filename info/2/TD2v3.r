
#TD2

# 1) Importer


# Read semicolon (";") separated values
df = read.csv2(file.choose())

head(df)

dim(df)

summary(df)

str(df)

class(df)
df$roe = as.numeric(df$roe)
str(df)




# 2) On s'attend a un signe positif

mean_roe = mean(df$roe)
mean_roe

var_roe = var(df$roe)
var_roe

mean_salary = mean(df$salary)
mean_salary

cc = cov(df$roe, df$roe)
cc

covrs = cov(df$roe, df$salary)
covrs

# Formules : hb1 = cov/var_roe ; hb0 = mean_salary - (hb1*mean_roe)

hb1 = covrs/var_roe
hb1

hb0 = mean_salary-(hb1*mean_roe)
hb0

r1 = lm(salary ~ roe, df)
summary(r1)



# Y est une variable aléatoire


# The coefficient t-value is a measure of how many standard deviations our coefficient estimate is far away from 0. 
# We want it to be far away from zero as this would indicate we could reject the null hypothesis - that is, we could 
# declare a relationship between speed and distance exist. In our example, the t-statistic values are relatively far 
# away from zero and are large relative to the standard error, which could indicate a relationship exists.
# In general, t-values are also used to compute p-values.



# Rprésentation graphique

coeff = coefficients(r1)

coeff


eq = paste0("salary = ", round(coeff[1],1), " + ", round(coeff[2],1), " roe " )

plot(df$roe,df$salary,
     main=eq,
     xlab="roe",
     ylab="salary",
     abline(r1,col = "red")
)

# Multpile R-Squared : pourcentage de variation
# causé par la variable indépendante

# tests d'hypothèses




summary(r1)

tval1 = summary(r1)$coef[1] / summary(r1)$coef[3]
tval1 # 4.517 > 2.58 --> Significatif a 1, 5 et 10%

tval2 = summary(r1)$coef[2] / summary(r1)$coef[4]
tval2 # 1.663 < 1.96 --> Significatif a 10% seulement




df$consprod

r2 = lm(salary ~ consprod, df)
summary(r2)




coeff2 = coefficients(r2)

coeff2

eq2 = paste0("y = ", round(coeff2[1],1), " + ", round(coeff2[2],1), " x " )
eq2

plot(df$consprod,df$salary,
     main=eq2,
     xlab="consprod",
     ylab="salary",
     abline(r2,col = "red")
)


mean(df[df$consprod == 1,"salary"])
#1722.417 de salaire en moyenne dans secteyr biens conso

mean(df[df$consprod == 0,"salary"])
#1103.416 de salaire moyen dans les autres secteurs


# difference de salaire = 619$, stat diff au seuil de 0.1%, donc oui ? 
