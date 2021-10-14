
#TD2

# 1) Importer


# Read semicolon (";") separated values
df = read.csv2(file.choose())

str(df)
df$roe = as.numeric(df$roe)
str(df)




# 2) On s'attend a un signe positif

mean_roe = mean(df$roe)
mean_roe

var_roe = var(df$roe)
var_roe

mean_salary = mean(df$salary)
mean_salary

covrs = cov(df$roe, df$salary)
covrs

# Formules : hb1 = cov/var_roe ; hb0 = mean_salary - (hb1*mean_roe)

hb1 = covrs/var_roe
hb1

hb0 = mean_salary-(hb1*mean_roe)
hb0

r1 = lm(salary ~ roe, df)
summary(r1)



# Rprésentation graphique

coeff = coefficients(r1)

eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))

plot(df$roe,df$salary,
     main=eq,
     xlab="roe",
     ylab="salary",
     abline(r1,col = "red")
     )

# Multpile R-Squared : pourcentage de variation
# causé par la variable indépendante

# TODO : tests d'hypothèses


