
#https://cran.r-project.org/web/packages/AmesHousing/index.html

install.packages("modeldata")

library(modeldata)

data("ames", package = "modeldata")

ames = as.data.frame(ames)

sort(colnames(ames))

ames = subset(
  ames,
  ames$Sale_Condition == "Normal" &
    ames$Bldg_Type == "OneFam"
)

dim(ames) 



p = lm(Gr_Liv_Area ~ Sale_Price, data = ames)
plot(ames$Gr_Liv_Area,ames$Sale_Price)
abline(p, col="red")

plot(ames$Total_Bsmt_SF,ames$Sale_Price)

plot(ames$Bedroom_AbvGr,ames$Sale_Price)
plot(ames$Full_Bath,ames$Sale_Price)
plot(ames$Half_Bath,ames$Sale_Price)
plot(ames$TotRms_AbvGrd,ames$Sale_Price)
plot(ames$Bsmt_Unf_SF,ames$Sale_Price)




summary (lm(Sale_Price ~ Bsmt_Unf_SF +  Bsmt_Fin_Sf, data = ames))
summary (lm(Sale_Price ~ Bsmt_Unf_SF +  Total_Bsmt_SF, data = ames))
summary (lm(Sale_Price ~ Bsmt_Unf_SF +  Bsmt_Fin_Sf + Total_Bsmt_SF, data = ames))


m1 = lm(
  Sale_Price ~ 
    Gr_Liv_Area + 
    Bsmt_Unf_SF + 
    Bsmt_Fin_Sf +
    Full_Bath + 
    Half_Bath + 
    Bedroom_AbvGr + 
    TotRms_AbvGrd,
  data = ames
)

summary(m1)

a1 = lm(
  Sale_Price ~ 
    Gr_Liv_Area + 
    Bsmt_Unf_SF + 
    Bsmt_Fin_Sf +
    Full_Bath,
  data = ames
)

summary(a1)



g = subset (ames, ames$Neighborhood == "Gilbert")

m2 = lm(
  Sale_Price ~
    Gr_Liv_Area + 
    Bsmt_Unf_SF + 
    Bsmt_Fin_Sf +
    Full_Bath + 
    Half_Bath + 
    Bedroom_AbvGr + 
    TotRms_AbvGrd,
  data = g
)

summary(m2)

m21 = lm(
  Sale_Price ~
    Gr_Liv_Area + 
    Bsmt_Unf_SF + 
    Bsmt_Fin_Sf +
    Bedroom_AbvGr + 
    TotRms_AbvGrd,
  data = g
)

summary(m21)

a = lm(
  Sale_Price ~
    Gr_Liv_Area,
  data = ames
)

summary(m3)

abline(a)

#create dataset
data <- data.frame(x = c(1, 1, 2, 4, 4, 5, 6, 7, 7, 8, 9, 10, 11, 11),
                   y = c(13, 14, 17, 23, 24, 25, 25, 24, 28, 32, 33, 35, 40, 41))

#fit simple linear regression model
model <- lm(y ~ x, data = data)

#create scatterplot of data
plot(data$x, data$y)

#add fitted regression line
abline(model, col="red")







