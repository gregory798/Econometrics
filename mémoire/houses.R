install.packages("modeldata")

library(modeldata)

data("ames", package = "modeldata")

ames = as.data.frame(ames)

sort (colnames(ames))

ames = subset(
  ames,
  ames$Sale_Condition == "Normal" &
    ames$Bldg_Type == "OneFam"
)

dim(ames) 


ames$Year_Frac = ames$Year_Sold + ames$Mo_Sold/12
ames$Bsmt_Fin_Sf = ames$Total_Bsmt_SF - ames$Bsmt_Unf_SF


plot(ames$Year_Frac,    ames$Sale_Price)
plot(ames$Gr_Liv_Area,    ames$Sale_Price)
plot(ames$Bedroom_AbvGr,    ames$Sale_Price)
plot(ames$Full_Bath,    ames$Sale_Price)
plot(ames$Half_Bath,    ames$Sale_Price)
plot(ames$TotRms_AbvGrd,    ames$Sale_Price)
plot(ames$Total_Bsmt_SF,    ames$Sale_Price)
plot(ames$Bsmt_Unf_SF,    ames$Sale_Price)
plot(ames$Year_Frac,    ames$Sale_Price)
plot(ames$Year_Frac,    ames$Sale_Price)




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




old_town = subset (ames, ames$Neighborhood == "Old_Town")

m2 = lm(
  Sale_Price ~
    Gr_Liv_Area + 
    Bsmt_Unf_SF + 
    Bsmt_Fin_Sf +
    Full_Bath + 
    Half_Bath + 
    Bedroom_AbvGr + 
    TotRms_AbvGrd,
  data = old_town
)

summary(m2)

m21 = lm(
  Sale_Price ~
    Gr_Liv_Area + 
    Bsmt_Unf_SF + 
    Bsmt_Fin_Sf +
    Bedroom_AbvGr + 
    TotRms_AbvGrd,
  data = old_town
)

summary(m21)












