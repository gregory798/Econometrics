
library(DataExplorer)
library(tidyverse)
library(forcats)

# DATA ----

# install.packages("forcats")
data(gss_cat)

head(ames)

ames


ames %>% glimpse()

# 1.0 EDA Report ----

ames %>%
  create_report(
    output_file  = "ames_report",
    output_dir   = "ames/",
    y            = "Sale_Price",
    report_title = "EDA Report - Ames Housing"
  )

# 2.0 Data Introduction ----

ames %>% introduce()

gss_cat %>% plot_intro()

# 3.0 Missing Values ----

gss_cat %>% plot_missing()

gss_cat %>% profile_missing()

# 4.0 Continuous Features ----

gss_cat %>% plot_density()

gss_cat %>% plot_histogram()

# 5.0 Categorical Features ----

ames %>% plot_bar()

# 6.0 Relationships ----

ames %>% plot_correlation(maxcat = 5)
summary(ames)

library("dplyr")

ames_num = select_if(ames, is.numeric)
sapply(ames, class)


sapply(ames, class)
correlation = cor(ames_num, ames_num$Sale_Price, method = "pearson")
names = rownames(correlation)
abs_cor = abs(correlation)
data = data.frame(X_var = names,abs_cor = abs_cor,cor = correlation)
data[order(data$abs_cor,decreasing = TRUE),]
View(data)
