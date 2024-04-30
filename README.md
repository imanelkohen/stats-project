# stats-project
EC124 Data set

View(micro_world_139countries)
attach(micro_world_139countries)
my_data <- micro_world_139countries

# Calculate how many people in each region reported to have a mobile money account 
my_data$account_mob <- as.character(my_data$account_mob)
((sum(my_data$account_mob == "1" & my_data$regionwb == "South Asia", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "South Asia", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "South Asia", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "East Asia & Pacific (excluding high income)", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "East Asia & Pacific (excluding high income)", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "East Asia & Pacific (excluding high income)", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "High income", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "High income", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "High income", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "Latin America & Caribbean (excluding high income)", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "Latin America & Caribbean (excluding high income)", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "Latin America & Caribbean (excluding high income)", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "Middle East & North Africa (excluding high income)", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "Middle East & North Africa (excluding high income)", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "Middle East & North Africa (excluding high income)", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "Sub-Saharan Africa (excluding high income)", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "Sub-Saharan Africa (excluding high income)", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "Sub-Saharan Africa (excluding high income)", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "Europe & Central Asia (excluding high income)", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "Europe & Central Asia", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "Europe & Central Asia (excluding high income)", na.rm = TRUE))) *100)

# Create barplot
data2 <- c(22.29199, 36.50935, 7.184521, 20.1095, 20.4, 28.59346, 10.663)
my_matrix <- matrix(c(22.29199,77.70801,36.50935,63.49065,7.184521,92.815479,20.1095,79.8905,20.4,79.6,28.59346,71.40654,10.663,89.337), nrow=2)
par(mar=c(5,12,4,1)+.1)
barplot(my_matrix, beside = FALSE, names.arg = c("Europe & Central Asia", "Sub-Saharan Africa", "Middle East & North Africa", "Latin America & Caribbean", "High income", "East Asia & Pacific", "South Asia"), horiz = TRUE, las = 1, xlab = "Percentage of mobile money account holders (%)")
title(ylab = "Region", line=11.5, cex.lab=1)

# ANOVA on regions and mobile money account (KIV, am reviewing if there's any other test that's more suitable. But from this one, the hypothesis "Is the number of mobile money account owners different between regions?" is rejected)
library(tidyverse)
dat <- my_data %>%
  select(regionwb, account_mob)
summary (dat)
res_aov <- aov(account_mob ~ regionwb,
               data = dat)
summary(res_aov)
oneway.test(account_mob ~ regionwb, data = dat, var.equal = FALSE)

