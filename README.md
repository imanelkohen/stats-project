# stats-project
EC124 Data set
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
barplot(my_matrix, beside = FALSE, names.arg = c("Europe & Central Asia", "Sub-Saharan Africa", "Middle East & North Africa", "Latin America & Caribbean", "High income", "East Asia & Pacific", "South Asia"), horiz = TRUE)

