# stats-project
EC124 Data set
# Calculate how many people in each region reported to have a mobile money account 
((sum(my_data$account_mob == "1" & my_data$regionwb == "South Asia", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "South Asia", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "South Asia", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "East Asia & Pacific (excluding high income)", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "East Asia & Pacific (excluding high income)", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "East Asia & Pacific (excluding high income)", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "High income", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "High income", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "High income", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "Latin America & Caribbean (excluding high income)", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "Latin America & Caribbean (excluding high income)", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "Latin America & Caribbean (excluding high income)", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "Middle East & North Africa (excluding high income)", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "Middle East & North Africa (excluding high income)", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "Middle East & North Africa (excluding high income)", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "Sub-Saharan Africa (excluding high income)", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "Sub-Saharan Africa (excluding high income)", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "Sub-Saharan Africa (excluding high income)", na.rm = TRUE))) *100)
((sum(my_data$account_mob == "1" & my_data$regionwb == "Europe & Central Asia (excluding high income)", na.rm = TRUE))/((sum(my_data$account_mob == "1" & my_data$regionwb == "Europe & Central Asia", na.rm = TRUE) + sum(my_data$account_mob == "0" & my_data$regionwb == "Europe & Central Asia (excluding high income)", na.rm = TRUE))) *100)

# Create barplot
data2 <- c(22.29199, 36.50935, 7.184521, 20.1095, 20.4, 28.59346, 10.663)
barplot(data2, names.arg = c("Europe & Central Asia", "Sub-Saharan Africa", "Middle East & North Africa", "Latin America & Caribbean", "High income", "East Asia & Pacific", "South Asia"))
