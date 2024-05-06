# stats-project

# Import dataset and install necessary packages
install.packages("haven")
install.packages("scales")
install.packages("tidyverse")
install.packages("stats")
library(haven)
library(scales)
library(tidyverse)
library(stats)
my_data <- read_dta("micro_world_139countries.dta")
attach(micro_world_139countries)


# Calculate how many people in each region reported to have a mobile money account 
my_data$account_mob <- as.character(my_data$account_mob)
(sum(my_data$account_mob == "1" & my_data$regionwb == "South Asia", na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$regionwb == "South Asia", na.rm = TRUE))*100
(sum(my_data$account_mob == "1" & my_data$regionwb == "East Asia & Pacific (excluding high income)", na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$regionwb == "East Asia & Pacific (excluding high income)", na.rm = TRUE)) *100
(sum(my_data$account_mob == "1" & my_data$regionwb == "High income", na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$regionwb == "High income", na.rm = TRUE))*100
(sum(my_data$account_mob == "1" & my_data$regionwb == "Latin America & Caribbean (excluding high income)", na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$regionwb == "Latin America & Caribbean (excluding high income)", na.rm = TRUE))*100
(sum(my_data$account_mob == "1" & my_data$regionwb == "Middle East & North Africa (excluding high income)", na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1")& my_data$regionwb == "Middle East & North Africa (excluding high income)", na.rm = TRUE))*100
(sum(my_data$account_mob == "1" & my_data$regionwb == "Sub-Saharan Africa (excluding high income)", na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$regionwb == "Sub-Saharan Africa (excluding high income)", na.rm = TRUE))*100
(sum(my_data$account_mob == "1" & my_data$regionwb == "Europe & Central Asia (excluding high income)", na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$regionwb == "Europe & Central Asia (excluding high income)", na.rm = TRUE))*100

# Create barplot
data2 <- c(18.2285, 36.50935, 7.184521, 20.1095, 20.4, 28.59346, 10.663)
my_matrix <- matrix(c(18.2285,81.7715,36.50935,63.49065,7.184521,92.815479,20.1095,79.8905,20.4,79.6,28.59346,71.40654,10.663,89.337), nrow=2)
par(mar=c(5,12.6,4,6)+.4)
barplot(my_matrix, beside = FALSE, names.arg = c("Europe & Central Asia", "Sub-Saharan Africa", "Middle East & North Africa", "Latin America & Caribbean", "High income", "East Asia & Pacific", "South Asia"), horiz = TRUE, las = 1, xlab = "Percentage of mobile money account holders (%)",  col = c("pink", "grey"), main = "Does the respondent own a mobile money account?")
title(ylab = "Region", line=11.7, cex.lab=1)
par(xpd = TRUE)
legend(2, 4, inset=c(-0.4,0), legend=c("Yes", "No"), fill = c("pink","grey"), x = "topright")


# Age group - gender barplot 
my_data$account <- as.character(my_data$account)
my_data$female <- as.character(my_data$female)
my_data$age <- as.character(my_data$age)
my_data$account_fin <- as.character(my_data$account_fin)

# Percentage of women who have a financial institution account in the 15-24 group
(sum(my_data$account_fin == "1" & my_data$female == "1" & my_data$age %in% c("15":"24"), na.rm = TRUE))/(sum(my_data$account_fin %in% c("0":"1") & my_data$female == "1" & my_data$age %in% c("15":"24"), na.rm = TRUE)) *100

# Percentage of women who have a mobile money account in the 15-24 group
(sum(my_data$account_mob == "1" & my_data$female == "1" & my_data$age %in% c("15":"24"), na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$female == "1" & my_data$age %in% c("15":"24"), na.rm = TRUE)) *100

# Percentage of men who have a financial institution account in the 15-24 group
(sum(my_data$account_fin == "1" & my_data$female == "2" & my_data$age %in% c("15":"24"), na.rm = TRUE))/(sum(my_data$account_fin %in% c("0":"1") & my_data$female == "2" & my_data$age %in% c("15":"24"), na.rm = TRUE)) *100

# Percentage of men who have a mobile money account in the 15-24 group
(sum(my_data$account_mob == "1" & my_data$female == "2" & my_data$age %in% c("15":"24"), na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$female == "2" & my_data$age %in% c("15":"24"), na.rm = TRUE)) *100

# Percentage of women who have a financial institution account in the 25-50 group
(sum(my_data$account_fin == "1" & my_data$female == "1" & my_data$age %in% c("25":"50"), na.rm = TRUE))/(sum(my_data$account_fin %in% c("0":"1") & my_data$female == "1" & my_data$age %in% c("25":"50"), na.rm = TRUE)) *100

# Percentage of women who have a mobile money account in the 25-50 group  
(sum(my_data$account_mob == "1" & my_data$female == "1" & my_data$age %in% c("25":"50"), na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$female == "1" & my_data$age %in% c("25":"50"), na.rm = TRUE)) *100

# Percentage of men who have a financial institution account in the 25-50 group
(sum(my_data$account_fin == "1" & my_data$female == "2" & my_data$age %in% c("25":"50"), na.rm = TRUE))/(sum(my_data$account_fin %in% c("0":"1") & my_data$female == "2" & my_data$age %in% c("25":"50"), na.rm = TRUE)) *100

# Percentage of men who have a mobile money account in the 25-50 group  
(sum(my_data$account_mob == "1" & my_data$female == "2" & my_data$age %in% c("25":"50"), na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$female == "2" & my_data$age %in% c("25":"50"), na.rm = TRUE)) *100

# Percentage of women who have a financial institution account in the 51+ group
(sum(my_data$account_fin == "1" & my_data$female == "1" & my_data$age %in% c("51":"99"), na.rm = TRUE))/(sum(my_data$account_fin %in% c("0":"1") & my_data$female == "1" & my_data$age %in% c("51":"99"), na.rm = TRUE)) *100

# Percentage of women who have a mobile money account in the 51+ group
(sum(my_data$account_mob == "1" & my_data$female == "1" & my_data$age %in% c("51":"99"), na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$female == "1" & my_data$age %in% c("51":"99"), na.rm = TRUE)) *100

# Percentage of men who have a financial institution account in the 51+ group
(sum(my_data$account_fin == "1" & my_data$female == "2" & my_data$age %in% c("51":"99"), na.rm = TRUE))/(sum(my_data$account_fin %in% c("0":"1") & my_data$female == "2" & my_data$age %in% c("51":"99"), na.rm = TRUE)) *100

# Percentage of men who have a mobile money account in the 51+ group
(sum(my_data$account_mob == "1" & my_data$female == "2" & my_data$age %in% c("51":"99"), na.rm = TRUE))/(sum(my_data$account_mob %in% c("0":"1") & my_data$female == "2" & my_data$age %in% c("51":"99"), na.rm = TRUE)) *100

heights <- c(43.18566, 25.11242, 50.70936, 30.74725, 62.38329, 25.53407, 71.86184, 33.52607, 72.98378, 12.77638, 79.14117, 17.5796)
par(mar=c(4,5.5,7,2)+.1)
barplot(matrix(heights, ncol = 6), beside = TRUE,col = c("pink","grey"), names.arg = c("Women", "Men", "Women", "Men", "Women", "Men"), ylab = "Percentage (%)", ylim = c(0,80), las = 1, space = c(0,0,0.8,0,0.8,0,0.8,0,0.8,0,0.8,0), lwd = 1)
legend(3, 1, inset=c(0.01,-0.27), legend=c("Has a financial instiution account", "Has a mobile money account"), fill = c("pink","grey"), x = "topleft")
title(xlab = "Ages 15-24            Ages 25-50               Ages 51+", line=2.5, cex.lab=1.1)
title(main = "Respondents with an account", line = 4.5)



# Create barplot for reason for no mobile money account 
my_data$fin13_1a <- as.character(my_data$fin13_1a)
my_data$fin13_1b <- as.character(my_data$fin13_1b)
my_data$fin13_1c <- as.character(my_data$fin13_1c)
my_data$fin13_1d <- as.character(my_data$fin13_1d)
my_data$fin13_1e <- as.character(my_data$fin13_1e)
my_data$fin13_1f <- as.character(my_data$fin13_1f)
                              
(sum(my_data$fin13_1a== "1", na.rm = TRUE))/(sum(my_data$fin13_1a %in% c("1":"4"), na.rm = TRUE)) *100
(sum(my_data$fin13_1b== "1", na.rm = TRUE))/(sum(my_data$fin13_1b %in% c("1":"4"), na.rm = TRUE)) *100
(sum(my_data$fin13_1c== "1", na.rm = TRUE))/(sum(my_data$fin13_1c %in% c("1":"4"), na.rm = TRUE)) *100
(sum(my_data$fin13_1d== "1", na.rm = TRUE))/(sum(my_data$fin13_1d %in% c("1":"4"), na.rm = TRUE)) *100
(sum(my_data$fin13_1e== "1", na.rm = TRUE))/(sum(my_data$fin13_1e %in% c("1":"4"), na.rm = TRUE)) *100
(sum(my_data$fin13_1f== "1", na.rm = TRUE))/(sum(my_data$fin13_1f %in% c("1":"4"), na.rm = TRUE)) *100

par(mar = c(9.6, 5, 4, 3.5) +.1, lwd = 1)
barplot(height = c(29.00614, 30.78123, 32.45879, 63.96177, 17.72652, 35.74564), names.arg = c('Too Far', 'Too expensive', 'Lack Documentation', 'Lack of Money', 'Use agent', 'No mobile phone'), las = 2, ylab = "Percentage", ylim = c(0,70), space = 0.1, cex.names = 0.9, main = "Why don't respondents have a mobile money account?", col = "pink")
title(xlab = "Reason for no mobile money account", line=8.4, cex.lab=1)

# Create income quartile barplot 
my_data$inc_q <- as.character(my_data$inc_q)

# Percentage of people in the poorest 20% who own a financial institution account 
(sum(my_data$inc_q == "1" & my_data$account_fin == "1", na.rm = TRUE))/(sum(my_data$inc_q == "1" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people in the poorest 20% who own a mobile money account 
(sum(my_data$inc_q == "1" & my_data$account_mob == "1", na.rm = TRUE))/(sum(my_data$inc_q == "1" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people in the second 20% who own a financial institution account 
(sum(my_data$inc_q == "2" & my_data$account_fin == "1", na.rm = TRUE))/(sum(my_data$inc_q == "2" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people in the second 20% who own a mobile money account 
(sum(my_data$inc_q == "2" & my_data$account_mob == "1", na.rm = TRUE))/(sum(my_data$inc_q == "2" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people in the middle 20% who own a financial institution account 
(sum(my_data$inc_q == "3" & my_data$account_fin == "1", na.rm = TRUE))/(sum(my_data$inc_q == "3" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people in the middle 20% who own a mobile money account 
(sum(my_data$inc_q == "3" & my_data$account_mob == "1", na.rm = TRUE))/(sum(my_data$inc_q == "3" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people in the fourth 20% who own a financial institution account 
(sum(my_data$inc_q == "4" & my_data$account_fin == "1", na.rm = TRUE))/(sum(my_data$inc_q == "4" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people in the fourth 20% who own a mobile money account 
(sum(my_data$inc_q == "4" & my_data$account_mob == "1", na.rm = TRUE))/(sum(my_data$inc_q == "4" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people in the richest 20% who own a financial institution account 
(sum(my_data$inc_q == "5" & my_data$account_fin == "1", na.rm = TRUE))/(sum(my_data$inc_q == "5" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people in the richest 20% who own a mobile money institution account 
(sum(my_data$inc_q == "5" & my_data$account_mob == "1", na.rm = TRUE))/(sum(my_data$inc_q == "5" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

heights2 <- c(55.11497,9.728943, 60.81592, 11.43489, 63.84593, 13.31228, 68.0075, 15.94451, 74.68399, 21.00479)
par(mar=c(4,5.5,5,3)+.1)
par(xpd = TRUE)
barplot(matrix(heights2, ncol = 5), beside = TRUE,col = c("pink","grey"), names.arg = c("Poorest 20%", "Second 20%", "Middle 20%", "Fourth 20%", "Richest 20%"), ylab = "Percentage (%)", ylim = c(0,80), las = 1, lwd = 1, space = c(0,0,0.8,0,0.8,0,0.8,0,0.8,0))
legend(inset=c(0.01,-0.06), legend=c("Has a financial instiution account", "Has a mobile money account"), fill = c("pink","grey"), x = "topleft", cex = 0.9, box.lty = 0)
title(main = "Account owners by income quintile ", line = 2.4)
title(xlab = "Income Quintile", line=2.5, cex.lab=1.1)

# Create education level barplot 
my_data$educ <- as.character(my_data$educ)

# Percentage of people who completed primary school or less who own a financial institution account 
(sum(my_data$educ == "1" & my_data$account_fin == "1", na.rm = TRUE))/(sum(my_data$educ == "1" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people who completed primary school or less who own a mobile money account 
(sum(my_data$educ == "1" & my_data$account_mob == "1", na.rm = TRUE))/(sum(my_data$educ == "1" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people who completed secondary school who own a financial institution account 
(sum(my_data$educ == "2" & my_data$account_fin == "1", na.rm = TRUE))/(sum(my_data$educ == "2" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people who completed secondary school who own a mobile money account 
(sum(my_data$educ == "2" & my_data$account_mob == "1", na.rm = TRUE))/(sum(my_data$educ == "2" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people who completed tertiary education or more who own a financial institution account 
(sum(my_data$educ == "3" & my_data$account_fin == "1", na.rm = TRUE))/(sum(my_data$educ == "3" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

# Percentage of people who completed tertiary education or more who own a mobile money account 
(sum(my_data$educ == "3" & my_data$account_mob == "1", na.rm = TRUE))/(sum(my_data$educ == "3" & my_data$account %in% c("0":"1"), na.rm = TRUE)) *100

heights3 <- c(37.4987,12.1644, 68.84297, 17.38374, 92.26581, 12.86439)
par(mar=c(4,8,5,4)+.2)
par(xpd = TRUE)
barplot(matrix(heights3, ncol = 3), beside = TRUE,col = c("pink","grey"), names.arg = c("Primary school or less", "Secondary school", "Tertiary education or more"), ylab = "Percentage (%)", ylim = c(0,100), las = 1, lwd = 1, space = c(0,0,0.4,0,0.4,0))
legend(inset=c(0.01,-0.06), legend=c("Has a financial instiution account", "Has a mobile money account"), fill = c("pink","grey"), x = "topleft", cex = 0.9, box.lty = 0)
title(main = "Account owners by level of education completed ", line = 2.4)
title(xlab = "Level of education completed", line=2.5, cex.lab=1.1)


# mobile account in relation to regions
library(scales)
library(tidyverse)
my_data <- my_data[my_data$account_mob >= 0, na.rm = TRUE]
keep_columns <- c("regionwb", "account_mob")
region_data <- my_data[keep_columns]
region_data <- region_data %>% filter(regionwb != "")
levels(factor(region_data$regionwb))

mynamestheme <- theme(
  plot.title = element_text(family = "Mono", face = "bold", size = (12)),
  legend.title = element_text(colour = "black", face = "bold", family = "Mono"),
  legend.text = element_text(colour = "steelblue4", family = "Mono"),
  axis.title = element_text(family = "Mono", size = (10), colour = "steelblue4"),
  axis.text = element_text(family = "Mono", colour = "black", size = (10))
)

ggp <- ggplot(region_data) + 
  mynamestheme +
  geom_bar(position = "fill") + 
  aes(x = regionwb, fill = factor(account_mob)) + 
  ggtitle("Proportion of mobile account ownership in different regions") +
  labs(x = "Region", y = "Proportion", fill = "Account") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  scale_x_discrete(labels = label_wrap(24))

ggp + coord_flip()


# Chi-squared test for region
table_1 <- table(regionwb, account_mob)
table_1 <- table_1[-1,]
table_1
test_1 <- chisq.test(table_1)
test_1$expected
test_1
test_1$p.value

# Chi-squared test for genders
table_2 <- table(female, account_mob)
table_2

test_2 <- chisq.test(table_2)
test_2$expected
test_2
test_2$p.value

# t-test for age
test_3 <- t.test(age ~ account_mob,
               data = my_data,
               var.equal = FALSE,
               alternative = "greater"
)
test_3
test_3$p.value

# Chi-squared test for education level
table_4 <- table(educ, account_mob)
table_4
test_4 <- chisq.test(table_4)
test_4$expected
test_4
test_4$p.value

# Chi-squared test for income level
table_5 <- table(inc_q, account_mob)
table_5
test_5 <- chisq.test(table_5)
test_5$expected
test_5
test_5$p.value
