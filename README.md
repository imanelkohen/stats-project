# stats-project
EC124 Data set


# Import dataset and install necessary packages
install.packages("haven")
library(haven)
my_data <- read_dta("micro_world_139countries.dta")
attach(micro_world_139countries)
install.packages("dplyr")
library(dplyr)


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
groups <- rep(1:6, each = 2)
bars <- rep(c("Bar 1", "Bar 2"), times = 6)
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

par(mar = c(9.6, 5, 4, 4.1) +.1, lwd = 1)
barplot(height = c(29.00614, 30.78123, 32.45879, 63.96177, 17.72652, 35.74564), names.arg = c('Too Far', 'Too expensive', 'Lack Documentation', 'Lack of Money', 'Use agent', 'No mobile phone'), las = 2, ylab = "Percentage", ylim = c(0,70), space = 0.1, cex.names = 0.9, main = "Why don't respondents have a mobile money account?", col = "pink")
title(xlab = "Reason for no mobile money account", line=8.4, cex.lab=1)

# Gender in relation to regions
library(scales)
keep_columns <- c("regionwb", "female")
gender_data <- my_data[keep_columns]
gender_data <- gender_data %>% filter(regionwb != "")
levels(factor(gender_data$regionwb))

mynamestheme <- theme(
  plot.title = element_text(family = "Helvetica", face = "bold", size = (15)),
  legend.title = element_text(colour = "black", face = "bold", family = "Helvetica"),
  legend.text = element_text(colour = "steelblue4", family = "Helvetica"),
  axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
  axis.text = element_text(family = "Helvetica", colour = "black", size = (10))
)

ggp <- ggplot(gender_data) + 
  mynamestheme +
  geom_bar(position = "fill") + 
  aes(x = regionwb, fill = factor(female)) + 
  ggtitle("Proportion of genders in different regions") +
  labs(x = "Region", y = "Proportion", fill = "Gender") +
  scale_fill_discrete(labels = c("Female", "Male")) +
  scale_x_discrete(labels = label_wrap(24))

ggp + coord_flip()


# Keep age and region
my_data_2 <- data.frame(my_data$regionwb, my_data$age)
my_data_2$my_data.age <- as.numeric(as.character(my_data_2$my_data.age))
my_data_2 <- my_data_2[my_data_2$my_data.age > 14,]
my_data_2["age_group"] = cut(my_data_2$my_data.age, c(15, 24, 50, Inf), c("15-24", "25-50", ">50"), include.lowest=TRUE)
view(my_data_2)


# Age groups in relation to region
keep_columns <- c("my_data.regionwb", "age_group")
age_data <- my_data_2[keep_columns]
age_data <- age_data %>% filter(my_data.regionwb != "")
levels(factor(age_data$my_data.regionwb))

mynamestheme <- theme(
  plot.title = element_text(family = "Helvetica", face = "bold", size = (15)),
  legend.title = element_text(colour = "black", face = "bold", family = "Helvetica"),
  legend.text = element_text(colour = "steelblue4", family = "Helvetica"),
  axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
  axis.text = element_text(family = "Helvetica", colour = "black", size = (10))
)

ggp <- ggplot(age_data) + 
  mynamestheme +
  geom_bar(position = "fill") + 
  aes(x = my_data.regionwb, fill = factor(age_group)) + 
  ggtitle("Proportion of age groups in different regions") +
  labs(x = "Region", y = "Proportion", fill = "Age") +
  scale_fill_discrete(labels = c("Age 15-24", "Age 25-50", "Age 51+")) +
  scale_x_discrete(labels = label_wrap(24))

ggp + coord_flip()

# Chisquare test for region
table <- table(regionwb, account_mob)
table <- table[-1,]
table

test_2 <- chisq.test(table)
test_2$expected
test_2

# Chi-squared test for education level
table <- table(educ, account_mob)
table

test_2 <- chisq.test(table)
test_2$expected
test_2

# Chi-squared test for income level
table <- table(inc_q, account_mob)
table

test_2 <- chisq.test(table)
test_2$expected
test_2
