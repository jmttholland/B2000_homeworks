library(tidyverse)
setwd("~/Documents/CCNY Eco MA/B2000/H:Ws/H:W 3")
load("~/Documents/CCNY Eco MA/B2000/acs2017_ny/acs2017_ny_data.RData")
options(scipen = 99999)
summary(acs2017_ny)
names(acs2017_ny)

# Creating working age men in NYC data set
working_age_men_in_nyc <- subset(acs2017_ny, (acs2017_ny$SEX == 'Male') & 
                            (acs2017_ny$in_NYC == 1) & 
                            (acs2017_ny$AGE >= 18)  & 
                            (acs2017_ny$AGE < 67))

# Creating factor of NYC boroughs
borough_factor <- factor((working_age_men_in_nyc$in_Bronx + 2*working_age_men_in_nyc$in_Manhattan
                     + 3*working_age_men_in_nyc$in_StatenI + 4*working_age_men_in_nyc$in_Brooklyn + 
                       5*working_age_men_in_nyc$in_Queens), levels = c(1,2,3,4,5), labels = 
                      c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
as.numeric(borough_factor)
data_with_borough_factor <- cbind(working_age_men_in_nyc, borough_factor)


# Creating function to normalize variables
norm_varb <- function (x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Cleaning up and normalizing data
str(working_age_men_in_nyc$OWNCOST)
is.na(working_age_men_in_nyc$OWNCOST) <- working_age_men_in_nyc$OWNCOST == 99999
housing_cost <- working_age_men_in_nyc$OWNCOST + working_age_men_in_nyc$RENT
housing_cost
housing_cost_num <- as.numeric(housing_cost)
data_with_bf_and_housing_costs <- cbind(data_with_borough_factor, housing_cost_num)
norm_income <- norm_varb(working_age_men_in_nyc$INCTOT)
norm_housing_cost <- norm_varb(housing_cost)
norm_income <- as.numeric(norm_income)
norm_housing_cost <- as.numeric(norm_housing_cost)

# Creating prelim KNN data
prelim_data <- data.frame(norm_income, norm_housing_cost)
good_data <- complete.cases(prelim_data, borough_factor)
length(good_data == 1) 
nrow(prelim_data)
dat_use <- subset(prelim_data, good_data) 
nrow(dat_use)
y_use <- subset(borough_factor, good_data)
length(borough_factor)
length(y_use)
nrow(as.data.frame(y_use))

# Splitting data for training
set.seed(12345)
NN_obs <- sum(good_data == 1)
select1 <- (runif(NN_obs) < 0.8)
NN_obs
sum(select1 == 1) / sum(select1 == 0) 
train_data <- subset(dat_use, select1)
test_data <- subset(dat_use, (!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
nrow(test_data) - length(true_data)

prop.table(summary(cl_data))
prop.table(summary(true_data))

require(class)

# Visual depiction of classifications
plot(norm_housing_cost, norm_income, col = borough_factor, pch = 10)
legend("topleft",
       legend = c(levels(borough_factor)),
       col = 1:5,
       pch = 10)
plot(norm_housing_cost, norm_income, col = borough_factor, xlim = c(0, 0.5), ylim = c(0, 0.5), pch = 10)
legend("topleft",
       legend = c(levels(borough_factor)),
       col = 1:5,
       pch = 10)

# Running KNN for norm_housing and norm_income
for (indx in seq(1, 9, by = 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

cl_data_n <- as.numeric(cl_data)
model_ols_1 <- lm(cl_data_n ~ train_data$norm_income + train_data$norm_housing_cost)
summary(model_ols_1)
y_hat <- fitted.values(model_ols_1)
(data.frame(y_hat, cl_data_n))
mean(y_hat[cl_data_n == 1])
mean(y_hat[cl_data_n == 2])
mean(y_hat[cl_data_n == 3])
mean(y_hat[cl_data_n == 4])
mean(y_hat[cl_data_n == 5])

# Regressing Bronx alone
cl_data_n1 <- as.numeric(cl_data_n == 1)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_income + train_data$norm_housing_cost)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])
plot(train_data$norm_housing_cost, cl_data_n1)
model_ols_v2 <- lm(cl_data_n1 ~ train_data$norm_housing_cost)
df5 <- data.frame(cl_data_n1, train_data$norm_housing_cost)
ggplot(data = df5, aes(x = train_data$norm_housing_cost, y = cl_data_n1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE )

# Logistic regression for Bronx
logistic_model_v1 <- glm(cl_data_n1 ~ train_data$norm_income + train_data$norm_housing_cost, family = binomial(link = logit))
ggplot(data = df5, aes(x = train_data$norm_housing_cost, y = cl_data_n1)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)
coefficients <- as.numeric(coefficients(logistic_model_v1))
odds_ratio <- exp(coefficients)
odds_ratio
odds_fitted <- fitted.values(logistic_model_v1)
df6 <- data.frame(odds_fitted, cl_data_n1)
max(df6$odds_fitted[cl_data_n1 == 1])
mean(odds_fitted[cl_data_n1 == 1])
mean(odds_fitted[cl_data_n1 == 0])

# Logistic model for Manhattan 
levels(borough_factor)
cl_data_n2 <- as.numeric(cl_data_n == 2)
logistic_model_manhattan <- glm(cl_data_n2 ~ train_data$norm_income + train_data$norm_housing_cost, family = binomial(link = logit))
df9 <- data.frame(train_data$norm_housing_cost, cl_data_n2)
ggplot(data = df9, aes(x = train_data.norm_housing_cost, y = cl_data_n2)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)
odds_manhattan_1 <- exp(coefficients(logistic_model_manhattan)) 
odds_fitted_manhattan <- fitted.values(logistic_model_manhattan)
mean(odds_fitted_manhattan[cl_data_n2 == 1])
mean(odds_fitted_manhattan[cl_data_n2 == 0])

# Creating test and train data for KNN for Rent and Income (without other housing costs)
norm_rent <- norm_varb(working_age_men_in_nyc$RENT)
norm_rent <- as.numeric(norm_rent)
prelim_data_2 <- data.frame(norm_rent, norm_income)
length(complete.cases(prelim_data_2)) - nrow(prelim_data_2)
select2 <- runif(nrow(prelim_data_2)) < .8
train_data_2 <- subset(prelim_data_2, select2)
test_data_2 <- subset(prelim_data_2, (!select2))
cl_data_2 <- borough_factor[select2]
true_data_2 <- borough_factor[(!select2)]
plot(norm_rent, norm_income, col = borough_factor)
legend("topleft",
       legend = c(levels(borough_factor)),
       col = 1:5,
       pch = 10)

prop.table(summary(cl_data_2))
prop.table(summary(true_data_2))

# Running KNN for rent and income

# for (i in seq(1, 9, by = 2)) {
#   pred_borough_2 <- knn(train = train_data_2, test = test_data_2, cl = cl_data_2, l = 0,
#                         k = i, prob = FALSE, use.all = TRUE)
#   correct_borough_2 <- sum(pred_borough_2 == true_data_2)
#   correct_rate_2 <- correct_borough_2 / length(true_data_2)
#   print(c(i, correct_rate_2))
# }

# It is not working — R is telling me there are too many ties. 
# I'm not sure how to solve this. Is it because norm_rent has lots of zeros?

# subtracting observations where norm_rent = 0 to create new data for rent and income KNN
prelim_data_2 <- cbind(prelim_data_2, borough_factor)
prelim_data_2 <- subset(prelim_data_2, prelim_data_2$norm_rent != 0)
borough_factor_2 <- prelim_data_2[,3]
length(borough_factor_2)
prelim_data_2 <- prelim_data_2[,1:2]
sum(complete.cases(prelim_data_2)) - nrow(prelim_data_2)
nrow(prelim_data_2) - length(borough_factor_2)
select_2 <- runif(nrow(prelim_data_2)) < .8
train_data_2 <- subset(prelim_data_2, select_2)
test_data_2 <- subset(prelim_data_2, (!select_2))
cl_data_2 <- subset(borough_factor_2, select_2)
true_data_2 <- subset(borough_factor_2, (!select_2))
length(cl_data) - nrow(train_data)
length(true_data_2) - nrow(test_data_2)
plot(prelim_data_2$norm_rent, prelim_data_2$norm_income, col = borough_factor_2)
legend("topleft",
       legend = c(levels(borough_factor_2)),
       col = 1:5,
       pch = 10)
prop.table(summary(cl_data_2))
prop.table(summary(true_data_2))

# Retrying KNN for income and rent but with occurances of rent = 0 removed
for (i in seq(1, 9, by = 2)) {
  pred_borough_2 <- knn(train = train_data_2, test = test_data_2, cl = cl_data_2, k = i, l = 0,
                        prob = FALSE, use.all = TRUE)
  correct_borough_2 <- sum(pred_borough_2 == true_data_2)
  correct_rate_2 <- correct_borough_2 / length(true_data_2)
  print(c(i, correct_rate_2))
}
# It worked this time, so it must have failed previously because 
# there were too many 0s in the rent column
# Still only predicting just above a third of boroughs correctly. 

# Creating test and train data for KNN for EDUC and norm_income
prelim_data_3 <- data.frame(educ = as.numeric(working_age_men_in_nyc$EDUC), norm_income)
str(prelim_data_3$educ)
length(complete.cases(prelim_data_3)) - nrow(prelim_data_3)
nrow(prelim_data_3) - length(borough_factor)
select3 <- runif(nrow(prelim_data_3)) < .8
train_data_3 <- subset(prelim_data_3, select3)
test_data_3 <- subset(prelim_data_3, (!select3))
cl_data_3 <- subset(borough_factor, select3)
true_data_3 <- subset(borough_factor, (!select3))
plot(prelim_data_3$educ, norm_income, col = borough_factor)
legend("topleft",
       legend = c(levels(borough_factor)),
       col = 1:5,
       pch = 10)

# Running KNN for EDUC and norm_income
for (i in seq(1, 9, by = 2)) {
  pred_borough_3 <- knn(train = train_data_3, test = test_data_3, cl = cl_data_3, k = i, l = 0,
                        prob = FALSE, use.all = TRUE)
  correct_borough_3 <- sum(pred_borough_3 == true_data_3)
  correct_rate_3 <- correct_borough_3 / length(true_data_3)
  print(c(i, correct_rate_3))
}
# No better success rate than income and housing cost KNN
df <- data.frame(pred_borough_3, true_data_3)
df2 <- data.frame(prop.table(summary(pred_borough)), 
                             prop.table(summary(pred_borough_2)),
                             prop.table(summary(pred_borough_3)),
                             prop.table(summary(true_data)))
str(df2)
average_estimated_props <- numeric()
for (row in 1:nrow(df2)) {
  average <- sum(df2[row, 1:3])/3
  average_estimated_props[row] <- average
}
df3 <- data.frame(average_estimated_props, prop.table(summary(true_data)), average_estimated_props - prop.table(summary(true_data)),
                  (average_estimated_props - prop.table(summary(true_data)))/prop.table(summary(true_data)))
df3[,4]
# KNN seems to be consistently low balling the Bronx and Staten Island
# Also seems to be low balling Manhattan, but to a lesser extent
# The algorithm is choosing Brooklyn far too much
# And is quite good with Queens. 

# Fourth column of df3 gives us the difference between the average estimated proportions and the true proportions as a proportion of
# the original borough proportion. So it tells us how far off, on average after 3 different KNN tests, the estimated proportions are
# from the true borough proportions. We can see that Brooklyn is being over represented by 43%. Bronx, Manhattan,
# and Staten Island are being underrepresented by 43%, 23% and 89% respectively. Queens is very well estimated by KNN
# with less than a 1% over-estimation of the borough's proportion.
# Not sure if any of what I've done here is practically valid, but I just wanted to check estimated proportions against actual proportions
# to have an idea of which boroughs the algorithm is getting wrong.
      
# Trying to find a way to compare the means of all the variables so I can find a variable that has very different means per Borough to use for KNN
# working_age_men_in_nyc <- cbind(working_age_men_in_nyc, borough_factor)
# df_means <- data.frame(x = c(colnames(working_age_men_in_nyc)))
# for (mean in c(bronx_means, brooklyn_means, manhattan_means, staten_island_means, queens_means)) {
#   for (col in 1:ncol(working_age_men_in_nyc)) {
#     bronx_means <- mean(subset(working_age_men_in_nyc, working_age_men_in_nyc$borough_factor == "Bronx")[,col], na.rm = TRUE)
#     brooklyn_means <- mean(subset(working_age_men_in_nyc, working_age_men_in_nyc$borough_factor == "Brooklyn")[,col], na.rm = TRUE)
#     manhattan_means <- mean(subset(working_age_men_in_nyc, working_age_men_in_nyc$borough_factor == "Manhattan")[,col], na.rm = TRUE)
#     staten_island_means <- mean(subset(working_age_men_in_nyc, working_age_men_in_nyc$borough_factor == "Staten Island")[,col], na.rm = TRUE)
#     queens_means <- mean(subset(working_age_men_in_nyc, working_age_men_in_nyc$borough_factor == "Queens")[,col], na.rm = TRUE)
#     means_hold <- data.frame()
#   }
#   df_means[,ncol(df_means) + 1] <- mean
# }  
# warnings()
# str(working_age_men_in_nyc)
# 
# storage <- numeric(ncol(working_age_men_in_nyc))

# Plotting different types of income to see which has the largest variance accross boroughs, 
# to use that income measure as a KNN predictor to see if that gets me a higher success rate
ggplot(data = working_age_men_in_nyc, aes(x = borough_factor)) +
  geom_line(aes(y = as.numeric(INCTOT)), color = "red", size = 2) +
  geom_line(aes(y = as.numeric(INCWAGE)), color = "black", size = 2) +
  geom_line(aes(y = as.numeric(HHINCOME)), colour = "purple",size = .5)
# Looks like there is the most difference in HHincome

# KNN for HHIncome and EDUC
norm_hhincome <- norm_varb(working_age_men_in_nyc$HHINCOME)
prelim_data_4 <- data.frame(educ = as.numeric(working_age_men_in_nyc$EDUC), 
                            norm_hhincome = as.numeric(norm_hhincome))
sum(is.na(norm_hhincome))
sum(complete.cases(prelim_data_4)) - nrow(prelim_data_4)
good_cut <- complete.cases(prelim_data_4)
useable_data_4 <- subset(prelim_data_4, good_cut)
borough_factor_4 <- subset(borough_factor, good_cut)
select_4 <- runif(nrow(useable_data_4)) < .8
train_data_4 <- subset(useable_data_4, select_4)
test_data_4 <- subset(useable_data_4, !select_4)
cl_data_4 <- subset(borough_factor_4, select_4)
true_data_4 <- subset(borough_factor_4, !select_4)

for (i in seq(1, 15, by = 2)) {
  pred_borough_4 <- knn(train = train_data_4, test = test_data_4, cl = cl_data_4, k = i, l = 0, prob = FALSE,
      use.all = TRUE)
  correct_borough_4 <- sum(pred_borough_4 == true_data_4)
  correct_rate_4 <- correct_borough_4 / length(true_data)
  print(c(i, correct_rate_4))
}
# Getting a much better success rate, of about 85%
plot(x = train_data_4$norm_hhincome, y = train_data_4$educ, col = cl_data_4)
legend("topleft",
       legend = levels(cl_data_4),
       col = 1:5,
       pch = 10)

# Running KNNs with EDUC and norm_income and then norm_housing price, to see if EDUC is 
# the predictor that is working the best.

df4 <-  data.frame(pred_borough_4, true_data_4) 
df5 <- data.frame(prop.table(summary(pred_borough_4)), prop.table(summary(true_data_4)),
                  prop.table(summary(pred_borough_4)) - prop.table(summary(true_data_4)), 
                  (prop.table(summary(pred_borough_4)) - prop.table(summary(true_data_4)))/prop.table(summary(true_data_4)))
df2 <- cbind(df2, prop.table(summary(pred_borough_4)))

# Regression for HHIncome and EDUC for Manhattan
cl_data_4_n <- as.numeric(cl_data_4)
cl_data_4_n2 <- as.numeric(cl_data_4_n == 2)
logistic_model_HHincome_educ_manhattan <- glm(cl_data_4_n2 ~ train_data_4$educ + train_data_4$norm_hhincome, family = binomial(link = logit))
summary(logistic_model_HHincome_educ_manhattan) # Positive coefficients on EDUC and norm_hh_income tells us that increasing education and household income increases
# the odds of being in Manhattan, which fits with prior assumptions.
coefficients_2 <- coefficients(logistic_model_HHincome_educ_manhattan)
odds_ratio_2 <- exp(coefficients_2)
odds_to_prob <- function (x, beta_1, beta_2, beta_3) {
  p_hat <- exp(beta_1 + x*beta_2 + x*beta_3)/(exp(beta_1 + x*beta_2 + x*beta_3) + 1)
  return(p_hat)
}
(probability_of_manhattan <- odds_to_prob(x = 0, beta_1 = coefficients_2[1], beta_2 = coefficients_2[2], beta_3 = coefficients_2[3]))
for (norm_inc in seq(0, 1, by = 0.1)) {
  probability <- odds_to_prob(x = norm_inc, beta_1 = coefficients_2[1], beta_2 = coefficients_2[2], beta_3 = coefficients_2[3])
  print(c(norm_inc, probability))
}
train_hh_income <- working_age_men_in_nyc$HHINCOME
df10 <- data.frame(train_data_4$norm_hhincome, cl_data_4_n2)
ggplot(data = df10, aes(x =  train_data_4.norm_hhincome, y  =cl_data_4_n2)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)

# KNN doing slightly better for Brooklyn with HHincome than 37% overestimate.

# KKN for HHincome and EDUC but for Manhattan only
cl_data_4_manhattan <- 
cl_data_4_manhattan <- subset(cl_data_4_n2, select_4)
true_data_4_manhattan <- subset(cl_data_4_n2, !select_4)
for (i in seq(1, 9, by = 2)) {
  pred_borough_manhattan <- knn(train = train_data_4, test = test_data_4, cl = cl_data_4_manhattan,
                                k = i, l = 0, prob = FALSE, use.all = TRUE)
  correct_manhattan <- sum(pred_borough_manhattan == true_data_4_manhattan)
  correct_rate_manhattan <- correct_manhattan / length(true_data_4_manhattan)
  print(c(i, correct_rate_manhattan))
}
                  