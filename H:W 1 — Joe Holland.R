library(tidyverse)
install.packages("knitr")
install.packages("rmarkdown")
library(knitr)
library(rmarkdown)

x <- 1:50
w <- 1 + sqrt(x)/2
example1 <- data.frame(x=x, y= x + rnorm(x)*w)
attach(example1)

fm <- lm(y ~ x)
summary(fm)

lrf <- lowess(x, y) # Lowess uses weighted least squares
plot(x, y)
lines(x, lrf$y)
abline(0, 1, lty=3)
abline(coef(fm))
?lowess

load("~/Documents/CCNY Eco MA/B2000/Household_Pulse_data_w48.RData")

Household_Pulse_data[1:10, 1:7] # 10 rows, 7 columns
attach(Household_Pulse_data) # Allows reference to variables without referencing data each time
levels(SEXUAL_ORIENTATION)
as.factor(SEXUAL_ORIENTATION)
summary(Household_Pulse_data)
nrow(Household_Pulse_data)

summary(TBIRTH_YEAR[GENID_DESCRIBE == "female"]) #Square brackets act as filter
summary(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "transgender"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "other"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "NA"])
(mean(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
- mean(TBIRTH_YEAR[GENID_DESCRIBE == "male"]))
sd(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "female"])


