setwd("~/r_practice/assignment-two")
library(visreg)
library(car)
# a) Read the sleep data into R (‘animalsleep.txt’, Vula > Resources > Data and R code)
sleep <- read.csv("animalsleep.txt")
str(sleep)
head(sleep)
attach(sleep)


# b) Produce a scatterplot matrix of the relevant variables.
pairs(sleep[,2:11], pch = 19)


# c)


# Question 1: Find the correlation between total time spent sleeping and brain weight, 
# and the corresponding p-value.
# This gives only the correlation which is -0.358102
a <- cor(TotalSleep, BrainWt, use = "pairwise.complete.obs") 
# This tell us the p value which is 0.00578
b <- cor.test(TotalSleep, BrainWt, use = "pairwise.complete.obs")

# Question 2: Interpret the output from the correlation test above
# Relatively weak negative correlation

# Question 3: Find the minimum, median and maximum total sleep time.
summary(TotalSleep)
#min is  2.60, median 10.45, max is 19.90

# Question 4: Which animal in this data set has the shortest average total sleep time.
which.min(TotalSleep)
# This gives row 53 so one would assume it is Roedeer with 2.6

# Question 5: Which animal in this data set sleeps the most?
which.max(TotalSleep)
# This gives row 33 so it is Littlebrownbat with 19.9 


# d) Produce a histogram of total time spent sleeping.
hist(TotalSleep)
# and it is a approximately symmetrical like a normal distribution


# e) Create the following two plots next to each other: a scatter plot of total
# sleep time against brain weight, and secondly, a plot of total sleep time 
# against the log of brain weight.
cex.lab <- 2; cex.axis <- 1.5
logBrainWt <- log(BrainWt)
two.plot <- lm(TotalSleep ~ BrainWt + logBrainWt, data=sleep)
visreg(two.plot, "BrainWt", line=list(col="blue"), points=list(cex=1, pch=16, col=1), ylab="", xlab="BrainWt", cex.lab=cex.lab, cex.axis=cex.axis)
mtext("TotalSleep", side=2, line=3.5, cex=cex.lab)
visreg(two.plot, "logBrainWt", line=list(col="red"), points=list(cex=1, pch=16, col=1), xlab="logBrainWt", yaxt="n", ylab="", cex.lab=cex.lab, cex.axis=cex.axis)
mtext("TotalSleep", side=2, line=3.5, cex=cex.lab)
# The relationship between sleep and brain wt has the majority of values in a straight line
# and for the log values its a negative linear relationship.


# f)


# Use R to fit the following three linear regression models, each with total sleep 
# time as the response.
# Model 1: only one explanatory variable: log of brain weight.
# Model 2: two explanatory variables: log of brain weight, log of body weight.
# Model 3: two explanatory variables: brain weight and body weight, untransformed = natural scale.
m1 <- lm(TotalSleep ~ logBrainWt, data = sleep)
logBodyWt <- log(BodyWt)
m2 <- lm(TotalSleep ~ logBrainWt + logBodyWt, data = sleep)
m3 <- lm(TotalSleep ~ BrainWt + BodyWt, data = sleep)

# Question 8: Compare the effects of (log of) brain weight on total sleep in models 1 and 2.
summary(m1) 
summary(m2)
# appears to be relatively the same with minor discrepancy of 0.1 but p values differ largely

# Question 9: Find the variance inflation factor for brain weight in model 2.
# library(car) is necessary for vif() function
vif(m2)
#anyway the answer is 12.2363



# g) 

# Look at the residual plots from models 2 and 3.
# Residual plot: residuals vs fitted
cex.lab <- 1.8; cex.axis <- 1.5; pch <- 16; lwd <- 2
plot(fitted(m2), rstandard(m2), ylab="Standardised residuals of m2", xlab="Fitted values", cex.lab=cex.lab, cex.axis=cex.axis, pch=pch)
abline(h=0, col=2, lwd=lwd)
plot(fitted(m3), rstandard(m3), ylab="Standardised residuals of m3", xlab="Fitted values", cex.lab=cex.lab, cex.axis=cex.axis, pch=pch)
abline(h=0, col=2, lwd=lwd)

# Question 10: 
# Name the two animals that produce the two influential points in model 3.
plot(m3)
# so from the plot Cook's Distance we can see it's at point 1 and 5
# African elephant
# Asian elephant


# Question 11: For their weight and brain size, do the above two animals have shorter 
# or longer sleep times than predicted by model 3?
# The internet says shorter so I will say short
summary(TotalSleep)
# so if you look at the elephant row you'll see sleep is 3


# h) Briefly check that you agree that models 2 and 3 are no good, the one because of 
# collinearity, the other because it misspecifies the relationship between the 
# response and the explanatory variables.


# i) Fit another 2 linear regression models: m4 with total sleep time as the response,
# and gestation, danger, life span and log of brain weight as the explanatory variables; 
# m4.log with the same explanatory variables but log of total sleep time as the response.
m4 <- lm(TotalSleep ~ Gestation + Danger + LifeSpan + logBrainWt, data = sleep)
logTotalSleep <- log(TotalSleep)
m4.log <- lm(logTotalSleep ~ Gestation + Danger + LifeSpan + logBrainWt, data = sleep)

# Question 12: 
# Interpret the effect of danger on total sleep time using the output from model 4.
summary(m4)
m4$coefficients
# Under the model,  average total sleep time is expected to decrease by 1.61 units
# for each unit increase in danger, all else being equal. There is strong evidence 
# against the null hypothesis of no effect of danger on total sleep time.

# Question 13: 
# No information on total sleep time is available for giraffes. Predict total
# sleep time,in hours, using model 4. Use the covariate values given in the data.
# Gestation = 400
# Danger = 5
# LifeSpan = 28
# logBrainwt = log(680)
years <- ( 17.564674 +(-400 * 0.007052324) - (5 * 1.607919733) + (28 * 0.003174871) - (2.8325089127 * 0.646090884) )

# Question 14: 
# Give a 95% prediction interval for total sleep time in giraffes  (using model m4).
newdata <- data.frame(Gestation = 400, Danger = 5, LifeSpan = 28, logBrainWt = log(680), newdata = newdata)
pred.sleep <- predict(m4, newdata, interval="predict")
#  fit       lwr      upr
# 2.579178 -3.518959 8.677314

# Question 15: In your own words, give an interpretation of the above prediction interval.
# The interval will always contain the estimated total sleep time value but not
# always the true sleep time value. The width of the interval (12.2 hours) tells 
# us a bit about the uncertainty in the total sleep time estimate which is a bit
# big. However, it will be different every time we collect a new sample of data. 
# The negative value of the lower limit we can disregard because it doesn't make 
#sense as giraffes cannot sleep less than 0 hours.

# Question 16: Repeat the above prediction using model m4.log. Look at the residual 
# plots of both models. Use this information to compare models m4 and m4.log.
newdata <- data.frame(Gestation = 400, Danger = 5, LifeSpan = 28, logBrainWt = log(680), newdata = newdata)
pred.sleep.m4.log <- predict(m4.log, newdata, interval="predict")
# fit       lwr      upr
# 1.255543 0.6167954 1.894291
plot(m4)
plot(m4.log)

# Repeating the above prediction with m4.log the width interval is much smaller
# compared to the prediction with m4. In m4.log, the lower limit is 0.62 hours 
# and the upper limit is 1.89 hours. Thus, m4.log appears to be more precise in 
# predicting the total sleep time for giraffes. Comparing the residual plots of 
# m4 and m4.log, there is some evidence that m4.log gives a better indication of
# a linear model compared to m4. This is due to the variation of the residuals
# being greater in m4.log compared to m4. And both plots stay within 2 standard errors