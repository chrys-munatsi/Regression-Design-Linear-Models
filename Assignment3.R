setwd("~/r_practice/assignment-three")

# a) 
# Read the lion data into R

lions <- read.table("lions2.txt")
attach(lions)

str(lions)
dim(lions)
names(lions)
head(lions)


# FIRST STEPS: EXPLORATORY DATA ANALYSIS
# b) 
library(visreg)

# side-by-side boxplots of proportion black vs sex
boxplot(prop.black ~ sex, data = lions)
# side-by-side boxplots of age vs sex
boxplot(age ~ sex, data = lions)
# histogram of age
hist(age)
# proportion black 
hist(prop.black)
# scatterplots of proportion black vs age
plot(prop.black, age)


# c) 
# Use a t-test to compare proportion black between males and females.
t.test(prop.black ~ sex, var.equal = TRUE)

# Question 1: What is the baseline or reference category of the sex variable? 
# Use the exact same name as used in the original data. ANSWER IS female

# Question 2: Give an estimate (with 95% confidence limits) for the difference in 
# average proportion black between males and females. This value should be negative 
# if males have blacker noses, positive if females have blacker noses
t.test(prop.black ~ sex, var.equal = F)
0.5262500 - 0.3203125 

# Question 3: In words, give a brief interpretation of the above confidence interval.

# Question 4: Considering the t-test and the plots above, do these results mean 
# that, everything else being equal, female lions have on average blacker noses
# than male lions?
# POSSIBLY YES


# d) 
# Rephrase the above t-test as a linear model: fit a linear model (model A) with 
# proportion black as the response variable and sex as the only explanatory variable.
modelA <- lm(prop.black ~ sex, data = lions)

# Question 5: What is the null hypothesis tested on the sexmale line (in the R 
# output from the model summary)?
summary(modelA)
# The main null hypothesis of a multiple regression is that there is no relationship
# between the X variables and the Y variable; in other words, the Y values you 
# predict from your multiple regression equation are no closer to the actual Y 
# values than you would expect by chance.


# e) 
# Plot proportion black against age, using different colours for the different sexes.
# You can add a legend to make clearer which colour refers to which sex:
plot(lions[lions$sex=="female",]$prop.black, lions[lions$sex=="female",]$age, col="blue", pch=16, xlab = "Age", ylab = "prop.black", xlim=c(0,1.2), ylim=c(0,16))
par(new=TRUE)
plot(lions[lions$sex=="male",]$prop.black, lions[lions$sex=="male",]$age, col="red", pch=16, xlab = "Age", ylab = "prop.black", xlim=c(0,1.2), ylim=c(0,16))
legend("topleft", legend=c('Female', 'Male'), col=c('blue', 'red'), pch=c(16,16))


# f)
# f) Fit the following 3 models, each time with proportion black as the response 
# variable and the following explanatory variables:
# model B: age as the only explanatory variable.
# model C: age and sex as explanatory variables
# model D: age, sex and interaction between age and sex as explanatory variables

modelB <- lm(prop.black ~ age, data = lions)
modelC <- lm(prop.black ~ age + sex, data = lions)
modelD <- lm(prop.black ~ age + sex + age*sex, data = lions)

# Question 7: What does an interaction between sex and age imply?


# Question 8: Interpret the regression coefficient estimates in model D.


# Question 9: What is the value of the maximised log likelihood for Model D (with
# the interaction term)? How many parameters were estimated for this model?


# h) 
# Consider a model set consisting of the following four models (three of which 
# you have already fitted), each time with proportion black as the response variable:

mA <- lm(prop.black ~ sex, data=lions)
mB <- lm(prop.black ~ age, data=lions) 
mC <- lm(prop.black ~ age + sex, data = lions)
mD <- lm(prop.black ~ age + sex + age*sex, data = lions)
mE <- lm(prop.black ~ age + sex + park + age*sex, data = lions)


# Question 10: What is the Akaike weight of the best model?
aics <- AIC(mA, mB, mC, mD, mE)
delta.aics <- aics$AIC - min(aics$AIC)
cbind(model=c("mA", "mB", "mC", "mD","mE"), delta.aics)
wi <- exp(-0.5*delta.aics)/sum(exp(-0.5*delta.aics))
cbind(model=c("mA", "mB","mC","mD", "mE"),wi)

# Question 11: Does the result in Question 10 above depend on which other models
# are in the set? Yes or NO
  

# Question 12: What is wrong with using linear regression to model proportion of 
# nose blackness?


# i) 
# Actually, we wanted to be able to predict age from proportion blackness of the nose. Fitting a
# linear regression model with age as the response variable is probably not quite the right thing to do,
# because for these lions we knew the age exactly and measured the proportion black on their nose.
# Usually in linear regression models we assume that the values of the explanatory variables are known
# exactly, and all the uncertainty is in the response. But let’s just ignore this for the moment, and fit the following models.
# model 1: A i = B i + e i
# model 2: A i = B i + S i + P i + e i
# model 3: A i = B i + S i ∗ P i + e i
# where A i , B i , S i , P i are the age, proportion black, sex and park for lion i, respectively, and e i ∼ N (0, σ 2 ).
m1 <- lm(age ~ prop.black, data = lions)
m2 <- lm(age ~ prop.black + sex + park, data = lions)
m3 <- lm(age ~ prop.black + sex*park, data = lions)


# Question 13: In model 3 there are 4 categories (2 sex times 2 parks, the interaction between park
#and sex means that the effect of sex depends on park), and one continuous explanatory variable.
#This will result in four estimated regression lines, one for each of the 4 categories. Find the following estimates:

# i. Slope of line for males in Serengeti.
-2.8434

# ii. Slope of line for females in Ngorongoro.


# iii. Intercept for males in Serengeti.
0.7528

# iv. Intercept for females in Serengeti.


# Question 14: Give a 95% prediction interval for the age of a lion with a completely black nose
# (proportion = 1), using model 1.


# Question 15: In your own words, interpret the above interval estimate.
