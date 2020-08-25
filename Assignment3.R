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
legend("topleft", col = 1:2, legend = levels(sex), pch = "-")
plot(prop.black, age)

