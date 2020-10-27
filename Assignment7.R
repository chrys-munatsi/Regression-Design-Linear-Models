library(MASS)

eagles <- eagles 

# Question 1
# The number combinations of the factor variables across the 3 
# variables are six
# There are 8 observations of 5 variables.

# Question 2
# Find the average success rate for immature (pirating) eagles.



# Question 3
# Which of the six groups (two pirate age groups, two pirate size groups, 2 
# victim size groups) has the lowest median success rate? 
Small victim
# I.e., what seems the single most important factor in determining success
# or failure in a pirating attempt? 
Size and age

# Question 4
# Which of the four categories of pirating eagle has lowest median success 
# rate?
Small Immature

# Question 5: 
# Attach a copy of your figure with the four boxplots (success rate per pirate 
# eagle category).
boxplot(y/n ~ A + P)

# Question 6:
# Based on the single explanatory variable models, which single variable is 
# the best predictor for success of a pirating attempt?
P Size of pirating eagle (L = large, S = small)

# Question 7: Assuming the same distribution for the response and the same
# link function, are the following two models nested? η i represents the linear predictor.
Yes

# Question 8:
# When are likelihood ratio tests in logistic regression appropriate?
It can only be used for nested models and is valid asymptotically  ie large ηi 
and large N ( number of observations) 

# Question 9:
# Fully specify the model that has been fitted in model A.

# Question 10: 
mA <- glm(cbind(y, n-y)  A + P + A*P + V, family = binomial)
mB <- glm(cbind(y, n-y)  A + P + V, family = binomial)
mC <- glm(cbind(y, n-y)  A + P + A*P, family = binomial)


# Look at output from model A.
# Give residual degrees of freedom, 
# number of parameters estimated
(excluding intercept), total number of observations.