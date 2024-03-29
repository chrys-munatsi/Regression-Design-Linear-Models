library(MASS)

eagles <- eagles 

# Question 1
# The number combinations of the factor variables across the 3 
# variables are six
# There are 8 observations of 5 variables.

# Question 2
# Find the average success rate for immature (pirating) eagles.
((17/27)+(1)+(0/28)+(1/4))/4
[1] 0.4699074


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
boxplot(y/n ~ A + P, xlab = "Eagle category", ylab = "Success rate", las = 1, cex.lab = 1.5, cex.axis = 1.5)
stripchart(y/n ~ A + P, add=T,vertical=TRUE, method="jitter", jitter=.1)


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
Yi ~ Bin(1, pi)

logit(pi) = β0 + β1 Ai +  β2Pi  + β3AiPi  + β4Vi

where Yi is success (1) or failure (0) of pirating eagles, all ni = 1, and pi denotes the probability of success in pixel i


# Question 10: 
mA <- glm(cbind(y, n-y) ~ A + P + A*P + V, family = binomial)
mB <- glm(cbind(y, n-y) ~ A + P + V, family = binomial)
mC <- glm(cbind(y, n-y) ~ A + P + A*P, family = binomial)


# Look at output from model A.
# Give residual degrees of freedom, = 3
# number of parameters estimated (excluding intercept), = 4
# total number of observations. 
 length(residuals(mA)) = 8 
https://stackoverflow.com/questions/47695924/multinom-how-to-get-the-number-of-observations

# Question 11: 
# In the output from model A, which group is represented by the intercept?
Adult pirating eagle


# Question 12: 
# In words, what is the null hypothesis tested in the intercept line in the output from model A?
That the log-odds for success for the intercept is equal to 0

# Question 13: 
# In the output from model A, there is evidence for an interaction between size and age. Is 
# the #reason and interpretation for this interaction the relationship between age and size,
# immature eagles being smaller than adults?
Yes

# Question 14: 
# Interpret the coefficient on the PS line in the output from model B on both the log-odds and
# the odds scale.
On average, log(p/1-p) decreases by 3.5 units if the reference category increases by 1 unit.
A unit increase in PS is associated with the odds of the reference category 
(p/1-p) changing by a factor of exp(-3.4605)  = 0.03 being multiplied by 0.03 
ie a reduction of 97%

# Question 15: 
# State the null hypothesis (in words) that is being tested on the PS line in the output from
# model B.
That there is no difference in the log-odds for success between PS and the reference category

# Question 16: How many parameters are estimated in model C? What is its AIC value? 
# What is the maximised log-likelihood of this model?
4 parameter
AIC: 74.478
logLik(mC) = -33.23876 (df=4)


# Question 17: Look at the outputs from models A and B. What would you reply to a 
# claim that age of pirate eagle has no measurable effect on the probability of success? Justify your answer.
I would agree with the claim because large eagles, regardless of age, were usually successful
in pirating from feeding eagles. The small difference between the AIC values from models 
A and B show that there is little to no measurable effect of the age of pirate eagles.  
The p values for age in both models A and B were not statistically significant.


# Question 18: 
# Which of the three models (A, B, C) has most support from the data?
Model A 

# Question 19: Using the output from model A, estimate the probability of success for a large adult pirating
# eagle attacking a large victim.
0.71

# Question 20:
# Roughly summarize the results from model A. You don’t need to give estimates or p-values.
# Just roughly explain how each factor affects the probability of success.
Large eagles, regardless of age, were usually successful in pirating from feeding eagles.
Body size of the attacking eagle was more important than age in explaining the success 
of pirating the attacking bird. Small pirating eagles were generally unsuccessful 
unless they were adults attempting to steal from other small eagle..
