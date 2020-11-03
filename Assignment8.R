#################### EXPLORATORY DATA ANALYSIS ############################

setwd("~/r_practice/assignment-eight")
fish <- read.csv("maxn.csv")
head(fish); tail(fish)
names(fish)
Ptero <- fish$X.4
str(Ptero)
Ptero <- as.numeric(Ptero)


# Question 1: 
# Create a histogram of the (maximum) number of fish observed
# per time window. Comment on the shape of this histogram.
hist(Ptero)
# right -skewed

# Question 2:
# Create a box plot of the (maximum) number of fish counted
# per time window and comment on the distribution.
boxplot(Ptero, xlab = "Max no. of fish", ylab = "Time window", las = 1, cex.lab = 1.5, cex.axis = 1.5)
stripchart(Ptero, add=T,vertical=TRUE, method="jitter", jitter=.1)
# more fish at smaller time window

# Question 3: 
# Would you expect a symmetric distribution for number of fish?
# I would not as counts asymmetrical distributed around the mean are a 
# common feature of count data 

# Question 4:
# Create a scatter plot of the number of fish against depth, and of the natural
# logarithm of the number of fish against depth.
depth <- as.numeric(fish$X.8) 
Ptero1 <- Ptero + 0.5
habitat <- fish$X.11

par(mfrow = c(1,2))
plot(Ptero1 ~ depth, col = as.numeric(habitat1), ylab = "Number of Panga fish",
     xlab = "Depth (m)", pch = as.numeric(habitat1), ylim = c(0,30))
points(Ptero1[1] ~ depth[1], col = "blue", pch = 20)

plot(log(Ptero1) ~ depth, col = as.numeric(habitat1), ylab = "log(Number of Panga fish)",
     xlab = "Depth (m)", pch = as.numeric(habitat1), ylim = c(0, 3.5))
points(log(Ptero1[1]) ~ depth[1], col = "blue", pch = 20)




##The one that put letters instead of numbers
plot(Ptero ~ depth, col = "orange", ylab = "Number of fish",
       xlab = "Depth (m)", pch = habitat)
points(Ptero[1] ~ depth[1], col = "red", pch = 20)

# Question 5: 
# Comment on the relationship between the count and depth. Do you think a 
# quadratic term for depth might be required in the model? Give a reason for your answer



# Question 6: 
# Find the median number of fish counted in each of the three habitats.
habitat1  <- as.factor(habitat)
levels(habitat1)
utapply(Ptero, habitat1, median)

# Question 7: 
# Fit the null model, i.e. a model with only the intercept.
# Interpret the intercept coefficient estimate.
nullM <- glm(Ptero ~ 1, family = poisson)
summary(nullM)
# The intercept estimates the log average rate of the number of Panga fish.
# The expected number of sightings during a specified time window is 
# exp(1.99454) = 7.35.

# Question 8:
# In statistical notation, fully specify/define the model fitted in m4.

# Fit the following models. Assume an appropriate distribution for the response
Prof_Cat <- fish$X.12
m1 <- glm(Ptero ~ habitat, family = poisson)
m2 <- glm(Ptero ~ Prof_Cat, family = poisson)
m3 <- glm(Ptero ~ habitat + depth, family = poisson)
m4 <- glm(Ptero ~ habitat + depth + Prof_Cat, family = poisson)
m5 <- glm(Ptero ~ habitat + depth + I(depth^2) + Prof_Cat, family = poisson)
m6 <- glm(Ptero ~ depth + I(depth^2) + Prof_Cat, family = poisson)


# Question 9: 
# Find the log-likelihood of model m4.
logLik(m4) 
# -125.8434 (df=5)


# Question 10: 
# What does the intercept in model m4 estimate?
summary(m4)
# The intercept estimates the log average rate of the number of Panga fish in the 
# Kelp habitat. The expected number of sightings during a specified time window is 
# exp(-1.059757) = 0.35


# Question 11: 
# What does the depth coefficient in model m4 estimate?
# The depth of the habitat kelp

# Question 12: 
# A question on p-values.

# Question 13: 
# After adjusting for depth and habitat, is there evidence that the species
# responds to protection status?

# Question 15:
# Using the output from model m5, give a 95% confidence interval
# for the ratio of average number of fish in reef vs kelp, all else being equal
confint.default(m5)
exp(confint.default(m5))


# Question 16:
# Interpret the above confidence interval, i.e., what does it tell you about the fish?


# Question 17: 
# Using model m5, predict the average number of fish per video at a depth of 30m
# in reef habitat, in protected and unprotected areas.

