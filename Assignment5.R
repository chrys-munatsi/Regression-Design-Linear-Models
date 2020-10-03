setwd("~/r_practice/assignment-five")

frog <- read.csv('Hyperolius.csv')

##### EXPLORATORY DATA ANALYSIS
head(frog) ; tail(frog) ; summary(frog)
attach(frog)
boxplot(mass ~ food, data=frog, range = 0, axes =T, ylab = "mean mass per container")
axis(1, at = 1:4, labels=sort(unique(frog$food)))
axis(2, at = seq(0, 100, 20), las = 1)
stripchart(mass ~ food, data = frog, add=TRUE, vertical=TRUE, method = "jitter", jitter=.1)


# Question 2 
# Is this a balanced design
# True
# Younneed to correct and make sure it works


# Question 3

# (a.) 
# What was the value of the largest observation for the response variable 
0.49

# (b.) 
# Which treatment was it observed in?  
90

# (c.)
# What was the value of the smallest observation for the response variable
0.17

# (d.)
# Which treatment was it observed in?  
30


# Question 4
# Are any of these extreme values likely to be influential outliers?
sort(tapply(frog$mass, frog$food, sd))
# No 


# Question 5 
# Check the assumption of equal population variances by calculating the standard 
# deviation of the observations by treatment.
sort(tapply(frog$mass, frog$food, sd))
# (a.) 
# What is the ratio of the largest to the smallest standard deviation? 
0.07224957 / 0.03130495 = 2.307928
# (b.) 
# Is the assumption of equal variance reasonable? (Yes or No)
# Yes 


# Question 6
m1 <- aov(values~treatment)
summary(m1)
# (a.) 
# What is the estimated Mean Square for Error, to three decimal places? 
0.030
# (b.)
# What is the F value for the treatment effect, to two decimal places?  
13.21
# (c.) 
# What is the numerator degrees of freedom for this F test?  
3
# (d.) 
# What is the denominator degrees of freedom for this F test?  
16


# Question 7
# What are the estimated mean and standard error for treatment "10", respectively?
m1.gm <- lm(values ~ treatment - 1)
summary(m1.gm)
# 0.23600 - estimate
# 0.02131 - SE

# Question 8 
# What is the (absolute) estimated difference between treatments "10" and "60" 
0.30000 - 0.23600
# and the associated standard error, respectively


# Question 9
# What are the estimated mean and standard error for treatment "60", respectively? 
# estimate - 0.30000
# SE - 0.02131

# Question 10
# What is the (absolute) estimated difference between treatments "30" and "60"
# and the associated standard error, respectively, rounded to two decimal places?  
# estimate 0.30000-0.21600 

# Question 11
# What is the p-value corresponding to the null hypothesis that treatments "30" and "60"
# have the same mean, rounded to two decimal places? 
data1 =data.frame(treatment,values)
m2 <- t.test(values ~ treatment, data = data1[data1$treatment %in% c("30", "60"),])
# p-value = 0.006287

# Question 12 
# Summarize what you have learned about the relationship between mass at
# metamorphosis and amount of food from the above analyses (that is, from the box plots, 
# anova and contrasts) in your own words.


# Question 13 
# Use Tukey's procedure to calculate all possible pairwise contrasts then answer the following questions. All answers must be rounded to 2 decimal places.

# (a.) 
# What is the (absolute) estimated difference between treatments "30" and "60"? 

 
# (b.) What are the lower and upper 95% confidence limits corresponding to the 
# difference given in (a.)

# (c.) 
# What is the adjusted p-value for the comparison in (a.)? 
library(multcompView)
treatment <- factor(sort(rep(c(10,30,60,90),5)))
values = c(0.29,0.24,0.22,0.24,0.19,0.25,0.23,0.20,0.23,0.17,0.34,0.24,0.33,0.29,0.3,0.41,0.49,0.37,0.38,0.29)

data=data.frame(treatment,values)

# What is the effect of the treatment on the value ?
model=lm(data$values ~ data$treatment)
ANOVA=aov(model)
summary(ANOVA)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'data$treatment', conf.level=0.95)
TUKEY
