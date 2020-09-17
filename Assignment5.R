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
m1 <- aov(mass~food, data = frog)
summary(m1)
# (a.) 
# What is the estimated Mean Square for Error, to three decimal places? 
0.00264  = 0.003
# (b.)
# What is the F value for the treatment effect, to two decimal places?  
29.85
# (c.) 
# What is the numerator degrees of freedom for this F test?  
1
# (d.) 
# What is the denominator degrees of freedom for this F test?  
18



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




