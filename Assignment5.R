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




