setwd("~/r_practice/assignment-five")

frog <- read.csv('Hyperolius.csv')

##### EXPLORATORY DATA ANALYSIS
head(frog) ; tail(frog) ; summary(frog)
attach(frog)
boxplot(mass ~ food, data=frog, range = 0, axes =T, ylab = "mean mass per container")
axis(1, at = 1:4, labels=sort(unique(frog$food)))
axis(2, at = seq(0, 100, 20), las = 1)
stripchart(mass ~ food, data = frog, add=TRUE, vertical=TRUE, method = "jitter", jitter=.1)
