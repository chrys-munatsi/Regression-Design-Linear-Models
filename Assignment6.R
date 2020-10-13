### ASSIGNMENT SIX NEARLY THERE

setwd("~/r_practice/assignment-six")

### EXPLORATORY DATA ANALYSIS
geckos <- read.csv("Patch-scale+summary+data.csv")
summary(geckos)
head(geckos) ; tail(geckos)
names(geckos)

# Rename variables we need and levelsof factor relating to elephant treatment 
library(plyr)
geckos <-rename(geckos, c("July.2013.lizard.density.per.ha" = "dens2013",
                          "KLEE.Exclusion.Treatment" = "elephants",
                          "Fire.Treatment" = "fire"))
geckos$elephants <- revalue(geckos$elephants, c("Elephants present" = "Present", 
                                                "No elephants"  = "Excluded"))

# Question 1
# Levels of the blocking factor (three)
geckos$Block
# "Central" "North" "South" 

# Question 2
geckos$fire
# Levels of the factors related to fire (two)
# "Burned"  "Unburned"

# Question 3
# List the treatments
fenced megaherbivore‐exclusion plots (‘exclosures’)
‘unfenced’ control plots
# Burned.Excluded, Unburned.Excluded, Burned.Present, Unburned.Present

# Question 4
# Is the design balanced
# Yes

# Question 5
# How many replicates per treatment
# Three 

# Question 6
# Looking at boxplots is there a strong fire-elepahnts effect? Explain why
# Yes. In the treatment combination where the plot was burned and elephants were 
# present (Burned.Present) boxplot had the highest density of lizards per 
# hectare in July 2013. From the interaction plot, there is the main effect 
# of fire and the main of elephants, so there is a strong fire-elephant
# interaction effect.

boxplot(dens2013 ~ fire + elephants, data = geckos, xlab = "fire treatment", ylab = "density", las = 1, cex.lab = 1.5, cex.axis = 1.5)
stripchart(dens2013 ~ fire + elephants, data = geckos, add=T,vertical=TRUE, method="jitter", jitter=.1)
# From the above we see that 
# no outliers, errors are independent and normally distributed 
#checking for additive
attach(geckos)
plot(geckos$elephants[elephants == "Present"], geckos$elephants[elephants == "Excluded"], 
     xlab = "elephants", ylab = "Density 2013")
axis(1)
axis(2, las = 1)
points(geckos$fire[fire == "Burned"], geckos$fire[fire == "Unburned"], pch=19)
legend(1, 2.5, legend = c("elephants", "fire"), pch = c(21, 19), bty = "n" )



geckos$fire <- factor(geckos$fire)
geckos$elephants <- factor(geckos$elephants)

# An alternative to the boxplots
# shows that the average effect of fire is larger than that of elephants, i.e. that fire  may be the more important factor.
plot.design(dens2013 ~ elephants * fire,
            data = geckos,
            xlab = "treatment factors ", cex.axis = 1.5,
            cex.lab = 1.5, las = 1, cex = 1.3,
            ylab = "density 2013")

#An interaction plot
interaction.plot(elephants, fire, dens2013,
                 ylab = "density 2013",
                 cex = 1.3, col = 2:3, lwd = 3)


### Question 7
# Check the assumption of equal population variances by calculating the standard
# deviation of the observations by treatment.

# What is the ratio of the largest to the smallest standard deviation?
sd <- aggregate(geckos$dens2013 ~ geckos$elephants + geckos$fire, FUN = sd)
sd
97.38639 / 76.77484


### Question 8
# Check the degrees of freedom and make sure they correspond with your expectations.
## --- or ANOVA
m1.aov <- aov(dens2013 ~ elephants * fire, data = geckos)
summary(m1.aov)
# How many degrees of freedom does the factor 'fire' have?  
# How many degrees of freedom does the factor 'elephants' have?  
# How many degrees of freedom do we need for the interaction?  
# How many degrees of freedom do we need for the Block effect?  
# How many error degrees of freedom do we have?  
# What is the total number of experimental units?
# 3 sites and 4 treatments per site so = 12

### Question 9
# Starting with the third question we asked in the instructions ("Does the effect
# of fire depend on whether the trees were browsed by elephants?"), which 
# term do you test to answer this question? What is the null hypothesis for this test?
# What do the results mean, in relation to our question?
The term tested is Fire. The null hypothesis for this test is that the effect of 
fire is independent of whether trees were browsed by elephants or not.
The results show that the density of elephant damaged trees in plots with 
elephants present) was greater in burned patches than in unburned patches. 
The F value (72.97)  with the p-value (0·013 ) show that there is some 
evidence against the null hypothesis.


### Question 10
# According to the anova table the answer to the question &quot;Does elephant
# herbivory affect the density of geckos?&quot; should be:
There is evidence that, on average (across the fire levels), elephants affect
the density of geckos (p = 0.011).

# This is testing the effect of species on duration. The H0 is that all treatments 
# have the same mean. The F value is large and the p-value small so we have
# evidence against H0. That means that at least two treatments cause a different mean response.




### Question 12
What is the estimated gecko density for the treatment with elephants and burning 
in block 'Central'? 
library(emmeans)
m3 <- lm(dens2013 ~ Block * elephants * fire, data = geckos)
summary(m3)
emmeans(m3, ~ Block * elephants * fire)


### Question 13
# a. What was the main effect of elephants (average difference between elephants
# present and elephants excluded) on gecko density, to one decimal point?  
m1.aov <- aov(dens2013 ~ elephants * fire, data = geckos)
summary(m1.aov)
model.tables(m1.aov, type="effects", se=TRUE)
So I chose 202.1 or is it 101.02
# b. And what was the associated standard error?
from means its 51.33 or is it 36.29


### Question 14
# Give a 95% confidence interval for the difference between burned and unburned
# plots when elephants were excluded. Use 'unburned' - 'burned', i.e. the values
# should be negative if burning increases gecko density and positive if it 
# decreases gecko density.
m2 <- aov(dens2013 ~ elephants * fire, data=geckos)
ph <- TukeyHSD(m2, conf.level=0.95)
# > Excluded:Unburned-Excluded:Burned   -70.000000 -302.4498  162.4498 0.7726160

### Interpret the confidence interval you calculated in response to question 14.
# The confidence interval is very wide and includes zero which suggests large 
# variability and that there is no clear evidence for changes in lizard density

m1 <- lm(dens2013 ~ elephants * fire, data = geckos)
summary(m1)
emmeans(m1, ~ elephants * fire)




m3 <- lm(dens2013 ~ Block * elephants * fire, data = geckos)
summary(m3)
emmeans(m3, ~ Block * elephants * fire)

m1.lsm <- emmeans(m1, ~ elephants | fire)
m1.lsm

## user-defined contrasts
contrast(m1.lsm, list(c1 = c(1, -1, -1, 1), # interaction
                        c2 = c(1, -1, 1, -1) / 2, # unburnt vs burnt 
                        c3 = c(1, 1, -1, -1) / 2), # present vs excluded
           by = NULL)


m1.ctr <- contrast(m1.lsm, list(c1 = c(1, -1, -1, 1), # interaction
                         c2 = c(1, -1, 1, -1) / 2, # unburnt vs burnt
                         c3 = c(1, 1, -1, -1) / 2), # present vs excluded
           by = NULL)
summary(m1.ctr, infer = c(TRUE, TRUE))



model.tables(m1.aov, "means")




### For this one the line's DO cross
plot(geckos$fire, geckos$dens2013, las=1, ylab="Density 2013")
i<-1 # a running variable to help make colour coded lines
for(block in unique(geckos$elephants)){
  temp <- geckos[geckos$elephants == block,]
  temp <- temp[sort(as.numeric(temp$fire),index.return=TRUE)$ix,]
  lines(temp$fire, temp$dens2013, col=i)
  i<-i+1
}

### For this one the line's don't cross
plot(geckos$elephants, geckos$dens2013, las=1, ylab="Density 2013")
i<-1 # a running variable to help make colour coded lines
for(block in unique(geckos$fire)){
  temp <- geckos[geckos$fire == block,]
  temp <- temp[sort(as.numeric(temp$elephants),index.return=TRUE)$ix,]
  lines(temp$fire, temp$dens2013, col=i)
  i<-i+1
}



boxplot(dens2013 ~ fire + elephants, data = geckos, xlab = "fire treatment", ylab = "density", las = 1, cex.lab = 1.5, cex.axis = 1.5)
i<-1 # a running variable to help make colour coded lines
for(block in unique(geckos$elephants)){
  temp <- geckos[geckos$elephants == block,]
  temp <- temp[sort(as.numeric(temp$fire),index.return=TRUE)$ix,]
  lines(temp$elephants, temp$dens2013, col=i)
  i<-i+1
}

# examine standard deviation of observations per treatment
sort(tapply(geckos$dens2013, geckos$elephants, sd))

