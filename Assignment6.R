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
Burned.Excluded

Unburned.Excluded

Burned.Present

Unburned.Present

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
# of fire and a small effect of elephants, so there is a strong fire-elephant
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
sort(tapply(geckos$dens2013, geckos$fire, sd))
#  Unburned    Burned 
80.65897 238.05805 
# 238.05805 / 80.65897
#  2.951414
sort(tapply(geckos$dens2013, geckos$elephants, sd))
# Excluded   Present 
84.88678 279.21670
# 279.21670 / 84.88678
# 3.289284



### Question 8
# Check the degrees of freedom and make sure they correspond with your expectations.

# How many degrees of freedom does the factor 'fire' have?  
# How many degrees of freedom does the factor 'elephants' have?  
# How many degrees of freedom do we need for the interaction?  
# How many degrees of freedom do we need for the Block effect?  
# How many error degrees of freedom do we have?  
# What is the total number of experimental units?
# 3 sites and 4 treatments per site so = 12

library(emmeans)
m1 <- lm(dens2013 ~ elephants * fire, data = geckos)
summary(m1)

## --- or ANOVA
m1.aov <- aov(dens2013 ~ elephants * fire, data = geckos)
summary(m1.aov)

emmeans(m1, ~ elephants * fire)

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


#  Interaction plot for experiment referring to 2013 only
with(geckos,
     interaction.plot(elephants, fire, dens2013,
                      ylab = "density 2013",
                      cex = 1.3, col = 2:3, lwd = 3))

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


m2 <- aov(dens2013 ~ elephants + fire, data=geckos)
ph <- TukeyHSD(m2, ordered = T, conf.level=0.95)
