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

# Question 4
# Is the design balanced
# Yes

# Question 5
# How many replicates per treatment

# Question 6
# Looking at boxplots is there a strong fire-elepahnts effect? Explain why
boxplot(dens2013 ~ fire + elephants, data = geckos, xlab = "fire treatment", ylab = "density", las = 1, cex.lab = 1.5, cex.axis = 1.5)
stripchart(dens2013 ~ fire + elephants, data = geckos, add=T,vertical=TRUE, method="jitter", jitter=.1)

geckos$fire <- factor(geckos$fire)
geckos$elephants <- factor(geckos$elephants)

# An alternative to the boxplots
# shows that the average effect of fire is larger than that of elephants, i.e. that fire  may be the more important factor.
plot.design(dens2013 ~ elephants * fire,
            data = geckos,
            xlab = "treatment factors ", cex.axis = 1.5,
            cex.lab = 1.5, las = 1, cex = 1.3,
            ylab = "density 2013")

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


plot(geckos$elephants, geckos$dens2013, las=1, ylab="density 2013")
i<-1 # a running variable to help make colour coded lines
for(block in unique(geckos$babbler)){
  temp <- geckos[geckos$babbler == block,]
  temp <- temp[sort(as.numeric(temp$call),index.return=TRUE)$ix,]
  lines(temp$call, temp$lrt, col=i)
  i<-i+1
}







# examine standard deviation of observations per treatment
sort(tapply(geckos$dens2013, geckos$elephants, sd))





