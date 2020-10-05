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


