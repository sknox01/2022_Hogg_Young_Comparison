# Conditional Inference Trees analysis

# specify libraries needed
library(party)
library(plyr)
library(readr)

# Load data
load("output/daily_data.Rda")

# Add water quality data
data.daily$SO4 <- NA
data.daily$SO4[which(data.daily$site == 'Young')] <- 305
data.daily$SO4[which(data.daily$site == 'Hogg')] <- 1114

data.daily$TP <- NA
data.daily$TP[which(data.daily$site == 'Young')] <- 494
data.daily$TP[which(data.daily$site == 'Hogg')] <- 164

tree<-ctree(FCH4_gC~GPP_f_gC+WTH+TS+SO4+TP, data=data.daily)
plot(tree)