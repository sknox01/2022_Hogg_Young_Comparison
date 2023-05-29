library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(here)
library(psych)
library(rstatix)
library(lme4)
library(AICcmodavg)

# Load data
load(here("output/daily_data.Rda"))

# Mixed effects modeling
vars_model <- c("FCH4_gC","TA","WTD","SO4_interp","TP_interp","DOC_interp","NH4_N_interp","NO3_NO2_N_interp")
data.daily.model <- na.omit(data.daily[,c(vars_model,"site","year")]) 
data.daily.model$site <- as.factor(data.daily.model$site)
data.daily.model$year <- as.factor(data.daily.model$year)

# Yr nested in site
# Notes:
# https://wiki.qcbs.ca/r_workshop6
# This is variation we simply want to control for (sometimes referred to as random factors).
# Random effect - They usually are grouping factors for which you want to control the effect in your model, 
# but are not interested in their specific effect on the response variable.

# In LMM's allowing intercepts and/or slopes to vary by certain factors (random effects) simply means 
# you assume they come from a normal distribution. A mean and standard deviation of that distribution are estimated based on your data. 
# The most likely intercepts and slopes from that distribution are then fit by optimization (ex. maximum likelihood or restricted maximum likelihood).

# Confidence intervals of intercepts and slopes are adjusted to account for pseudoreplication based on the intraclass correlation coefficient (ICC) 
# - How much variation is there among groups versus within groups? This determines your effective sample size - 
# An adjusted sample sized based on how correlated the data within groups are.

# mixed models can deal with unbalanced designs 

# Steps
# 1) Check distribution of variables - Major skews can lead to problems with model homogeneity down the road. 
# 2) Check for collinearity between variables: It is important not to include 2 collinear variables in the same model, otherwise their effects on the response variable will be confounded

# Inspect the data
pairs.panels(
  data.daily.model[,vars_model],
  main = "",
  gap = 0, # set to zero for no gap between plot panels
  lm = TRUE, # draw linear regression lines for pairs
  stars = TRUE, # display significance
  bg = c('#257ABA', '#C9B826')[data.daily.model$site], # color based on site
  pch = 21) # data point shape

# Normality test - Shapiro test. Normally distributed if p > 0.05. Note that most are NOT normally distributed
df <- na.omit(data.daily.model)%>%
  group_by(site) # removed by year here. But should it be included?

df.dist <- list()
for (i in 1:length(vars_model)){
  dist <- df%>%dplyr::rename("var" = vars_model[i]) %>%shapiro_test(var)
  dist$variable <- c(vars_model[i],vars_model[i])
  df.dist[[i]] <- dist
}

# NOTES: DOC and SO4 are highly correlated - removing DOC for now. See if we should remove any other WQ variables

# 3) Consider the scale of your data:
# If two variables in the same model have different ranges of scale, the criteria the mixed models use to come up with parameter estimates are likely to return 'convergence errors'. 
# Z-correction standardizes your variables and adjusts for this scaling problem by placing all your variables on the same scale even if they were originally in different units

data.daily.model.norm <- data.daily.model
data.daily.model.norm[,vars_model] <- as.data.frame(scale(data.daily.model[,vars_model]))

# 4) To know if a mixed model is necessary for your data set you must establish if it is actually important to account for variation in the factors that might be affecting the relationship that you're interested in, Lake and Species in this case.
# We can do this by:
# a) Creating a linear model without factors
# b) Calculating the residuals of this linear model
# c) Plotting the residuals of the linear model against the factors of interest

# TEST simple linear model
m0<-lm(FCH4_gC~TA+WTD+SO4_interp+TP_interp+NH4_N_interp+NO3_NO2_N_interp, data=data.daily.model)
m0.resid<-rstandard(lm.test)

# site effect
plot(m0.resid~ data.daily.model.norm$site, xlab = "Site", ylab="Standardized residuals")
abline(0,0, lty=2)

# year effect - test when more than 1 year is available
plot(m0.resid~ data.daily.model.norm$year, xlab = "Year", ylab="Standardized residuals")
abline(0,0, lty=2)

# 5) Coding potential models and model selection

# REML (Restricted Maximum Likelihood) is the default estimation method in the “lmer” function. 
# REML estimates can be used when comparing models with the same fixed effects (i.e. nested models). 
# However, if you are comparing models where the fixed effects differ among models then maximum likelihood should be used to fit parameters as this method is not dependent on the coefficients of the fixed effects. 
# Fitting using maximum likelihood is done by setting REML=FALSE in the lmer command.

m1 <- lmer(FCH4_gC~TA+WTD+SO4_interp+TP_interp+NH4_N_interp+NO3_NO2_N_interp+(1|site), data = data.daily.model.norm, REML=F)
min <- min(data.daily.model.norm$FCH4_gC)-0.001
m2 <- lmer(log(FCH4_gC-min)~TA+WTD+SO4_interp+TP_interp+NH4_N_interp+NO3_NO2_N_interp+(1|site), data = data.daily.model.norm, REML=F) # deal with negative values
m3 <- lmer(log(FCH4_gC-min)~TA+WTD+SO4_interp+TP_interp+NH4_N_interp+(1|site), data = data.daily.model.norm, REML=F) # deal with negative values
# TEST MORE!! AND CHECK IF REML SHOULD BE T/F

AICc<-c(AICc(m0), AICc(m1), AICc(m2), AICc(m3))
# Put values into one table for easy comparison
Model<-c("M0", "M1", "M2", "M3")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable

# 6) Model validation
# Checking model assumptions
# a) Look at independence: plot fitted values vs residuals
m.final <- m2
E1 <- resid(m.final) # Select final model
F1<-fitted(m.final)
plot(x = F1, 
     y = E1, 
     xlab = "Fitted Values",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

# b) Look at independence:
# i. plot residuals vs each covariate in the model
# CHECK EACH VARIABLE
plot(x = data.daily.model.norm$WTD, 
     y = E1, 
     xlab = "WTD",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

boxplot(E1 ~ site,   
        ylab = "Normalized residuals",
        data = data.daily.model.norm, xlab = "Site")
abline(h = 0, lty = 2)

# c) Look at normality: histogram
hist(E1)

# Interpreting results and visualizing the model
# a) Interpreting results
summary(m.final)

