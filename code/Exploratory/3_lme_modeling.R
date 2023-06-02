library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(here)
library(psych)
library(rstatix)
library(nlme)
library(lme4)
library(AICcmodavg)
library(plotly)

# Load data
load(here("output/30min_data.Rda"))
#load(here("output/daily_data.Rda"))

# Visualize data 
# NOTE: do some data filtering?

ggplotly(ggplot(data, aes(x = datetime,color = site))+
  geom_point(aes(y = FCH4))) %>% toWebGL()

# compute daily fluxes (testing filtering - filter summer differently from winter?)
ind_Hogg <- which(data$site == "Hogg")
data$FCH4[ind_Hogg][which((data$FCH4[ind_Hogg] < -50) | (data$FCH4[ind_Hogg] > 50))] <- NA
ind_Young <- which(data$site == "Young")
#filter summer range
data$FCH4[ind_Young][which((data$FCH4[ind_Young] < -150) | (data$FCH4[ind_Young] > 300))] <- NA
#filter winter range
ind_winter <- (data$datetime[ind_Young] > "2021-09-01 00:00:00 UTC" & data$datetime[ind_Young] < "2022-04-01 00:00:00 UTC")
data$FCH4[ind_Young][which((data$FCH4[ind_Young] < -150) | (data$FCH4[ind_Young] > 100) & ind_winter)] <- NA

ggplotly(ggplot(data, aes(x = datetime,color = site))+
           geom_point(aes(y = FCH4))) %>% toWebGL()

# Compute daily data
conv_co2 <- 12.01*(60*60*24)/(10^6)
conv_ch4 <- 12.01*(60*60*24)/(10^9)
conv_energy <- (60*60*24)/(10^6)
data.daily <- data %>%
  mutate(year = year(datetime),
         jday = yday(datetime)) %>%
  group_by(site,year,jday) %>%
  dplyr::summarize(FC_gC = mean(NEE_PI_F_MDS, na.rm = TRUE)*conv_co2,
                   GPP_PI_F_NT_gC = mean(GPP_PI_F_NT, na.rm = TRUE)*conv_co2,
                   GPP_PI_F_DT_gC = mean(GPP_PI_F_DT, na.rm = TRUE)*conv_co2,
                   Reco_PI_F_NT_gC = mean(Reco_PI_F_NT, na.rm = TRUE)*conv_co2,
                   Reco_PI_F_DT_gC = mean(Reco_PI_F_DT, na.rm = TRUE)*conv_co2,
                   FCH4_gC = mean(FCH4, na.rm = TRUE)*conv_ch4,
                   WTD = mean(WTD, na.rm = TRUE),
                   TS = mean(TS_2, na.rm = TRUE),
                   TA = mean(TA_1_1_1, na.rm = TRUE),
                   VPD = mean(VPD_1_1_1, na.rm = TRUE),
                   SW_IN = mean(SW_IN_1_1_1, na.rm = TRUE)*conv_energy,
                   PPFD_IN = mean(PPFD_IN_1_1_1, na.rm = TRUE),
                   P = sum(P_1_1_1, na.rm = TRUE),
                   LE = mean(LE_PI_F_MDS, na.rm = TRUE)*conv_energy,
                   H = mean(LE_PI_F_MDS, na.rm = TRUE)*conv_energy,
                   pH = mean(pH,na.rm = TRUE),
                   pH_interp = mean(pH_interp,na.rm = TRUE),
                   Specific_cond = mean(Specific_cond,na.rm = TRUE),
                   Specific_cond_interp = mean(Specific_cond_interp,na.rm = TRUE),
                   DOC = mean(DOC,na.rm = TRUE),
                   DOC_interp = mean(DOC_interp,na.rm = TRUE), 
                   TDN = mean(TDN,na.rm = TRUE),
                   TDN_interp = mean(TDN_interp,na.rm = TRUE), 
                   SO4 = mean(SO4,na.rm = TRUE),
                   SO4_interp = mean(SO4_interp,na.rm = TRUE), 
                   SO4_pred = mean(SO4_pred,na.rm = TRUE),
                   SO4_pred_interp = mean(SO4_pred_interp,na.rm = TRUE), 
                   NO3_NO2_N = mean(NO3_NO2_N,na.rm = TRUE),
                   NO3_NO2_N_interp = mean(NO3_NO2_N_interp,na.rm = TRUE), 
                   NH4_N = mean(NH4_N,na.rm = TRUE),
                   NH4_N_interp = mean(NH4_N_interp,na.rm = TRUE), 
                   DRP = mean(DRP,na.rm = TRUE),
                   DRP_interp = mean(DRP_interp,na.rm = TRUE), 
                   TDP = mean(TDP,na.rm = TRUE),
                   TDP_interp = mean(TDP_interp,na.rm = TRUE), 
                   TP = mean(TP,na.rm = TRUE),
                   TP_interp = mean(TP_interp,na.rm = TRUE), 
                   ABS_280nm = mean(ABS_280nm,na.rm = TRUE),
                   ABS_280nm_interp = mean(ABS_280nm_interp,na.rm = TRUE), 
                   datetime = first(datetime),
                   site = first(site))

# plot daily data
ggplotly(ggplot()+
  geom_point(data = data.daily, aes(x = datetime,y = FCH4_gC,color = site)))

# A little bit more filtering
data.daily$FCH4_gC[data.daily$FCH4_gC < -0.019] <- NA

ggplotly(ggplot()+
           geom_line(data = data.daily, aes(x = datetime,y = FCH4_gC,color = site)))

ggplotly(ggplot()+
           geom_line(data = data.daily, aes(x = datetime,y = WTD,color = site)))

ggplotly(ggplot()+
           geom_line(data = data.daily, aes(x = datetime,y = TA,color = site)))

ggplotly(ggplot()+
           geom_line(data = data.daily, aes(x = datetime,y = GPP_PI_F_NT_gC,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = datetime,y = SO4,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = datetime,y = SO4_pred,color = site)))

ggplotly(ggplot()+
           geom_line(data = data.daily, aes(x = datetime,y = SO4_pred_interp,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = datetime,y = TP_interp,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = datetime,y = NH4_N_interp,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = datetime,y = NO3_NO2_N_interp,color = site)))

# See if any variables need to be transformed
# plot transformed data
# First check to see which variables are skewed
vars <- c("FCH4_gC","TA","WTD","GPP_PI_F_NT_gC","SO4_pred_interp","TP_interp","DOC_interp","NH4_N_interp","NO3_NO2_N_interp")
data.daily.model <- na.omit(data.daily[,c(vars,"site","year","datetime")]) 
data.daily.model$site <- as.factor(data.daily.model$site)
data.daily.model$year <- as.factor(data.daily.model$year)

pairs.panels(
  data.daily.model[,vars],
  main = "",
  gap = 0, # set to zero for no gap between plot panels
  lm = TRUE, # draw linear regression lines for pairs
  stars = TRUE, # display significance
  bg = c('#257ABA', '#C9B826')[data.daily.model$site], # color based on site
  pch = 21) # data point shape

# Transform select variables
min_FCH4 <- min(na.omit(data.daily.model$FCH4_gC))-0.1
data.daily.model$logFCH4_gC <- log(data.daily.model$FCH4_gC-min_FCH4)

ggplotly(ggplot(data.daily.model, aes(x = datetime,color = site))+
           geom_line(aes(y = logFCH4_gC)))

data.daily.model$logSO4_pred_interp <- log(data.daily.model$SO4_pred_interp)
data.daily.model$logSO4_pred_interp_sqd <- log(data.daily.model$SO4_pred_interp^2)
data.daily.model$logTP_interp <- log(data.daily.model$TP_interp)
data.daily.model$logNH4_N_interp <- log(data.daily.model$NH4_N_interp)
data.daily.model$logNO3_NO2_N_interp <- log(data.daily.model$NO3_NO2_N_interp)

ggplotly(ggplot(data.daily.model, aes(x = datetime,color = site))+
           geom_line(aes(y = logSO4_pred_interp)))
ggplotly(ggplot(data.daily.model, aes(x = datetime,color = site))+
           geom_line(aes(y = logSO4_pred_interp_sqd)))
ggplotly(ggplot(data.daily.model, aes(x = datetime,color = site))+
           geom_line(aes(y = logTP_interp)))
ggplotly(ggplot(data.daily.model, aes(x = datetime,color = site))+
           geom_line(aes(y = logNH4_N_interp)))
ggplotly(ggplot(data.daily.model, aes(x = datetime,color = site))+
           geom_line(aes(y = logNO3_NO2_N_interp)))

# Mixed modeling
# could explore Yr nested in site once we have multiple years

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
  subset(data.daily.model, select = -c(site,year,datetime)),
  main = "",
  gap = 0, # set to zero for no gap between plot panels
  lm = TRUE, # draw linear regression lines for pairs
  stars = TRUE, # display significance
  bg = c('#257ABA', '#C9B826')[data.daily.model$site], # color based on site
  pch = 21) # data point shape

# Normality test - Shapiro test. Normally distributed if p > 0.05. Note that most are NOT normally distributed
vars_model <- colnames(subset(data.daily.model, select = -c(site,year,datetime)))
df <- na.omit(data.daily.model)#%>%
  #group_by(site) # removed by site and year here. But should it be included?
df <- df[,vars_model]

df.dist <- list()
for (i in 1:length(vars_model)){
  dist <- df%>%dplyr::rename("var" = vars_model[i]) %>%shapiro_test(var)
  dist$variable <- c(vars_model[i]) #c(vars_model[i],vars_model[i])
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
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

# Fit the full model 
lm1.full <- lm(logFCH4_gC~TA+WTD+GPP_PI_F_NT_gC+logSO4_pred_interp+logTP_interp+logNH4_N_interp+logNO3_NO2_N_interp, data = data.daily.model.norm)
# Stepwise regression model
lm1.step <- stepAIC(lm1.full, direction = "both", 
                      trace = FALSE)
summary(lm1.step)

lm2.full <- lm(FCH4_gC~TA+WTD+GPP_PI_F_NT_gC+logSO4_pred_interp+logTP_interp+logNH4_N_interp+logNO3_NO2_N_interp, data = data.daily.model.norm)
# Stepwise regression model
lm2.step <- stepAIC(lm2.full, direction = "both", 
                    trace = FALSE)
summary(lm2.step)

AICc<-c(AICc(lm1.step), AICc(lm2.step))
# Put values into one table for easy comparison
Model<-c("LM1", "LM2")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable

# site effect
plot(lm1.step$residuals~ data.daily.model.norm$site, xlab = "Site", ylab="Standardized residuals")
abline(0,0, lty=2)

plot(lm2.step$residuals~ data.daily.model.norm$site, xlab = "Site", ylab="Standardized residuals")
abline(0,0, lty=2)

# year effect - test when more than 1 year is available

# 5) Coding potential models and model selection

# REML (Restricted Maximum Likelihood) is the default estimation method in the “lmer” function. 
# REML estimates can be used when comparing models with the same fixed effects (i.e. nested models). 
# However, if you are comparing models where the fixed effects differ among models then maximum likelihood should be used to fit parameters as this method is not dependent on the coefficients of the fixed effects. 
# Fitting using maximum likelihood is done by setting REML=FALSE in the lmer command.

# This an example of why REML should not be used when comparing models with different fixed effects. 
# REML, however, often estimates the random effects parameters better and therefore it is sometimes recommended to use ML for comparisons and REML 
# for estimating a single (perhaps final) model.

mth <- "ML"
lme1 <- lme(logFCH4_gC~TA+WTD+GPP_PI_F_NT_gC+logSO4_pred_interp+logTP_interp+logNH4_N_interp+logNO3_NO2_N_interp,random=~1|site,data=data.daily.model.norm,method = mth) # Log seems better

lme2 <- lme(logFCH4_gC~TA,random=~1|site,data=data.daily.model.norm,method = mth)
lme3 <- lme(logFCH4_gC~WTD,random=~1|site,data=data.daily.model.norm,method = mth)
lme4 <- lme(logFCH4_gC~GPP_PI_F_NT_gC,random=~1|site,data=data.daily.model.norm,method = mth) # Best
lme5 <- lme(logFCH4_gC~logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = mth)
lme6 <- lme(logFCH4_gC~logTP_interp,random=~1|site,data=data.daily.model.norm,method = mth) 
lme7 <- lme(logFCH4_gC~logNH4_N_interp,random=~1|site,data=data.daily.model.norm,method = mth)
lme8 <- lme(logFCH4_gC~logNO3_NO2_N_interp,random=~1|site,data=data.daily.model.norm,method = mth)

AICc<-c(AICc(lme1), AICc(lme2),AICc(lme3),AICc(lme4),AICc(lme5),AICc(lme6),AICc(lme7),AICc(lme8))
# Put values into one table for easy comparison
Model<-c("LME1", "LME2","LME3", "LME4","LME5", "LME6", "LME7", "LME8")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable

lme2 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+TA,random=~1|site,data=data.daily.model.norm,method = mth)
lme3 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+WTD,random=~1|site,data=data.daily.model.norm,method = mth)
lme4 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = mth)
lme5 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp,random=~1|site,data=data.daily.model.norm,method = mth) # Best
lme6 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logNH4_N_interp,random=~1|site,data=data.daily.model.norm,method = mth)
lme7 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logNO3_NO2_N_interp,random=~1|site,data=data.daily.model.norm,method = mth)

AICc<-c(AICc(lme1), AICc(lme2),AICc(lme3),AICc(lme4),AICc(lme5),AICc(lme6),AICc(lme7))
# Put values into one table for easy comparison
Model<-c("LME1", "LME2","LME3", "LME4","LME5", "LME6", "LME7")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable

lme2 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA,random=~1|site,data=data.daily.model.norm,method = mth) # Best 
lme3 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+WTD,random=~1|site,data=data.daily.model.norm,method = mth) 
lme4 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = mth) 
lme5 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+logNH4_N_interp,random=~1|site,data=data.daily.model.norm,method = mth) 
lme6 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+logNO3_NO2_N_interp,random=~1|site,data=data.daily.model.norm,method = mth) 

AICc<-c(AICc(lme1), AICc(lme2),AICc(lme3),AICc(lme4),AICc(lme5),AICc(lme6))
# Put values into one table for easy comparison
Model<-c("LME1", "LME2","LME3", "LME4","LME5", "LME6")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable

lme2 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+WTD,random=~1|site,data=data.daily.model.norm,method = mth)  
lme3 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = mth) 
lme4 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNH4_N_interp,random=~1|site,data=data.daily.model.norm,method = mth) 
lme5 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp,random=~1|site,data=data.daily.model.norm,method = mth) # Best 

AICc<-c(AICc(lme1), AICc(lme2),AICc(lme3),AICc(lme4),AICc(lme5))
# Put values into one table for easy comparison
Model<-c("LME1", "LME2","LME3", "LME4","LME5")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable

lme2 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+WTD,random=~1|site,data=data.daily.model.norm,method = mth) # best 
lme3 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = mth)  
lme4 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+logNH4_N_interp,random=~1|site,data=data.daily.model.norm,method = mth) 

AICc<-c(AICc(lme1), AICc(lme2),AICc(lme3),AICc(lme4))
# Put values into one table for easy comparison
Model<-c("LME1", "LME2","LME3", "LME4")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable

lme1 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+WTD,random=~1|site,data=data.daily.model.norm,method = mth) 
lme2 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+WTD+logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = mth) # best 
lme3 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+WTD+logNH4_N_interp,random=~1|site,data=data.daily.model.norm,method = mth) 

AICc<-c(AICc(lme1), AICc(lme2),AICc(lme3))
# Put values into one table for easy comparison
Model<-c("LME1", "LME2","LME3")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable

lme1 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+WTD+logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = mth) # best 
lme2 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+WTD+logSO4_pred_interp+logNH4_N_interp,random=~1|site,data=data.daily.model.norm,method = mth) 

AICc<-c(AICc(lme1), AICc(lme2))
# Put values into one table for easy comparison
Model<-c("LME1", "LME2")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable

# With and without temporal autocorrelation
# https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html
lme1 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+WTD+logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = mth) 
lme2 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+WTD+logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = mth,correlation=corAR1()) # Best - same as lme3
lme3 <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+WTD+logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = mth,correlation=corAR1(form = ~ 1 | site)) 

AICc<-c(AICc(lme1), AICc(lme2),AICc(lme3))
# Put values into one table for easy comparison
Model<-c("LME1", "LME2","LME3")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable

# Refit final model
m.final <- lme(logFCH4_gC~GPP_PI_F_NT_gC+logTP_interp+TA+logNO3_NO2_N_interp+WTD+logSO4_pred_interp,random=~1|site,data=data.daily.model.norm,method = "REML",correlation=corAR1()) 
summary(m.final)

# 6) Model validation
# Checking model assumptions
# a) Look at independence: plot fitted values vs residuals
E1 <- residuals(m.final) # Select final model
F1<-fitted(m.final)
plot(x = F1, 
     y = E1, 
     xlab = "Fitted Values",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

data.daily.model.norm$F1 <- F1
data.daily.model.norm$E1 <- E1

# Check for temporal autocorrelation in the model residuals - https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html
acf(E1)

# Visualize time series (change if final model changes)
ggplot(data.daily.model.norm, aes(x = datetime,color = site))+
  geom_point(aes(y = logFCH4_gC))+
  geom_line(aes(y = F1))

ggplot(data.daily.model.norm, aes(x = logFCH4_gC,y = E1,color = site))+
  geom_point()

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
shapiro_test(E1)

# Interpreting results and visualizing the model
# a) Interpreting results
summary(m.final)
coef(m.final)
