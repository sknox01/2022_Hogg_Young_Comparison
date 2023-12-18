library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(here)
library(plotly)
library(ggpubr)
library(mgcv)
library(visreg)
library(gratia)

# Load data
load(here("output/daily_data.Rda")) # Average only for days with more than 50% of data

 #only daily FCH4 data with more than 50% coverage
data.daily$FCH4[data.daily$FCH4_n<48/2] <- NA

# Calculate salinity (ppt) from specific conductivity data
data.daily$salinity_model <- data.daily$Specific_cond_interp*0.0005 - 0.0224

# Create DOC/SO4 variable
data.daily$DOC_SO4_interp <- data.daily$DOC_interp/data.daily$SO4_pred_interp

# Only consider fluxes for days with more than 50% of data

# Define dataset for periods when we have water quality data
predictors_model <- c("FCH4","TA","WTD","GPP_PI_F_NT_gC","TP_interp","pH_interp",
                      "Specific_cond_interp")

data.daily.model <- na.omit(data.daily[,c(predictors_model,"site","year","datetime")]) # Remove all missing data
data.daily.model$site <- as.factor(data.daily.model$site)
data.daily.model$year <- as.factor(data.daily.model$year)
data.daily.model$site_year <- NA
data.daily.model$site_year[(data.daily.model$year == 2021 & data.daily.model$site == "Young")] <- "2021.Young"
data.daily.model$site_year[(data.daily.model$year == 2022 & data.daily.model$site == "Young")] <- "2022.Young"
data.daily.model$site_year[(data.daily.model$year == 2021 & data.daily.model$site == "Hogg")] <- "2021.Hogg"
data.daily.model$site_year[(data.daily.model$year == 2022 & data.daily.model$site == "Hogg")] <- "2022.Hogg"
data.daily.model$site_year <- as.factor(data.daily.model$site_year)

predictors <- c("FCH4","TA","WTD","GPP","TP","pH",
                "Cond")

colnames(data.daily.model)[1:length(predictors_model)] <- predictors

# GAM Analysis
# We can use GAM modeling to model FCH4 as an additive function of covariates.

# Using gamm
additive_gam_mod1 <- gamm(FCH4 ~ site_year + s(TA, k=5, by=site_year) + s(WTD, k=5, by=site_year) + s(GPP, k=5, by=site_year) + s(TP, k=5, by=site_year) + 
                            s(Cond, k=5, by=site_year) + s(pH, k=5, by=site_year),  data = data.daily.model, method = "REML")

summary(additive_gam_mod1$gam)

res_additive_mod1 <- resid(additive_gam_mod1$lme, type = "normalized")

gam.check(additive_gam_mod1$gam)

acf(res_additive_mod1)

# View partial plots
additive_gam_mod1$gam$data <- data.daily.model
visreg(additive_gam_mod1$gam, xvar = "TA", by="site_year", overlay=TRUE, points=list(cex=0.8))
visreg(additive_gam_mod1$gam, xvar = "WTD", by="site_year", overlay=TRUE, points=list(cex=0.8))
visreg(additive_gam_mod1$gam, xvar = "GPP", by="site_year", overlay=TRUE, points=list(cex=0.8))
visreg(additive_gam_mod1$gam, xvar = "TP", by="site_year", overlay=TRUE, points=list(cex=0.8))
visreg(additive_gam_mod1$gam, xvar = "pH", by="site_year", overlay=TRUE, points=list(cex=0.8))
visreg(additive_gam_mod1$gam, xvar = "Cond", by="site_year", overlay=TRUE, points=list(cex=0.8))

# We can re-fit the original model using gam() rather than gamm() and further explore model fit using the R library gratia. 
# Using gam
additive_gam_mod2 <- gam(FCH4 ~ site_year + s(TA, k=5, by=site_year) + s(WTD, k=5, by=site_year) + s(GPP, k=5, by=site_year) + s(TP, k=5, by=site_year) + 
                            s(Cond, k=5, by=site_year) + s(pH, k=5, by=site_year),  data = data.daily.model, method = "REML")

summary(additive_gam_mod2$gam)
gam.check(additive_gam_mod2)
appraise(additive_gam_mod2)

Questions 
#Justification to remove DOC rather than SO4?
#gamm vs gam?
#interp vs carry forward for discrete samples
#k-values in gamm/gam
#Which type of interp to do?
#Do we need by site/year
