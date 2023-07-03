library(ggplot2)   # graphics
library(here)
library(plotly)
library(forecast)
library(fpp)
library(lubridate)

# Load data
load(here("output/daily_data.Rda"))

# Data formatting for PCMCI analysis - USTAR?????
predictors <- c("FCH4","TA","WTD","VPD","GPP_PI_F_NT_gC","SO4_pred_interp","TP_interp","DOC_interp","pH_interp","site")
data.daily$month <- month(data.daily$datetime)

# Select data from 2021 for now
ind <- (data.daily$year >= 2021 & data.daily$year <= 2021)
data.daily.tigramite <- data.daily[ind,predictors] # Remove all missing data

# plot original data
ggplotly(ggplot()+
           geom_point(data = data.daily[ind,], aes(x = datetime,y = WTD,color = site)))

write.csv(data.daily.tigramite, file = here("output",'tigramite_2021.csv'),row.names=FALSE)
