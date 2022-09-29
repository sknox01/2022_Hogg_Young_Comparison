# Load the data

# load packages
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(plotly)   # Allows you to zoom in on plots
library(openair) #For plotting wind and pollution roses (to explore the relationship between fluxes and wind direction)
library(wesanderson)
library(ggsignif)
library(ggpubr)
library(hms)
library(zoo)
library(here)

# Load function to calculate potential radiation
p <- sapply(list.files(pattern="potential_rad_generalized.R", path=here("code/Functions"), full.names=TRUE), source)

# Young site
file <- 'Young_L3.csv'
data.Young <- read.csv(here("data",file), header = T)
data.Young$site <- 'Young'
vars.Young <- colnames(data.Young)

file <- 'Young_L2.csv'
data.Young.L2 <- read.csv(here("data",file), header = T)
vars.Young.L2 <- colnames(data.Young.L2)

ind <- which(data.Young.L2$DATE == data.Young$DATE)

data.Young$FCH4 <- data.Young.L2$ch4_flux[ind]
data.Young$datetime <- as.POSIXct(data.Young$DATE,"%Y-%m-%d %H:%M:%OS",tz = 'UTC')
data.Young$DOY <- yday(data.Young$datetime)
data.Young$pot_rad <- potential_rad(-90,-100.534947,50.370433,data.Young$datetime,data.Young$DOY)

# add WTD
file <- 'Young_WTH.csv'
data.Young.WTD <- read.csv(here("data",file), header = T)
vars.Young.WTD <- colnames(data.Young.WTD)
data.Young.WTD$datetime <- round_date(as.POSIXct(data.Young.WTD$DateTime,"%d-%m-%Y %H:%M:%OS",tz = 'UTC'),"hour")

data.Young <- data.Young %>% 
  left_join(data.Young.WTD,by = 'datetime')

# Linearly interpolate between hourly measurements
NonNAindex <- which(!is.na(data.Young$WTH))
firstNonNA <- min(NonNAindex)
lastNonNA <- max(NonNAindex)

data.Young$WTH[firstNonNA:lastNonNA] <- na.approx(data.Young$WTH[firstNonNA:lastNonNA])

# Hogg site
file <- 'Hogg_L3.csv'
data.Hogg <- read.csv(here("data",file), header = T)
data.Hogg$site <- 'Hogg'
vars.Hogg <- colnames(data.Hogg)

file <- 'Hogg_L2.csv'
data.Hogg.L2 <- read.csv(here("data",file), header = T)
vars.Hogg.L2 <- colnames(data.Hogg.L2)

ind <- which(data.Hogg.L2$DATE == data.Hogg$DATE)

data.Hogg$FCH4 <- data.Hogg.L2$ch4_flux[ind]
data.Hogg$datetime <- as.POSIXct(data.Hogg$DATE,"%Y-%m-%d %H:%M:%OS",tz = 'UTC')
data.Hogg$DOY <- yday(data.Hogg$datetime)
data.Hogg$pot_rad <- potential_rad(-90,-100.201894,50.361781,data.Hogg$datetime,data.Hogg$DOY)

# add WTD
file <- 'Hogg_WTH.csv'
data.Hogg.WTD <- read.csv(here("data",file), header = T)
vars.Hogg.WTD <- colnames(data.Hogg.WTD)
data.Hogg.WTD$datetime <- round_date(as.POSIXct(data.Hogg.WTD$DateTime,"%d-%m-%Y %H:%M:%OS",tz = 'UTC'),"hour")

data.Hogg <- data.Hogg %>% 
  left_join(data.Hogg.WTD,by = 'datetime')

# Linearly interpolate between hourly measurements
NonNAindex <- which(!is.na(data.Hogg$WTH))
firstNonNA <- min(NonNAindex)
lastNonNA <- max(NonNAindex)

data.Hogg$WTH[firstNonNA:lastNonNA] <- na.approx(data.Hogg$WTH[firstNonNA:lastNonNA])

# Export subset of data for Matt F.
df.Hogg <- data.Hogg[,c('site','DATE','GPP_f', 'Reco','pot_rad','SWIN_1_1_1','VPD.x','air_p_mean','P_RAIN_1_1_1','TA_1_1_1','TS_1_1_1','TS_2_1_1','TS_3_1_1','WTH','LE_f','NEE_uStar_f','FCH4_gf_RF','FCH4')]
df.Young <- data.Young[,c('site','DATE','GPP_f', 'Reco','pot_rad','SWIN_1_1_1','VPD.x','air_p_mean','P_RAIN_1_1_1','TA_1_1_1','TS_1_1_1','TS_2_1_1','TS_3_1_1','WTH','LE_f','NEE_uStar_f','FCH4_gf_RF','FCH4')]

#Saving the data tp csv (for Matt)
#write.table(df.Hogg, file = here("output",'Hogg.csv'),row.names=FALSE,sep='\t') 
#write.table(df.Young, file = here("output",'Young.csv'),row.names=FALSE,sep='\t') 

# Merge data frames
data <- dplyr::bind_rows(df.Hogg,df.Young)

# Convert DATE from character to date
data$DATE <- as.POSIXct(data$DATE, format =  "%Y-%m-%d %H:%M:%OS", tz ='UTC')

# save 30 min data
save(data,file="output/30min_data.Rda")

# Group by site
data.site <- data %>%
  group_by(site)

# Plot met by site
p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = pot_rad, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = WTH, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = SWIN_1_1_1, color = as.factor(site)), size = 1) +
  geom_line(data = data.site, aes(x = DATE, y = pot_rad, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = P_RAIN_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = TA_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = TS_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = TS_2_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = TS_3_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = VPD.x, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = air_p_mean, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

# Plot CO2 fluxes by site
p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = NEE_uStar_f, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

# Plot FCH4 fluxes by site
p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = FCH4_gf_RF, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

# compute daily mean fluxes (using filled data for now)
conv_co2 <- 12.01*(60*60*24)/(10^6)
conv_ch4 <- 12.01*(60*60*24)/(10^6)
data.daily <- data %>%
  mutate(year = year(DATE),
         jday = yday(DATE)) %>%
  group_by(site,year,jday) %>%
  dplyr::summarize(FC_gC = mean(NEE_uStar_f, na.rm = TRUE)*conv_co2,
                   GPP_f_gC = mean(GPP_f, na.rm = TRUE)*conv_co2,
                   Reco_gC = mean(Reco, na.rm = TRUE)*conv_co2,
                   FCH4_gC = mean(FCH4_gf_RF, na.rm = TRUE)*conv_ch4,
                   WTH = mean(WTH, na.rm = TRUE),
                   TS = mean(TS_2_1_1, na.rm = TRUE),
                   SW_IN = mean(SWIN_1_1_1, na.rm = TRUE),
                   VPD = mean(VPD.x, na.rm = TRUE),
                   LE_f = mean(LE_f, na.rm = TRUE),
                   date = first(DATE))

# save daily data
save(data.daily,file="output/daily_data.Rda")

