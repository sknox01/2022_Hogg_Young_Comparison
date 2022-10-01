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

# NOTE: HOGG removed row 22-08-2022 13:00:04	22	8	2022	13:00:04	369
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
file <- 'Young_WTD_20220709.csv'
#file <- 'Young_WTH.csv'
data.Young.WTD <- read.csv(here("data",file), header = T)
vars.Young.WTD <- colnames(data.Young.WTD)
data.Young.WTD$time <- strptime(data.Young.WTD$Time, "%H:%M:%S",tz="UTC")
data.Young.WTD$datetime <- make_datetime(data.Young.WTD$Year, data.Young.WTD$Month, data.Young.WTD$Day, hour(data.Young.WTD$time), minute(data.Young.WTD$time))
#data.Young.WTD$datetime <- round_date(as.POSIXct(data.Young.WTD$DateTime,"%d-%m-%Y %H:%M:%OS",tz = 'UTC'),"hour")

data.Young <- data.Young %>% 
  left_join(data.Young.WTD,by = 'datetime')

# Add water quality data
file <- 'Young_WQ_2021_2022.csv'
data.Young.WQ_all <- read.csv(here("data",file), header = T)
vars.Young.WQ_all <- colnames(data.Young.WQ_all)
data.Young.WQ_all$datetime <- as.POSIXct(data.Young.WQ_all$Date, tz = "UTC")
data.Young.WQ <- data.Young.WQ_all[, c(seq(5,15),length(vars.Young.WQ_all)+1)] # select only variables of interest
colnames(data.Young.WQ) <- c("pH","Specific_cond","DOC","TDN","NO3_NO2_N","NH4_N","DRP","TDP","TP","ABS_280nm","SO4","datetime")

data.Young <- data.Young %>% 
  left_join(data.Young.WQ,by = 'datetime')

# Create new data frame and rename columns for interpolated variables
colnames(data.Young.WQ) <- c("pH_interp","Specific_cond_interp","DOC_interp","TDN_interp","NO3_NO2_N_interp","NH4_N_interp","DRP_interp","TDP_interp","TP_interp","ABS_280nm_interp","SO4_interp","datetime")
data.Young <- data.Young %>% 
  left_join(data.Young.WQ,by = 'datetime')

# Linearly interpolate between hourly WTD measurements
# Loop over each year
yrs <- unique(data.Young$Year)
yrs <- yrs[which(!is.na(unique(data.Young$Year)))]

for (i in 1:length(yrs)) {
  NonNAindex <- which(!is.na(data.Young$WTD) & data.Young$year == yrs[i])
  firstNonNA <- min(NonNAindex)
  lastNonNA <- max(NonNAindex)
  
  data.Young$WTD[firstNonNA:lastNonNA] <- na.approx(data.Young$WTD[firstNonNA:lastNonNA])
}

# Linearly interpolate between WQ measurements
for (i in 1:length(yrs)) {
  
  # First SO4 
  NonNAindex <- which(!is.na(data.Young$SO4) & data.Young$year == yrs[i])
  firstNonNA <- min(NonNAindex)
  lastNonNA <- max(NonNAindex)
  
  data.Young$SO4_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$SO4_interp[firstNonNA:lastNonNA])
  
  # Next all other variables - update once we have the latest WQ data
  NonNAindex <- which(!is.na(data.Young$Specific_cond) & data.Young$year == yrs[i])
  
  if (length(NonNAindex) >0) {
    firstNonNA <- min(NonNAindex)
    lastNonNA <- max(NonNAindex)
    
    data.Young$Specific_cond_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$Specific_cond_interp[firstNonNA:lastNonNA])
    data.Young$DOC_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$DOC_interp[firstNonNA:lastNonNA])
    data.Young$TDN_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$TDN_interp[firstNonNA:lastNonNA])
    data.Young$NO3_NO2_N_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$NO3_NO2_N_interp[firstNonNA:lastNonNA])
    data.Young$NH4_N_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$NH4_N_interp[firstNonNA:lastNonNA])
    data.Young$DRP_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$DRP_interp[firstNonNA:lastNonNA])
    data.Young$TDP_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$TDP_interp[firstNonNA:lastNonNA])
    data.Young$TP_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$TP_interp[firstNonNA:lastNonNA])
    data.Young$ABS_280nm_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$ABS_280nm_interp[firstNonNA:lastNonNA])
  }
}

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
file <- 'Hogg_WTD_20220709.csv'
#file <- 'Hogg_WTD.csv'
data.Hogg.WTD <- read.csv(here("data",file), header = T)
vars.Hogg.WTD <- colnames(data.Hogg.WTD)
data.Hogg.WTD$time <- strptime(data.Hogg.WTD$Time, "%H:%M:%S",tz="UTC")
data.Hogg.WTD$datetime <- make_datetime(data.Hogg.WTD$Year, data.Hogg.WTD$Month, data.Hogg.WTD$Day, hour(data.Hogg.WTD$time), minute(data.Hogg.WTD$time))
#data.Hogg.WTD$datetime <- round_date(as.POSIXct(data.Hogg.WTD$DateTime,"%d-%m-%Y %H:%M:%OS",tz = 'UTC'),"hour")

data.Hogg <- data.Hogg %>% 
  left_join(data.Hogg.WTD,by = 'datetime')

# Add water quality data
file <- 'Hogg_WQ_2021_2022.csv'
data.Hogg.WQ_all <- read.csv(here("data",file), header = T)
vars.Hogg.WQ_all <- colnames(data.Hogg.WQ_all)
data.Hogg.WQ_all$datetime <- as.POSIXct(data.Hogg.WQ_all$Date, tz = "UTC")
data.Hogg.WQ <- data.Hogg.WQ_all[, c(seq(5,15),length(vars.Hogg.WQ_all)+1)] # select only variables of interest
colnames(data.Hogg.WQ) <- c("pH","Specific_cond","DOC","TDN","NO3_NO2_N","NH4_N","DRP","TDP","TP","ABS_280nm","SO4","datetime")

data.Hogg <- data.Hogg %>% 
  left_join(data.Hogg.WQ,by = 'datetime')

# Create new data frame and rename columns for interpolated variables
colnames(data.Hogg.WQ) <- c("pH_interp","Specific_cond_interp","DOC_interp","TDN_interp","NO3_NO2_N_interp","NH4_N_interp","DRP_interp","TDP_interp","TP_interp","ABS_280nm_interp","SO4_interp","datetime")
data.Hogg <- data.Hogg %>% 
  left_join(data.Hogg.WQ,by = 'datetime')

# Linearly interpolate between hourly WTD measurements
# Loop over each year
yrs <- unique(data.Hogg$Year)
yrs <- yrs[which(!is.na(unique(data.Hogg$Year)))]

for (i in 1:length(yrs)) {
  NonNAindex <- which(!is.na(data.Hogg$WTD) & data.Hogg$year == yrs[i])
  firstNonNA <- min(NonNAindex)
  lastNonNA <- max(NonNAindex)
  
  data.Hogg$WTD[firstNonNA:lastNonNA] <- na.approx(data.Hogg$WTD[firstNonNA:lastNonNA])
}

# Linearly interpolate between WQ measurements
for (i in 1:length(yrs)) {
  
  # First SO4 
  NonNAindex <- which(!is.na(data.Hogg$SO4) & data.Hogg$year == yrs[i])
  firstNonNA <- min(NonNAindex)
  lastNonNA <- max(NonNAindex)
  
  data.Hogg$SO4_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$SO4_interp[firstNonNA:lastNonNA])
  
  # Next all other variables - update once we have the latest WQ data
  NonNAindex <- which(!is.na(data.Hogg$Specific_cond) & data.Hogg$year == yrs[i])
  
  if (length(NonNAindex) >0) {
    firstNonNA <- min(NonNAindex)
    lastNonNA <- max(NonNAindex)
    
    data.Hogg$Specific_cond_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$Specific_cond_interp[firstNonNA:lastNonNA])
    data.Hogg$DOC_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$DOC_interp[firstNonNA:lastNonNA])
    data.Hogg$TDN_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$TDN_interp[firstNonNA:lastNonNA])
    data.Hogg$NO3_NO2_N_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$NO3_NO2_N_interp[firstNonNA:lastNonNA])
    data.Hogg$NH4_N_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$NH4_N_interp[firstNonNA:lastNonNA])
    data.Hogg$DRP_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$DRP_interp[firstNonNA:lastNonNA])
    data.Hogg$TDP_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$TDP_interp[firstNonNA:lastNonNA])
    data.Hogg$TP_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$TP_interp[firstNonNA:lastNonNA])
    data.Hogg$ABS_280nm_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$ABS_280nm_interp[firstNonNA:lastNonNA])
  }
}

# Export subset of data for Matt F. - NOTE THAT WTD IS MISSING IN THE WINTER - FIGURE OUT THE BEST WAY TO HANDLE THAT
df.Hogg <- data.Hogg[,c('site','DATE','GPP_f', 'Reco','pot_rad','SWIN_1_1_1','PPFD_1_1_1','VPD.x','air_p_mean','P_RAIN_1_1_1','TA_1_1_1','TS_1_1_1','TS_2_1_1','TS_3_1_1','WTD','LE_f','NEE_uStar_f','FCH4_f','FCH4_gf_RF','FCH4','SO4_interp','TP_interp')]
# Replace FCH4_f with FCH4_gf_RF
df.Young <- data.Young[,c('site','DATE','GPP_f', 'Reco','pot_rad','SWIN_1_1_1','PPFD_1_1_1','VPD.x','air_p_mean','P_RAIN_1_1_1','TA_1_1_1','TS_1_1_1','TS_2_1_1','TS_3_1_1','WTD','LE_f','NEE_uStar_f','FCH4_f','FCH4','SO4_interp','TP_interp')]

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
  geom_point(data = data.site, aes(x = DATE, y = WTD, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

# Look at radiation data in more depth - use composite plots as in Ameriflux
p <- ggplot() +
  geom_line(data = data.site, aes(x = DATE, y = PPFD_1_1_1, color = as.factor(site)), size = 1) +
  geom_line(data = data.site, aes(x = DATE, y = SWIN_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.site, aes(x = DATE, y = SWIN_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.site, aes(x = DATE, y = SWIN_1_1_1, color = as.factor(site)), size = 1) +
  geom_line(data = data.site, aes(x = DATE, y = pot_rad, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.site, aes(x = DATE, y = PPFD_1_1_1/1.6, color = as.factor(site)), size = 1) +
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

# USE ONLY _gf_RF once available
p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = FCH4_f, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = DATE, y = SO4_interp, color = as.factor(site)), size = 1)
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

# Look at monthly data!
# 
