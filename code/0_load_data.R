# Load the data

# load packages
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(openair) #For plotting wind and pollution roses (to explore the relationship between fluxes and wind direction)
library(ggsignif)
library(hms)
library(zoo)
library(here)

# Load function to calculate potential radiation
p <- sapply(list.files(pattern="potential_rad_generalized.R", path="/Users/sara/Code/Biomet.net/R/data_visualization/", full.names=TRUE), source)

# Load function to load data from the database
p <- sapply(list.files(pattern="read_database_generalized.R", path="/Users/sara/Code/Biomet.net/R/database_functions/", full.names=TRUE), source)

# Hogg site
db_path <- '/Users/sara/Library/CloudStorage/OneDrive-UBC/UBC/Database'
data.Hogg <- read_data_generalized(db_path,c(2021:2022),"HOGG","Clean/ThirdStage",
                                   c("NEE","NEE_PI_F_MDS","FCH4","FCH4_PI_F_MDS","FCH4_PI_F_RF","H","H_PI_F_MDS","LE","LE_PI_F_MDS",
                                     "GPP_PI_F_DT","GPP_PI_F_NT","Reco_PI_F_DT","Reco_PI_F_NT","NETRAD_1_1_1","P_1_1_1","PA_1_1_1",
                                     "RH_1_1_1","SW_IN_1_1_1","PPFD_IN_1_1_1","TA_1_1_1","TS_1","TS_2","TS_3","USTAR","VPD_1_1_1","WD_1_1_1","WS_1_1_1"),"clean_tv",0)

data.Hogg$datetime <- as.POSIXct(data.Hogg$datetime,"%Y-%m-%d %H:%M:%OS",tz = 'UTC')
data.Hogg$DOY <- yday(data.Hogg$datetime)
data.Hogg$pot_rad <- potential_rad_generalized(-90,-100.201894,50.361781,data.Hogg$datetime,data.Hogg$DOY)

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
data.Hogg$year <- year(data.Hogg$datetime)
yrs <- unique(data.Hogg$year)
yrs <- yrs[which(!is.na(unique(data.Hogg$year)))]

for (i in 1:(length(yrs)-1)) { #n-1 years since the last timestep is the first day of the following year
  NonNAindex <- which(!is.na(data.Hogg$WTD) & data.Hogg$year == yrs[i])
  firstNonNA <- min(NonNAindex)
  lastNonNA <- max(NonNAindex)
  
  data.Hogg$WTD[firstNonNA:lastNonNA] <- na.approx(data.Hogg$WTD[firstNonNA:lastNonNA])
}

# Linearly interpolate between WQ measurements
for (i in 1:(length(yrs)-1)) {
  
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
    
    data.Hogg$pH_interp[firstNonNA:lastNonNA] <- na.approx(data.Hogg$pH_interp[firstNonNA:lastNonNA])
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

# Young site
db_path <- '/Users/sara/Library/CloudStorage/OneDrive-UBC/UBC/Database'
data.Young <- read_data_generalized(db_path,c(2021:2022),"YOUNG","Clean/ThirdStage",
                                    c("NEE","NEE_PI_F_MDS","FCH4","FCH4_PI_F_MDS","FCH4_PI_F_RF","H","H_PI_F_MDS","LE","LE_PI_F_MDS",
                                      "GPP_PI_F_DT","GPP_PI_F_NT","Reco_PI_F_DT","Reco_PI_F_NT","NETRAD_1_1_1","P_1_1_1","PA_1_1_1",
                                      "RH_1_1_1","SW_IN_1_1_1","PPFD_IN_1_1_1","TA_1_1_1","TS_1","TS_2","TS_3","USTAR","VPD_1_1_1","WD_1_1_1","WS_1_1_1"),"clean_tv",0)

data.Young$datetime <- as.POSIXct(data.Young$datetime,"%Y-%m-%d %H:%M:%OS",tz = 'UTC')
data.Young$DOY <- yday(data.Young$datetime)
data.Young$pot_rad <- potential_rad_generalized(-90,-100.534947,50.370433,data.Young$datetime,data.Young$DOY)

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
data.Young$year <- year(data.Young$datetime)
yrs <- unique(data.Young$year)
yrs <- yrs[which(!is.na(unique(data.Young$year)))]

for (i in 1:(length(yrs)-1)) {
  NonNAindex <- which(!is.na(data.Young$WTD) & data.Young$year == yrs[i])
  firstNonNA <- min(NonNAindex)
  lastNonNA <- max(NonNAindex)
  
  data.Young$WTD[firstNonNA:lastNonNA] <- na.approx(data.Young$WTD[firstNonNA:lastNonNA])
}

# Linearly interpolate between WQ measurements
for (i in 1:(length(yrs)-1)) {
  
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
    
    data.Young$pH_interp[firstNonNA:lastNonNA] <- na.approx(data.Young$pH_interp[firstNonNA:lastNonNA])
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

# Add site variable
data.Hogg$site <- "Hogg"
data.Young$site <- "Young"

# Export subset of data for Matt F. - NOTE THAT WTD/WQ IS MISSING IN THE WINTER - FIGURE OUT THE BEST WAY TO HANDLE THAT. Should we consider just an average annual value for WQ parameters
# NOTE- SEND WITH CORRECTED RADIATION DATA ONCE DARIAN FIXES IT & update FCH4 data
#df.Hogg <- data.Hogg[,c('site','DATE','GPP_f', 'Reco','pot_rad','SWIN_1_1_1','VPD.x','air_p_mean','P_RAIN_1_1_1','TA_1_1_1','TS_1_1_1','TS_2_1_1','TS_3_1_1','WTD','LE_f','NEE_uStar_f','FCH4_f','FCH4',"pH_interp","Specific_cond_interp","DOC_interp","TDN_interp","NO3_NO2_N_interp","NH4_N_interp","DRP_interp","TDP_interp","TP_interp","ABS_280nm_interp","SO4_interp")]
# Replace FCH4_f with FCH4_gf_RF
#df.Young <- data.Young[,c('site','DATE','GPP_f', 'Reco','pot_rad','SWIN_1_1_1','VPD.x','air_p_mean','P_RAIN_1_1_1','TA_1_1_1','TS_1_1_1','TS_2_1_1','TS_3_1_1','WTD','LE_f','NEE_uStar_f','FCH4_f','FCH4',"pH_interp","Specific_cond_interp","DOC_interp","TDN_interp","NO3_NO2_N_interp","NH4_N_interp","DRP_interp","TDP_interp","TP_interp","ABS_280nm_interp","SO4_interp")]

#Saving the data tp csv (for Matt)
#write.table(df.Hogg, file = here("output",'Hogg.csv'),row.names=FALSE,sep='\t') 
#write.table(df.Young, file = here("output",'Young.csv'),row.names=FALSE,sep='\t') 

# Merge data frames
# First make sure column names are the same
setdiff(colnames(data.Hogg),colnames(data.Young))
setdiff(colnames(data.Young),colnames(data.Hogg))

data <- dplyr::bind_rows(data.Hogg,data.Young)

# Convert DATE from character to date
data$datetime <- as.POSIXct(data$datetime, format =  "%Y-%m-%d %H:%M:%OS", tz ='UTC')

# save 30 min data
save(data,file="output/30min_data.Rda")
