# Written by Sara Knox to analyzer the difference in C and GHG fluxes at Hogg vs. Young
# August, 2022

# Notes:
# 1) Check met variables at Young. 
# 2) Check large NEE values in Feb/April 2022
# 3) Better gap-filling

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

# Load function to calculate potential radiation
fx_path <- '/Users/sara/Code/MLABcode/data_visualization/'
p <- sapply(list.files(pattern="potential_rad_generalized.R", path=fx_path, full.names=TRUE), source)

# Load data
data_path <-
  '/Users/sara/Code/Research/2022_Hogg_Young_Comparison/data/'

# Young site
file <- 'Young_L3.csv'
data.Young <- read.csv(paste0(data_path, file), header = T)
data.Young$site <- 'Young'
vars.Young <- colnames(data.Young)

file <- 'Young_L2.csv'
data.Young.L2 <- read.csv(paste0(data_path, file), header = T)
vars.Young.L2 <- colnames(data.Young.L2)

ind <- which(data.Young.L2$DATE == data.Young$DATE)

data.Young$FCH4 <- data.Young.L2$ch4_flux[ind]
data.Young$datetime <- as.POSIXct(data.Young$DATE,"%Y-%m-%d %H:%M:%OS",tz = 'UTC')
data.Young$DOY <- yday(data.Young$datetime)
data.Young$pot_rad <- potential_rad(-90,-100.534947,50.370433,data.Young$datetime,data.Young$DOY)

# add WTD
file <- 'Young_WTH.csv'
data.Young.WTD <- read.csv(paste0(data_path, file), header = T)
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
data.Hogg <- read.csv(paste0(data_path, file), header = T)
data.Hogg$site <- 'Hogg'
vars.Hogg <- colnames(data.Hogg)

file <- 'Hogg_L2.csv'
data.Hogg.L2 <- read.csv(paste0(data_path, file), header = T)
vars.Hogg.L2 <- colnames(data.Hogg.L2)

ind <- which(data.Hogg.L2$DATE == data.Hogg$DATE)

data.Hogg$FCH4 <- data.Hogg.L2$ch4_flux[ind]
data.Hogg$datetime <- as.POSIXct(data.Hogg$DATE,"%Y-%m-%d %H:%M:%OS",tz = 'UTC')
data.Hogg$DOY <- yday(data.Hogg$datetime)
data.Hogg$pot_rad <- potential_rad(-90,-100.201894,50.361781,data.Hogg$datetime,data.Hogg$DOY)

# add WTD
file <- 'Hogg_WTH.csv'
data.Hogg.WTD <- read.csv(paste0(data_path, file), header = T)
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

#Saving the file:
write.table(df.Hogg, file = '/Volumes/GoogleDrive/My Drive/UBC/Knox/Projects/2022-Causal inference/Hogg.csv',row.names=FALSE,sep='\t') 
write.table(df.Young, file = '/Volumes/GoogleDrive/My Drive/UBC/Knox/Projects/2022-Causal inference/Young.csv',row.names=FALSE,sep='\t') 

# Merge data frames
data <- dplyr::bind_rows(df.Hogg,df.Young)

# Convert DATE from character to date
data$DATE <- as.POSIXct(data$DATE, format =  "%Y-%m-%d %H:%M:%OS", tz ='UTC')

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
# Group by site
conv_co2 <- 12.01*(60*60*24)/(10^6)
conv_ch4 <- 12.01*(60*60*24)/(10^6)

data.daily <- data %>%
  mutate(year = year(DATE),
         jday = yday(DATE)) %>%
  group_by(site,year,jday) %>%
    dplyr::summarize(FC_gC = mean(NEE_uStar_f, na.rm = TRUE)*conv_co2,
                     FCH4_gC = mean(FCH4_gf_RF, na.rm = TRUE)*conv_ch4,
                     date = first(DATE))

# Plot daily CO2 fluxes by site
colors_sites <- c(wes_palette("FantasticFox1"),wes_palette("Zissou1"))
p <- ggplot() +
  geom_point(data = data.daily, aes(x = jday, y = FC_gC, color = factor(site), shape = factor(year)), size = 0.6)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.daily, aes(x = as.Date(date), y = FC_gC, color = factor(site)), size = 1) + scale_x_date(date_labels = "%b %y") +
  xlab('') + ylab(expression(NEE~(g~C~m^-2~d^-1))) +
  scale_color_manual(values = c("Hogg"=colors_sites[4],
                                "Young"=colors_sites[3]),
                     name="Site") +
  ylim(-6, 5) + theme(legend.position="top",text = element_text(size = 18)) 
p  

ggsave("/Users/sara/Code/Research/2022_Hogg_Young_Comparison/figures/NEE.pdf", p, height = 5, width = 6, dpi = 320)

# Plot daily FCH4 fluxes by site
p <- ggplot() +
  geom_point(data = data.daily, aes(x = jday, y = FCH4_gC*1000, color = factor(site), shape = factor(year)), size = 0.6)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.daily, aes(x = as.Date(date), y = FCH4_gC*1000, color = factor(site)), size = 1) + scale_x_date(date_labels = "%b %y") + 
  xlab('') + ylab(expression(FCH4~(mg~C~m^-2~d^-1))) +
  scale_color_manual(values = c("Hogg"=colors_sites[4],
                                "Young"=colors_sites[3]),
                     name="Site") +
  ylim(-10, 150) + theme(legend.position="top",text = element_text(size = 18))
p  

ggsave("/Users/sara/Code/Research/2022_Hogg_Young_Comparison/figures/FCH4.pdf", p, height = 5, width = 6, dpi = 320)

# Create box plots of daily NEE and FCH4
p_FC <- ggplot(data.daily, aes(x=site, y=FC_gC)) + 
  geom_boxplot() + xlab('') + ylab(expression(NEE~(g~C~m^-2~d^-1))) +
  geom_signif(comparisons = list(c("Hogg", "Young")), 
              map_signif_level=TRUE, tip_length = 0) + theme(text = element_text(size = 18))
p_FC

t.test(FC_gC ~ site, data.daily)

p_FCH4 <- ggplot(data.daily, aes(x=site, y=FCH4_gC*1000)) + 
  geom_boxplot() + xlab('') + ylab(expression(FCH4~(mg~C~m^-2~d^-1))) +
  geom_signif(comparisons = list(c("Hogg", "Young")), 
              map_signif_level=TRUE, tip_length = 0) + theme(text = element_text(size = 18))
p_FCH4
t.test(FCH4_gC*1000 ~ site, data.daily)

p <- ggarrange(p_FC, p_FCH4, ncol = 2, labels = c("A", "B"))
p

ggsave("/Users/sara/Code/Research/2022_Hogg_Young_Comparison/figures/boxplot.pdf", p, height = 5, width = 8, dpi = 320)

# cumulative fluxes (gC m-2 yr-1)
# For the first year (June 1, 2021 to May 31, 2022)
ind_s_Young <- which(data.daily$date == as.POSIXct("2021-06-01",tz = 'UTC') & data.daily$site == 'Young')
ind_e_Young <- which(data.daily$date == as.POSIXct("2022-05-31",tz = 'UTC') & data.daily$site == 'Young')

ind_s_Hogg <- which(data.daily$date == as.POSIXct("2021-06-01",tz = 'UTC') & data.daily$site == 'Hogg')
ind_e_Hogg <- which(data.daily$date == as.POSIXct("2022-05-31",tz = 'UTC') & data.daily$site == 'Hogg')


# Hogg
data.cummulative.Hogg <- data.daily[c(ind_s_Hogg:ind_e_Hogg), ] %>%
  group_by(site) %>%
  dplyr::summarize(FC_gC = sum(FC_gC, na.rm = TRUE),
                   FCH4_gC = sum(FCH4_gC, na.rm = TRUE))

data.cummulative.Hogg$GHG <- data.cummulative.Hogg$FC_gC*44.01/12.011+data.cummulative.Hogg$FCH4_gC*16.04/12.011*45
data.cummulative.Hogg

# Young
data.cummulative.Young <- data.daily[c(ind_s_Young:ind_e_Young), ] %>%
  group_by(site) %>%
  dplyr::summarize(FC_gC = sum(FC_gC, na.rm = TRUE),
                   FCH4_gC = sum(FCH4_gC, na.rm = TRUE))

# CHECK CALCULATION BASED ON MARION'S WORK (i.e, may need to account for C lost as CH4)
data.cummulative.Young$GHG <- data.cummulative.Young$FC_gC*44.01/12.011+data.cummulative.Young$FCH4_gC*16.04/12.011*45
data.cummulative.Young
