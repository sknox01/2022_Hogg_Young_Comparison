# Load packages
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

# Load data
load("output/daily_data.Rda")

# Trim data frame to start on June 1, 2021 
ind_s_Young <- which(data.daily$datetime == as.POSIXct("2021-06-01",tz = 'UTC') & data.daily$site == 'Young')
ind_Young <- which(data.daily$site == 'Young')
ind_Young_last <- ind_Young[length(ind_Young)]

ind_s_Hogg <- which(data.daily$datetime == as.POSIXct("2021-06-01",tz = 'UTC') & data.daily$site == 'Hogg')
ind_Hogg <- which(data.daily$site == 'Hogg')
ind_Hogg_last <- ind_Hogg[length(ind_Hogg)]

data.daily <- data.daily[c(ind_s_Hogg:ind_Hogg_last,ind_s_Young:ind_Young_last), ] 

# Plot daily CO2 fluxes by site
colors_sites <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # Color blind palette from: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/

p <- ggplot() +
  geom_line(data = data.daily, aes(x = as.Date(datetime), y = FC_gC, color = factor(site)),linewidth=0.3) + scale_x_date(date_labels = "%b %y") +
  xlab('') + ylab(expression(NEE~(g~C~m^-2~d^-1))) +
  scale_color_manual(values = c("Hogg"=colors_sites[6],
                                "Young"=colors_sites[2]),
                     name="Site") +
  theme_bw() +
  ylim(-8, 8) + theme(#legend.position = c(0.1, 0.85),
                      text = element_text(size = 10),
                      legend.title = element_text(size=6),
                      legend.text = element_text(size=6),
                      legend.key.height=unit(0.5,"line")) 
p  

ggsave("figures/NEE.png", p,units = "cm", height = 5, width = 9, dpi = 320)

# Plot daily FCH4 fluxes by site
p <- ggplot() +
  geom_point(data = data.daily, aes(x = as.Date(datetime), y = FCH4_gC*1000, color = factor(site)), size = 1) + scale_x_date(date_labels = "%b %y") + 
  xlab('') + ylab(expression(FCH4~(mg~C~m^-2~d^-1))) +
  scale_color_manual(values = c("MBPPW1"=colors_sites[4],
                                "MBPPW2"=colors_sites[3]),
                     name="") +
  ylim(-10, 150) + theme(legend.position="bottom",text = element_text(size = 10))
p  

ggsave("figures/FCH4.pdf", p,units = "cm",height = 5, width = 6, dpi = 320)

# cumulative fluxes (gC m-2 yr-1)
# For the first year (June 1, 2021 to May 31, 2022)
ind_s_Young <- which(data.daily$datetime == as.POSIXct("2021-06-01",tz = 'UTC') & data.daily$site == 'Young')
ind_e_Young <- which(data.daily$datetime == as.POSIXct("2022-05-31",tz = 'UTC') & data.daily$site == 'Young')

ind_s_Hogg <- which(data.daily$datetime == as.POSIXct("2021-06-01",tz = 'UTC') & data.daily$site == 'Hogg')
ind_e_Hogg <- which(data.daily$datetime == as.POSIXct("2022-05-31",tz = 'UTC') & data.daily$site == 'Hogg')

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
