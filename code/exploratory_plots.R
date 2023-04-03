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

# Explore 30 min data
load("output/30min_data.Rda")

# Group by site
data.site <- data %>%
  group_by(site)

# Plot individual variables by site
p <- ggplot() +
  geom_line(data = data.site, aes(x = datetime, y = pot_rad, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = WTD, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.site, aes(x = datetime, y = PPFD_IN_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.site, aes(x = datetime, y = SW_IN_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.site, aes(x = datetime, y = SW_IN_1_1_1, color = as.factor(site)), size = 1) +
  geom_line(data = data.site, aes(x = datetime, y = pot_rad, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = P_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.site, aes(x = datetime, y = TA_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.site, aes(x = datetime, y = RH_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

# Check soil temperature in more detail....
p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = TS_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = TS_2, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = TS_3, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.site, aes(x = datetime, y = VPD_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = PA_1_1_1, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

# Plot CO2 fluxes by site
p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = NEE, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = NEE_PI_F_MDS, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

# Plot FCH4 fluxes by site
p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = FCH4_PI_F_RF, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

# Plot water quality variables
p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = SO4_interp, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_point(data = data.site, aes(x = datetime, y = ABS_280nm_interp, color = as.factor(site)), size = 1)
toWebGL(ggplotly(p))

# compute daily mean fluxes (using filled data for now)
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
                   FCH4_gC = mean(FCH4_PI_F_RF, na.rm = TRUE)*conv_ch4,
                   WTD = mean(WTD, na.rm = TRUE),
                   TS = mean(TS_2, na.rm = TRUE),
                   SW_IN = mean(SW_IN_1_1_1, na.rm = TRUE)*conv_energy,
                   PPFD_IN = mean(PPFD_IN_1_1_1, na.rm = TRUE),
                   LE = mean(LE_PI_F_MDS, na.rm = TRUE)*conv_energy,
                   H = mean(LE_PI_F_MDS, na.rm = TRUE)*conv_energy,
                   datetime = first(datetime),
                   site = first(site))

p <- ggplot() +
  geom_line(data = data.daily, aes(x = datetime, y = FC_gC, color = as.factor(site)))
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.daily, aes(x = datetime, y = FCH4_gC, color = as.factor(site)))
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.daily, aes(x = datetime, y = H, color = as.factor(site)))
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.daily, aes(x = datetime, y = LE, color = as.factor(site)))
toWebGL(ggplotly(p))

# save daily data
save(data.daily,file="output/daily_data.Rda")