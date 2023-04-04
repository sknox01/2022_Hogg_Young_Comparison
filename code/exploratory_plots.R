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

# Load data
load("output/daily_data.Rda")

# Plot daily CO2 fluxes by site
colors_sites <- c(wes_palette("FantasticFox1"),wes_palette("Zissou1"))
p <- ggplot() +
  geom_point(data = data.daily, aes(x = jday, y = FC_gC, color = factor(site), shape = factor(year)), size = 0.6)
toWebGL(ggplotly(p))

# Plot daily FCH4 fluxes by site
p <- ggplot() +
  geom_point(data = data.daily, aes(x = jday, y = FCH4_gC*1000, color = factor(site), shape = factor(year)), size = 0.6)
toWebGL(ggplotly(p))

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

# Create box plots of daily NEE and FCH4 and look at differences between sites
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

ggsave("figures/boxplot.pdf", p, height = 5, width = 8, dpi = 320)