library(lubridate) 
library(dplyr)     
library(ggplot2) 

# Load data
load("output/daily_data.Rda")

# Include a full year of data - DOY 140, 2021 to DOY 139, 2022
ind <- (data.daily$year == 2021 & data.daily$jday >=140) | (data.daily$year == 2022 & data.daily$jday <140) 
data.daily <- data.daily[ind, ]

ggplot(data.daily, aes(x=jday, y=FCH4_gC*1000, color = as.factor(site)))+
  geom_line()

# Calculate stats
mean(data.daily$FCH4_gC[data.daily$site == "Young"]*1000,na.rm = T)
mean(data.daily$FCH4_gC[data.daily$site == "Hogg"]*1000,na.rm = T)
median(data.daily$FCH4_gC[data.daily$site == "Young"]*1000,na.rm = T)
median(data.daily$FCH4_gC[data.daily$site == "Hogg"]*1000,na.rm = T)

mean(data.daily$FCH4_gC[data.daily$site == "Young"]/12*1000,na.rm = T)
mean(data.daily$FCH4_gC[data.daily$site == "Hogg"]/12*1000,na.rm = T)
median(data.daily$FCH4_gC[data.daily$site == "Young"]/12*1000,na.rm = T)
median(data.daily$FCH4_gC[data.daily$site == "Hogg"]/12*1000,na.rm = T)

# Rename sites
data.daily$site[data.daily$site == "Young"] <- "Low Salinity Site"
data.daily$site[data.daily$site == "Hogg"] <- "High Salinity Site"


# Create boxplot
p_FCH4 <- ggplot(data.daily, aes(x=site, y=FCH4_gC/12.01*1000)) + 
  geom_boxplot() + xlab('') + ylab(expression(CH[4]~flux~(mmol~m^-2~d^-1))) + 
  theme(text = element_text(size = 12))
p_FCH4

ggsave("figures/boxplot.pdf", p_FCH4, height = 6, width = 5, dpi = 320)




