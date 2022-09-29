# Load data
load("output/daily_data.Rda")

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

ggsave("figures/NEE.pdf", p, height = 5, width = 6, dpi = 320)

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

ggsave("figures/FCH4.pdf", p, height = 5, width = 6, dpi = 320)

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
