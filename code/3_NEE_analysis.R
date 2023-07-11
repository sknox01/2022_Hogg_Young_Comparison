library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(here)
library(ggpubr)
library(cowplot)  # for publication friendly ggplot themes
library(wesanderson)
library(ggpubr)

# Explore 30 min data
load(here("output/30min_data.Rda"))

# Light response curve -----------------------------------------------------------------------------------------------------


# Only daytime values (i.e., PPFD > 10)
data$NEE_daytime <- data$NEE
data$NEE_daytime[data$PPFD_IN_1_1_1 < 10] <- NA
  
# Peak growing season only
ind_GS_Hogg <- which(((data$datetime >="2021-07-01 UTC" & data$datetime <"2021-9-1 UTC") & data$year == 2021 & data$site == 'Hogg' & data$PPFD_IN_1_1_1 >= 10 & !is.na(data$NEE_daytime) == TRUE)|
                  ((data$datetime >="2022-07-01 UTC" & data$datetime < "2022-9-1 UTC") & data$year == 2022 & data$site == 'Hogg' & data$PPFD_IN_1_1_1 >= 10 & !is.na(data$NEE_daytime) == TRUE))

data.GS.Hogg <- data[ind_GS_Hogg,] 

ind_GS_Young <- which(((data$datetime >="2021-07-01 UTC" & data$datetime <"2021-9-1 UTC") & data$year == 2021 & data$site == 'Young' & data$PPFD_IN_1_1_1 >= 10 & !is.na(data$NEE_daytime) == TRUE)|
                       ((data$datetime >="2022-07-01 UTC" & data$datetime < "2022-9-1 UTC") & data$year == 2022 & data$site == 'Young' & data$PPFD_IN_1_1_1 >= 10 & !is.na(data$NEE_daytime) == TRUE))

data.GS.Young <- data[ind_GS_Young,] 

# Fit curves

# Equation (same as bigleaf model - https://rdrr.io/github/lhmet-forks/bigleaf/man/light.response.html)
# NEE ~ -(alpha * PPFD/(1 - (PPFD/PPFD_ref) + (alpha * PPFD/GPP_ref)) - 
#  Reco)

PPFD_ref <- 2000
LRC.GS.Hogg.2021 <- nls(NEE_daytime~ -(alpha * PPFD_IN_1_1_1/(1 - (PPFD_IN_1_1_1/PPFD_ref) + (alpha * PPFD_IN_1_1_1/GPP_ref)) - 
                                         Reco),
                        data = data.GS.Hogg[data.GS.Hogg$year == 2021, ],
                        start = list(alpha = 0.01, GPP_ref = 20, Reco = 1))
summary(LRC.GS.Hogg.2021)

yhat.GS.Hogg.2021 <- predict(LRC.GS.Hogg.2021,newdata = data.GS.Hogg[data.GS.Hogg$year == 2021, 'PPFD_IN_1_1_1'])

LRC.GS.Hogg.2022 <- nls(NEE_daytime~ -(alpha * PPFD_IN_1_1_1/(1 - (PPFD_IN_1_1_1/PPFD_ref) + (alpha * PPFD_IN_1_1_1/GPP_ref)) - 
                                         Reco),
                        data = data.GS.Hogg[data.GS.Hogg$year == 2022, ],
                        start = list(alpha = 0.01, GPP_ref = 20, Reco = 1))
summary(LRC.GS.Hogg.2022)

yhat.GS.Hogg.2022 <- predict(LRC.GS.Hogg.2022,newdata = data.GS.Hogg[data.GS.Hogg$year == 2022, 'PPFD_IN_1_1_1'])

LRC.GS.Young.2021 <- nls(NEE_daytime~ -(alpha * PPFD_IN_1_1_1/(1 - (PPFD_IN_1_1_1/PPFD_ref) + (alpha * PPFD_IN_1_1_1/GPP_ref)) - 
                                         Reco),
                        data = data.GS.Young[data.GS.Young$year == 2021, ],
                        start = list(alpha = 0.01, GPP_ref = 20, Reco = 1))
summary(LRC.GS.Young.2021)

yhat.GS.Young.2021 <- predict(LRC.GS.Young.2021,newdata = data.GS.Young[data.GS.Young$year == 2021, 'PPFD_IN_1_1_1'])

LRC.GS.Young.2022 <- nls(NEE_daytime~ -(alpha * PPFD_IN_1_1_1/(1 - (PPFD_IN_1_1_1/PPFD_ref) + (alpha * PPFD_IN_1_1_1/GPP_ref)) - 
                                          Reco),
                         data = data.GS.Young[data.GS.Young$year == 2022, ],
                         start = list(alpha = 0.01, GPP_ref = 20, Reco = 1))
summary(LRC.GS.Young.2022)

yhat.GS.Young.2022 <- predict(LRC.GS.Young.2022,newdata = data.GS.Young[data.GS.Young$year == 2022, 'PPFD_IN_1_1_1'])

# Plot data
pal <- wes_palette("Zissou1", 360, type = "continuous")

p.Hogg.2021 <- ggplot(data = data.GS.Hogg[data.GS.Hogg$year == 2021,], aes(x = PPFD_IN_1_1_1, y = NEE_daytime)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey88", size=0.5) +
  geom_point(aes(color = WD_1_1_1),size = 0.3)+ylim(-30,20)+scale_color_gradientn(colours = pal)+
  geom_line(data = data.GS.Hogg[data.GS.Hogg$year == 2021,],aes(x = PPFD_IN_1_1_1, y = yhat.GS.Hogg.2021))+
  theme_classic()
p.Hogg.2021

p.Hogg.2022 <- ggplot(data = data.GS.Hogg[data.GS.Hogg$year == 2022,], aes(x = PPFD_IN_1_1_1, y = NEE_daytime)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey88", size=0.5) +
  geom_point(aes(color = WD_1_1_1),size = 0.3)+ylim(-30,20)+scale_color_gradientn(colours = pal)+
  geom_line(data = data.GS.Hogg[data.GS.Hogg$year == 2022,],aes(x = PPFD_IN_1_1_1, y = yhat.GS.Hogg.2022))+
  theme_classic()
p.Hogg.2022

p.Young.2021 <- ggplot(data = data.GS.Young[data.GS.Young$year == 2021,], aes(x = PPFD_IN_1_1_1, y = NEE_daytime)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey88", size=0.5) +
  geom_point(aes(color = WD_1_1_1),size = 0.3)+ylim(-30,20)+scale_color_gradientn(colours = pal)+
  geom_line(data = data.GS.Young[data.GS.Young$year == 2021,],aes(x = PPFD_IN_1_1_1, y = yhat.GS.Young.2021))+
  theme_classic()
p.Young.2021

p.Young.2022 <- ggplot(data = data.GS.Young[data.GS.Young$year == 2022,], aes(x = PPFD_IN_1_1_1, y = NEE_daytime)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey88", size=0.5) +
  geom_point(aes(color = WD_1_1_1),size = 0.3)+ylim(-30,20)+scale_color_gradientn(colours = pal)+
  geom_line(data = data.GS.Young[data.GS.Young$year == 2022,],aes(x = PPFD_IN_1_1_1, y = yhat.GS.Young.2022))+
  theme_classic()
p.Young.2022

# Save LRC plots in one figure
p <- ggarrange(p.Young.2021,p.Hogg.2021,p.Young.2022,p.Hogg.2022,common.legend = TRUE,legend = "bottom",ncol = 2, nrow = 2)
p

ggsave("figures/LRC_multipanel.png", p,units = "cm",height = 12, width = 12, dpi = 320)

# Model summary

summary(LRC.GS.Hogg.2021)
summary(LRC.GS.Hogg.2022)
summary(LRC.GS.Young.2021)
summary(LRC.GS.Young.2022)
