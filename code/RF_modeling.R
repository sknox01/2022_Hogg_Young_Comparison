# Load packages

library(tidyverse)
library(gridExtra)
library(ggplot2)

# Load functions to calculate GWP, GWP* and radiative forcing (from Ma et al. 2024 - https://www.nature.com/articles/s41612-024-00778-z)
p <- sapply(list.files(pattern="GHGpertubationconversion.R", path="/Users/saraknox/Code/2022_Hogg_Young_Comparison/code/Ma_code/", full.names=TRUE), source)

# Manitoba wetlands staying as wetlands
# Note that std is based on the interannual variability between years. 
CAEM1 <- GHGpertubationconversion(time1=-50, time2 = 500, pre_co2 = -39, pre_co2sd = sd(c(-55,-59,-4)), pre_ch4 = 3.9, pre_ch4sd = sd(c(3.7,3.3,4.3)),
                                  pst_co2 = -39, pst_co2sd = sd(c(-55,-59,-4)), pst_ch4 = 3.9, pst_ch4sd = sd(c(3.7,3.3,4.3)), type = "ins", runs=100)

CAEM2 <- GHGpertubationconversion(time1=-50, time2 = 500, pre_co2 = -209, pre_co2sd=sd(c(-233,-174,-217)),pre_ch4 = 0.4, pre_ch4sd =sd(c(0.3,0.5,0.6)),
                                      pst_co2 = -209, pst_co2sd = sd(c(-233,-174,-217)), pst_ch4 = 0.4, pst_ch4sd = sd(c(0.3,0.5,0.6)), type = "ins", runs=100)


CAEM3 <- GHGpertubationconversion(time1=-50, time2 = 500, pre_co2 = -205, pre_co2sd=sd(c(-233,-174,-217)), pre_ch4 = 16.2, pre_ch4sd =sd(c(3.7,3.3,4.3)), #sd based on the other sites for now
                                  pst_co2 = -205, pst_co2sd = sd(c(-233,-174,-217)), pst_ch4 = 16.2, pst_ch4sd = sd(c(3.7,3.3,4.3)), type = "ins", runs=100)


# Combine data frames

CAEM1$site <- "CA-EM1"
CAEM2$site <- "CA-EM2"
CAEM3$site <- "CA-EM3"

df_comb <- rbind(CAEM1, CAEM2, CAEM3)

ggplot(df_comb, aes(x = time, y = temp_mean, color = site)) +
  geom_line(size = 1) +
  labs(x = "Year",
       y = "Instantaneous RF derived temperature (K)",
       color = "Site") +
  ylim(-3.0E-13,5E-13) + 
  theme_minimal()

# Wetland to cropland conversion - tests
CAEM1_crop <- GHGpertubationconversion(time1=-50, time2 = 500, pre_co2 = -39, pre_co2sd = sd(c(-55,-59,-4)), pre_ch4 = 3.9, pre_ch4sd = sd(c(3.7,3.3,4.3)),
                                  pst_co2 = -10, pst_co2sd = sd(c(-55,-59,-4)), pst_ch4 = 0, pst_ch4sd = sd(c(3.7,3.3,4.3)), type = "ins", runs=100)

CAEM2_crop <- GHGpertubationconversion(time1=-50, time2 = 500, pre_co2 = -209, pre_co2sd=sd(c(-233,-174,-217)),pre_ch4 = 0.4, pre_ch4sd =sd(c(0.3,0.5,0.6)),
                                  pst_co2 = -10, pst_co2sd = sd(c(-233,-174,-217)), pst_ch4 = 0, pst_ch4sd = sd(c(0.3,0.5,0.6)), type = "ins", runs=100)


CAEM3_crop <- GHGpertubationconversion(time1=-50, time2 = 500, pre_co2 = -205, pre_co2sd=sd(c(-233,-174,-217)), pre_ch4 = 16.2, pre_ch4sd =sd(c(3.7,3.3,4.3)), #sd based on the other sites for now
                                  pst_co2 = -10, pst_co2sd = sd(c(-233,-174,-217)), pst_ch4 = 0, pst_ch4sd = sd(c(3.7,3.3,4.3)), type = "ins", runs=100)

# Combine data frames
CAEM1_crop$site <- "CA-EM1"
CAEM2_crop$site <- "CA-EM2"
CAEM3_crop$site <- "CA-EM3"

df_comb_crop <- rbind(CAEM1_crop, CAEM2_crop, CAEM3_crop)

ggplot(df_comb_crop, aes(x = time, y = temp_mean, color = site)) +
  geom_line(size = 1) +
  labs(x = "Year",
       y = "Instantaneous RF derived temperature (K)",
       color = "Site") +
  ylim(-3.0E-13,5E-13) + 
  theme_minimal()