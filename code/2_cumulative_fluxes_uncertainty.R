library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(here)
library(ggpubr)

# Clean up figures!

# Explore 30 min data
load(here("output/30min_data.Rda"))

# DEFINE ANNUAL PERIODS (June-June of each year)
# Young
year1_s_Young <- which(data$datetime == as.POSIXct("2021-06-01 00:30:00",tz = 'UTC') & data$site == 'Young')
year1_e_Young <- which(data$datetime == as.POSIXct("2022-06-01 00:00:00",tz = 'UTC') & data$site == 'Young')

year2_s_Young <- which(data$datetime == as.POSIXct("2022-06-01 00:30:00",tz = 'UTC') & data$site == 'Young')
year2_e_Young <- which(data$datetime == as.POSIXct("2023-06-01 00:00:00",tz = 'UTC') & data$site == 'Young')

# Hogg
year1_s_Hogg <- which(data$datetime == as.POSIXct("2021-06-01 00:30:00",tz = 'UTC') & data$site == 'Hogg')
year1_e_Hogg <- which(data$datetime == as.POSIXct("2022-06-01 00:00:00",tz = 'UTC') & data$site == 'Hogg')

year2_s_Hogg <- which(data$datetime == as.POSIXct("2022-06-01 00:30:00",tz = 'UTC') & data$site == 'Hogg')
year2_e_Hogg <- which(data$datetime == as.POSIXct("2023-06-01 00:00:00",tz = 'UTC') & data$site == 'Hogg')

data$year_ann <- NA
data$year_ann[year1_s_Young:year1_e_Young] <- 'Year1'
data$year_ann[year2_s_Young:year2_e_Young] <- 'Year2'
data$year_ann[year1_s_Hogg:year1_e_Hogg] <- 'Year1'
data$year_ann[year2_s_Hogg:year2_e_Hogg] <- 'Year2'

# Group by site & Consider only Year1 for now
data.site <- data[which(data$year_ann == 'Year1'),] 

# Cumulative plots
# Color palette
colors_sites <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                  '#257ABA', '#C9B826') 

data.site.cum_sum <- data.site %>% group_by(site) %>% 
  mutate(NEE_PI_F_MDS_cum_sum = cumsum(NEE_PI_F_MDS*12.01*60*30/(10^6)),
         FCH4_PI_F_RF_cum_sum = cumsum(FCH4_PI_F_RF*12.01*60*30/(10^9)),  
         GPP_PI_F_DT_cum_sum = cumsum(GPP_PI_F_DT*12.01*60*30/(10^6)),
         GPP_PI_F_NT_cum_sum = cumsum(GPP_PI_F_NT*12.01*60*30/(10^6)),
         Reco_PI_F_DT_cum_sum = cumsum(Reco_PI_F_DT*12.01*60*30/(10^6)),
         Reco_PI_F_NT_cum_sum = cumsum(Reco_PI_F_NT*12.01*60*30/(10^6)))

vars <- c("NEE_PI_F_MDS_cum_sum","FCH4_PI_F_RF_cum_sum","GPP_PI_F_DT_cum_sum",
          "GPP_PI_F_NT_cum_sum","Reco_PI_F_DT_cum_sum","Reco_PI_F_NT_cum_sum")
nvars <- length(vars)
plots_cum <- plot.new()
ylabel <- c(expression(paste("FC ", "(gC ","m"^"-2",")")),expression(paste("FCH4 ", "(gC ","m"^"-2",")")),
            expression(paste("GPP DT ", "(gC ","m"^"-2",")")),expression(paste("GPP NT ", "(gC ","m"^"-2",")")),
            expression(paste("RECO DT ", "(gC ","m"^"-2",")")),expression(paste("RECO NT ", "(gC ","m"^"-2",")")))
subplot_label <- c("(a)","(b)","(c)","(d)","(e)","(f)")
ypos <- c(50,6,750,750,600,700)
# Loop through each variables
for (i in 1:(nvars)){
  
  var <- vars[[i]]
  # Plot time series
  p <- ggplot() +
    geom_line(data = data.site.cum_sum, aes(.data[["datetime"]], .data[[var]], color = factor(.data[["site"]])), size = 0.5)+
    theme_classic()+
    scale_color_manual(breaks = c('Hogg','Young'),values=colors_sites[c(2,9)])+
    labs(color='Site')+xlab('')+ylab(ylabel[[i]])+ 
    theme(text = element_text(size = 7),axis.text = element_text(size = 8),plot.margin = margin(0, 0.2, 0, 0.2, "cm"))+  
    annotate(geom="text", x=data.site.cum_sum[["datetime"]][1], y=ypos[[i]], label=subplot_label[[i]],size = 3)
  
  plots_cum[[i]] <- p
}
plots_cum[[1]] <- plots_cum[[1]]+geom_hline(yintercept=0, linetype="dashed")
p1 <- ggarrange(plotlist=plots_cum,ncol = 1,
                nrow = 6, common.legend = TRUE,hjust = -3.5) # FIX UP FIGURE
p1
ggsave("figures/flux_cum_sum.png", p1,units = "cm",height = 12, width = 12, dpi = 320)

# Annual sums
data.site.annual <- data.site %>% group_by(site) %>% 
  summarise(NEE = sum(NEE_PI_F_MDS*12.01*60*30/(10^6), na.rm = TRUE),
            FCH4 = sum(FCH4_PI_F_RF*12.01*60*30/(10^9), na.rm = TRUE),
            GPP_DT = sum(GPP_PI_F_DT*12.01*60*30/(10^6), na.rm = TRUE),
            GPP_NT = sum(GPP_PI_F_NT*12.01*60*30/(10^6), na.rm = TRUE),
            Reco_DT = sum(Reco_PI_F_DT*12.01*60*30/(10^6), na.rm = TRUE),
            Reco_NT = sum(Reco_PI_F_NT*12.01*60*30/(10^6), na.rm = TRUE))

data.site.annual$GHG <- (data.site.annual$NEE+data.site.annual$FCH4)*44.01/12.011+data.site.annual$FCH4*16.04/12.011*45
data.site.annual

data.site.annual[,c(2:length(data.site.annual))] <- round(data.site.annual[,c(2:length(data.site.annual))])

save(data.site.annual,file="output/annual_sums.Rda")

# Error bars
