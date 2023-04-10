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
library(psych)
library(REdaS)
library(vegan)
library(reshape)
library(rstatix)
library(multcompView)

# Explore 30 min data
load(here("output/30min_data.Rda"))

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
load(here("output/daily_data.Rda"))

# Trim data frame to start on June 1, 2021 
ind_s_Young <- which(data.daily$datetime == as.POSIXct("2021-06-01",tz = 'UTC') & data.daily$site == 'Young')
ind_Young <- which(data.daily$site == 'Young' & data.daily$year < 2023)
ind_Young_last <- ind_Young[length(ind_Young)]

ind_s_Hogg <- which(data.daily$datetime == as.POSIXct("2021-06-01",tz = 'UTC') & data.daily$site == 'Hogg')
ind_Hogg <- which(data.daily$site == 'Hogg' & data.daily$year < 2023)
ind_Hogg_last <- ind_Hogg[length(ind_Hogg)]

data.daily <- data.daily[c(ind_s_Hogg:ind_Hogg_last,ind_s_Young:ind_Young_last), ] 

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
  geom_line(data = data.daily, aes(x = datetime, y = H, color = as.factor(site)))
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.daily, aes(x = datetime, y = LE, color = as.factor(site)))
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.daily, aes(x = datetime, y = FCH4_gC, color = as.factor(site)))
toWebGL(ggplotly(p))

p <- ggplot() +
  geom_line(data = data.daily, aes(x = datetime, y = WTD, color = as.factor(site)))
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

# water quality data

# Specify variables (only one year for each of these at the moment)
vars <- c('pH','Specific_cond','DOC','TDN','NO3_NO2_N','NH4_N','DRP','TDP','TP','ABS_280nm')
nvars <- length(vars)

# Create empty plot
plots_ts <- plot.new()
plots_bp <- plot.new()

# Loop through each variables
for (i in 1:nvars){
  
  var <- vars[[i]]
  # Plot time series
  p <- ggplot() +
    geom_point(data = data.daily, aes(.data[["datetime"]], .data[[var]], color = factor(.data[["site"]])), size = 1)
  toWebGL(ggplotly(p))
  
  plots_ts[[i]] <- p
  
  p <- ggplot(data.daily, aes(as.factor(.data[["site"]]), .data[[var]])) + 
    geom_boxplot(lwd=0.2,outlier.size=0.3) + xlab('') +
    geom_signif(comparisons = list(c("Hogg", "Young")), na.rm = TRUE,
                map_signif_level=TRUE, tip_length = 0,textsize=1.5,size = 0.2, 
                y_position = max(data.daily[[var]],na.rm = T)-0.1*(max(data.daily[[var]],na.rm = T)-min(data.daily[[var]],na.rm = T))) + 
    theme(text = element_text(size = 6))
  
  plots_bp[[i]] <- p
}

p <- ggarrange(plotlist=plots_bp)

ggsave("figures/WQ_bp.png", p,units = "cm",height = 8, width = 12, dpi = 320)

# FOR SO4 only since only variable with multiple years of observations (for now!)
var <- c('SO4')

p_ts_SO4 <- ggplot() +
  geom_point(data = data.daily, aes(.data[["datetime"]], .data[[var]], color = factor(.data[["site"]])), size = 1)
toWebGL(ggplotly(p_ts_SO4))

p_bp_SO4 <- ggplot(data.daily, aes(as.factor(.data[["year"]]), .data[[var]],fill = .data[["site"]])) + 
  geom_boxplot() + xlab('') + theme_bw() + theme(text = element_text(size = 6))
p_bp_SO4

ggsave("figures/SO4_bp.png", p_bp_SO4,units = "cm",height = 5, width = 6, dpi = 320)

# Principle component analysis on WQ parameters
library(corrplot)
library(factoextra)

#1. Extra WQ parameters
vars <- c('site','FCH4_gC','pH','SO4','Specific_cond','DOC','TDN','NO3_NO2_N','NH4_N','DRP','TDP','TP','ABS_280nm')

# Extract only variables of interest and non-NA data
data.PCA.all <- na.omit(data.daily[,vars])
data.PCA <- data.PCA.all[, -c(1:2)]

pairs.panels(
  data.PCA,
  main = "WQ vars",
  gap = 0, # set to zero for no gap between plot panels
  lm = TRUE, # draw linear regression lines for pairs
  stars = TRUE, # display significance
  bg = c("red", "blue")[as.factor(data.PCA.all$site)], # color based on site
  pch = 21) # data point shape

# Look at correlation between variables - CHECK PCA Assumptions!
corrplot(cor(data.PCA))

# Bartlett's Test Of Sphericity (https://swampthingecology.org/blog/pca-basics-in-rstats/)
bart_spher(data.PCA)
KMOS(data.PCA)

# The data.PCA dataset appears to be suitable for factor analysis. The KMO value for the entire dataset is above the suggested 0.5 threshold. 
# And based on Sphericity test (bart_spher()) the results looks good to move forward with a PCA analysis. 

# Compute PCA (from http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/)
res.pca <- prcomp(data.PCA, center = TRUE, scale. = TRUE)

# “loadings” of a PCA. (https://rpubs.com/carabidus/465971)
pca_loadings <- res.pca$rotation
kable(pca_loadings, caption = "PCA Loadings") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F)

# Orthogonality of PCs
pairs.panels(
  res.pca$x,
  main = "PCA Correlation Summary",
  gap = 0, # set to zero for no gap between plot panels
  lm = TRUE, # draw linear regression lines for pairs
  stars = TRUE, # display significance
  bg = c("red", "blue")[as.factor(data.PCA.all$site)], # color based on sex 
  pch = 21) # data point shape

#Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_eig(res.pca)

pca_summary <- summary(res.pca)$importance %>%
  as.data.frame()
kable(pca_summary, caption = "PCA Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)

groups <- as.factor(data.PCA.all$site)
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "grey33", # Variables color
                #pointsize = data.PCA.all$FCH4_gC*1000,
                pointshape = 21,
                fill.ind = groups,
                label = "var",
                legend.title = list(fill = "Site", size = "FCH4 (mgC m-2 d-1)"),
                title = ""
)

p <- ggplot(data.daily, aes(.data[["site"]], .data[[var]])) + 
  geom_boxplot() + xlab('') +
  geom_signif(comparisons = list(c("Hogg", "Young")), na.rm = TRUE,
              map_signif_level=TRUE, tip_length = 0) + theme(text = element_text(size = 18))
p

t.test(data.daily[[var]]~data.daily[["site"]])
lm <- lm(data.daily[[var]]~data.daily[["site"]])
summary(lm)

p <- ggplot(data.daily, aes(as.factor(.data[["year"]]), .data[[var]],fill = .data[["site"]])) + 
  geom_boxplot() + xlab('') +
  geom_signif()+
  geom_signif(comparisons = list(c("2021", "2022")), na.rm = TRUE,
              map_signif_level=TRUE, tip_length = 0) + theme(text = element_text(size = 18))
p

lm <- lm(data.daily[[var]]~data.daily[["site"]]*data.daily[["year"]])  
summary(lm)

# Two-way anova (http://www.sthda.com/english/wiki/two-way-anova-test-in-r OR https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/)
var <- 'SO4'

data.daily %>%
  group_by(site, year) %>%
  get_summary_stats(SO4, type = "mean_sd")

# NOTE: SEE Compute two-way ANOVA test in R for UNBALANCED designs!

# Test for normality - Normally distributed as p > 0.05
# (https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/)
data.daily %>%
  group_by(site, year) %>%
  shapiro_test(SO4)

# but check outliers further
ggqqplot(data.daily, "SO4", ggtheme = theme_bw()) +
  facet_grid(site ~ year, labeller = "label_both")

# Computation (used http://www.sthda.com/english/wiki/two-way-anova-test-in-r#compute-two-way-anova-test-in-r-balanced-designs)
data.daily.SO4 <- na.omit(data.daily[,c('site','year','SO4')])

res.aov2 <- aov(data.daily.SO4[[var]]~data.daily.SO4[["site"]]*data.daily.SO4[["year"]])
summary(res.aov2)

group_by(data.daily.SO4, site, year) %>%
  summarise(
    count = n(),
    mean = mean(SO4, na.rm = TRUE),
    sd = sd(SO4, na.rm = TRUE)
  )

# Tukey multiple pairwise-comparisons
# Also see: https://statdoe.com/two-way-anova-in-r/#comparing-means-by-tukeys-test
tukey <- TukeyHSD(res.aov2)

tukey.cld <- multcompLetters4(res.aov2, tukey)
print(tukey.cld)

# Check assumptions (http://www.sthda.com/english/wiki/two-way-anova-test-in-r#compute-two-way-anova-test-in-r-balanced-designs)
# 1. Homogeneity of variances
plot(res.aov2, 1)

#Use the Levene’s test to check the homogeneity of variances. The function leveneTest() [in car package] will be used:
leveneTest(SO4 ~ site*year, data = data.daily.SO4)

# NOTE: From the output above we can see that the p-value IS less than the significance level of 0.05. 
# This means that there is evidence to suggest that the variance across groups is statistically significantly different. 
# Therefore, we CAN'T assume the homogeneity of variances in the different treatment groups.

# 2. Normality
plot(res.aov2, 2)
# Points don't fall approximately along this reference line, we CAN'T assume normality.

# Explore linear model - simple linear regression
data.daily.na.omit <- na.omit(data.daily[,c("FCH4_gC","TA","WTD","SO4_interp","site")]) 

lm <- lmer(FCH4_gC~TA^2+WTD+SO4_interp,data=data.daily.na.omit)  
summary(lm)

ggplot(data.daily.na.omit, aes(x=predict(lm), y= data.daily.na.omit$FCH4_gC)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) + ylim(-0.01,0.08)+ xlim(-0.01,0.08)+
  labs(x='Predicted Values', y='Actual Values')





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