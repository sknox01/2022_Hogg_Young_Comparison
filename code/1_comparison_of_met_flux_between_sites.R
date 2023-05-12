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
library(ggpubr)
library(cowplot)
library(emmeans)
library(multcomp)
library(kableExtra)
library(factoextra)
library(rstatix)
library(car)
library(ggplotify)

# CREATE FUNCTIONS!!

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

# Figures in body of the manuscript
# Color palette
colors_sites <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                  '#257ABA', '#C9B826') 

# ------------------------------------------------------------------------------------------------
# Water Quality Data

# Table summary of WQ data
vars <- c('pH','SO4','Specific_cond','DOC','TDN','NO3_NO2_N','NH4_N','DRP','TDP','TP','ABS_280nm')

data.daily.WQ.mean <- data.daily %>%
  group_by(site,year) %>%
  summarise_at(vars(vars), funs(mean = mean(.,na.rm=TRUE)))

vars_names <- c("site","year",vars) # UPDATE LATER AND INCLUDE UNITS
colnames(data.daily.WQ.mean) <- vars_names

data.daily.WQ.mean[,vars] <- round(data.daily.WQ.mean[,vars])

save(data.daily.WQ.mean,file="output/daily_WQ_mean.Rda")

# PCA of WQ parameters 
# 1. Extra WQ parameters 
vars <- c('site','FCH4_gC','pH','SO4','Specific_cond','DOC','TDN','NO3_NO2_N','NH4_N','DRP','TDP','TP','ABS_280nm')

# Extract only variables of interest and non-NA data
data.PCA.all <- na.omit(data.daily[,vars])
data.PCA <- data.PCA.all[, -c(1:2)]

# 2. Check assumptions. Bartlett's Test Of Sphericity (https://swampthingecology.org/blog/pca-basics-in-rstats/)
bart_spher(data.PCA)
KMOS(data.PCA)

# The data.PCA dataset appears to be suitable for factor analysis. The KMO value for the entire dataset is above the suggested 0.5 threshold. 
# And based on Sphericity test (bart_spher()) the results looks good to move forward with a PCA analysis. 

# 3. Compute PCA (from http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/)
res.pca <- prcomp(data.PCA, scale = TRUE) # scale can both center and scale

# 4. Plot biplot (UPDATE COLORS & better format figure)
var_explained <- res.pca$sdev ^ 2 / sum(res.pca$sdev ^ 2)
groups <- as.factor(data.PCA.all$site)

p <- fviz_pca_biplot(res.pca, repel = TRUE,
                     col.var = "grey33", # Variables color
                     #pointsize = data.PCA.all$FCH4_gC*1000,
                     pointshape = 21,
                     fill.ind = groups,
                     palette = colors_sites[c(2,9)],
                     label = "var",
                     addEllipses = F,
                     legend.title = list(fill = "Site", size = "FCH4 (mgC m-2 d-1)"),
                     invisible = "quali",
                     labelsize = 2,
                     title = "") + 
  geom_hline(yintercept=0, color='gray70', size=0.3,linetype="dashed") +
  geom_vline(xintercept=0, color='gray70', size=0.3,linetype="dashed") + 
  theme(text = element_text(size = 8),legend.position="bottom")+
  labs(x = paste(paste("PC 1 (", round(var_explained[1]*100,1)),"%)", sep = ""), y = paste(paste("PC 2 (", round(var_explained[2]*100,1)),"%)", sep = ""))
p

ggsave("figures/PCA_WQ.png", p,units = "cm",height = 8, width = 6, dpi = 320)

# Figures for the SI 

# Box plots of WQ parameters

# Specify variables (only one year for each of these at the moment)
vars <- c('DOC','TDN','NO3_NO2_N','NH4_N','ABS_280nm')
nvars <- length(vars)

# Checking distribution of the water quality variables. 
# 1) qq plots
plots_qq <- plot.new()

for (i in 1:nvars){
  
  var <- vars[[i]]
  p <- ggqqplot(na.omit(data.daily), var, ggtheme = theme_bw(),title = var) +
    facet_grid(site ~ year, labeller = "label_both")
  
  plots_qq[[i]] <- p
}

p <- ggarrange(plotlist=plots_qq)
p

# 2) shapiro test. Normally distributed if p > 0.05. Note that most ARE normally distributed
df <- na.omit(data.daily[,c("site","year",vars)])%>%
  group_by(site, year)

df.dist <- list()
for (i in 1:nvars){
  st <- df%>%dplyr::rename("var" = vars[i]) %>%shapiro_test(var)
  st$variable <- c(vars[i],vars[i])
  df.dist[[i]] <- st
}

# Test for homogeneity of variances - p > 0.05 means no significant difference between the two variables. Generally this assumption isn't met
df.hv <- list()
for (i in 1:nvars){
  df.hv[[i]] <- df%>%dplyr::rename("var" = vars[i]) %>%var.test(var ~ site, data = .)
  df.hv[[i]]$data.name <- paste0(vars[i]," by site",sep = "")
}

# Create empty plot
plots_bp <- plot.new()

# Loop through each variables - CHECK NA.OMIT once we have all the data
for (i in 1:nvars){
  
  var <- vars[[i]]
  
  p <- ggplot(na.omit(data.daily), aes(as.factor(.data[["site"]]), .data[[var]])) + 
    geom_boxplot(lwd=0.2,outlier.size=0.3) + xlab('') +
    geom_signif(comparisons = list(c("Hogg", "Young")), na.rm = TRUE, test = "wilcox.test",
                map_signif_level=TRUE, tip_length = 0,textsize=1.5,size = 0.2, 
                y_position = max(data.daily[[var]],na.rm = T)-0.1*(max(data.daily[[var]],na.rm = T)-min(data.daily[[var]],na.rm = T))) + 
    theme(text = element_text(size = 6))
  
  plots_bp[[i]] <- p
}

p <- ggarrange(plotlist=plots_bp)

# Check if it should be a t-test or Wilcoxon test
# The results of these experiments indicate that Student’s t-test should definitely be avoided 
# for sample sizes smaller than 20 (https://www.datascienceblog.net/post/statistical_test/parametric_sample_size/)
# See also https://www.datascienceblog.net/post/statistical_test/signed_wilcox_rank_test/

# Since we have a small sample size, I went with Wilcoxon test, but since the data are generally normally distributed could go with a t-test. 
# ASK CO-AUTHORS
W_test <- list()
for (i in 1:nvars){
  
  var <- vars[[i]]
  W_test[[i]] <- wilcox.test(na.omit(data.daily)[[var]]~na.omit(data.daily)[["site"]])
}

ggsave("figures/WQ_bp.png", p,units = "cm",height = 8, width = 12, dpi = 320)

# For sites with variables for both years 

# Specify variables (only one year for each of these at the moment)
vars <- c('pH','Specific_cond','DRP','TDP','TP','SO4','SO4_pred')
nvars <- length(vars)

# Checking distribution of the water quality variables. 
# 1) qq plots
plots_qq <- plot.new()

for (i in 1:nvars){
  
  var <- vars[[i]]
  p <- ggqqplot(na.omit(data.daily), var, ggtheme = theme_bw(),title = var) +
    facet_grid(site ~ year, labeller = "label_both")
  
  plots_qq[[i]] <- p
}

p <- ggarrange(plotlist=plots_qq)
p

# 2) shapiro test. Normally distributed if p > 0.05. Note that most ARE normally distributed
df <- na.omit(data.daily[,c("site","year",vars)])%>%
  group_by(site) # removed by year here. But should it be included?

df.dist <- list()
for (i in 1:nvars){
  dist <- df%>%dplyr::rename("var" = vars[i]) %>%shapiro_test(var)
  dist$variable <- c(vars[i],vars[i])
  df.dist[[i]] <- dist
}

# Test for homogeneity of variances - p > 0.05 means no significant difference between the two variances. Generally assumption NOT met.
df.hv <- list()
for (i in 1:nvars){
  df.hv[[i]] <- df%>%dplyr::rename("var" = vars[i]) %>%var.test(var ~ site, data = .)
  df.hv[[i]]$data.name <- paste0(vars[i]," by site",sep = "")
}

# Create empty plot
plots_bp <- plot.new()

df <- na.omit(data.daily[,c('site','year',vars)])
df$site <- as.factor(df$site)
df$year <- as.factor(df$year)

# Loop through each variables - CHECK NA.OMIT once we have all the data
for (i in 1:nvars){
  
  var <- vars[[i]]
  
  p1 <- ggboxplot(
    df, x = 'site', y = var, fill = 'year',
    palette = colors_sites[c(2,9)],
    lwd=0.2,
    outlier.size=0.3
  )+theme(text = element_text(size = 6))
  p1 
  
  plots_bp[[i]] <- p1
}

p1 <- ggarrange(plotlist=plots_bp)

# Using the Kruskal-Wallis Test, we can decide whether the population distributions are identical without assuming them to follow the normal distribution.
# https://www.cfholbert.com/blog/nonparametric_two_way_anova/

# One method that is not unduly affected by violations of the normality and homoscedasticity assumptions of parametric ANOVA is to transform the data values to their ranks, and then compute a parametric two-way ANOVA on the data ranks.
# This rank transformation procedure can be used to determine whether the ranks differ from group to group. Post-hoc comparisons can be performed on the data ranks to determine which groups are different from each other. 

# Finally, let’s perform two-way ANOVA on the rank-transformed data. 
# Ranking is one of many procedures used to transform data that do not meet the assumptions of normality. 
# Conover and Iman (1981) provided a review of the four main types of rank transformations. 
# One method replaces each original data value by its rank (from 1 for the smallest to N for the largest, where N is the combined data sample size). 
# This rank-based procedure has been recommended as being robust to non-normal errors, resistant to outliers, and highly efficient for many distributions.

# ANOVA procedure on rank-transformed data
anova.rnk <- list()
aov.rnk <- list()
lt.rnk <- list()
dist.rnk <- list()
for (i in 1:nvars){
  
  var <- vars[[i]]
  
  aov.rnk[[i]] <- df%>%dplyr::rename("var" = vars[i]) %>% aov(
    rank(var) ~ year * site, data = .,
    contrasts = list(
      site = 'contr.sum',
      year = 'contr.sum'
    )
  )
  anova.rnk[[i]] <- Anova(aov.rnk[[i]], type = 'III')   
  
  #Use the Levene’s test to check the homogeneity of variances. The function leveneTest() [in car package] will be used:
  # NOTE: if p < 0.05. This means that there is evidence to suggest that the variance across groups is statistically significantly different. 
  # Therefore, we CAN'T assume the homogeneity of variances in the different treatment groups. 
  # if p > 0.05 CAN assume homogeneity of variances
  lt.rnk[[i]] <- df%>%dplyr::rename("var" = vars[i])%>%
    leveneTest(rank(var) ~ year * site, data = .)
  
  # Rename intercept to variable name
  rownames(anova.rnk[[i]])[1] <- vars[i]
  
  # Test for normality. Normally distributed if p > 0.05
  dist <- df%>%dplyr::rename("var" = vars[i]) %>%shapiro_test(var)
  dist$variable <- c(vars[i],vars[i],vars[i],vars[i])
  dist.rnk[[i]] <- dist
}

# Let’s inspect the residuals from each model for 1) Homogeneity of variances and 2) normality. Tests are above.
plots_hv <- plot.new()
plots_qq <- plot.new()
for (i in 1:nvars){
  
  var <- vars[[i]]  
  res.rnk = aov.rnk[[i]]$resid
  
  p <- as.ggplot(~(qqnorm(
    res.rnk, pch = 20, main = paste("Rank-Transformed ",var,sep=""),
    cex.lab = 1, cex.axis = 0.7, cex.main = 1
  )))
  plots_qq[[i]] <- p
  
  p1 <- as.ggplot(~plot(aov.rnk[[i]], 1, main = paste("Rank-Transformed ",var,sep="")))
  plots_hv[[i]] <- p1
}

# ------------------------------------------------------------------------------------------------
# Environmental variables - daily data 

# Plot of time series
vars <- c("PPFD_IN","TA","VPD","WTD","P")
nvars <- length(vars)
plots_ts <- plot.new()
ylabel <- c(expression(paste("PPFD ", "(\u00B5mol ","m"^"-2", " d"^"-1",")")),'TA (\u00B0C)','VPD (hPa)','WTD (cm)',expression(paste("P ", "(mm", " d"^"-1",")")))
subplot_label <- c("(a)","(b)","(c)","(d)","(e)")
ypos <- c(600,20,25,700,60)
# Loop through each variables
for (i in 1:(nvars)){
  
  var <- vars[[i]]
  # Plot time series
  p <- ggplot() +
    geom_line(data = data.daily, aes(.data[["datetime"]], .data[[var]], color = factor(.data[["site"]])), size = 0.5)+
    theme_classic()+
    scale_color_manual(breaks = c('Hogg','Young'),values=colors_sites[c(2,9)])+
    labs(color='Site')+xlab('')+ylab(ylabel[[i]])+ 
    theme(text = element_text(size = 7),axis.text = element_text(size = 8),plot.margin = margin(0, 0.2, 0, 0.2, "cm"))+  
    annotate(geom="text", x=data.daily[["datetime"]][nrow(data.daily)-10], y=ypos[[i]], label=subplot_label[[i]],size = 3)
  
  plots_ts[[i]] <- p
}

p1 <- ggarrange(plotlist=plots_ts,ncol = 1,
                nrow = 5, common.legend = TRUE,hjust = -3.5)
p1
ggsave("figures/Met_ts.png", p1,units = "cm",height = 12, width = 12, dpi = 320)

# Focus on GS for now (use only data when WQ data is available)
# use the period of June 1 to Oct 15 which is the period common to both sites for both years where WQ is also available
ind_GS <- which(((data.daily$datetime <"2021-06-01 UTC" | data.daily$datetime >"2021-10-15 UTC") & data.daily$year == 2021)|
                  ((data.daily$datetime <"2022-06-01 UTC" | data.daily$datetime > "2022-10-15 UTC") & data.daily$year == 2022))

data.daily.GS <- data.daily
data.daily.GS[ind_GS,!names(data.daily.GS) %in% c("year","site","datetime")] <- NA

data.annual.met.GS <- data.daily.GS %>% # Compare results in using 30 min data. Why aren't they exactly the same....
  group_by(site,year) %>%
  dplyr::summarize(TA = mean(TA,na.rm = TRUE),
                   VPD = mean(VPD,na.rm = TRUE),
                   PPFD_IN = mean(PPFD_IN,na.rm = TRUE), # Update units later
                   P = sum(P,na.rm = TRUE),
                   WTD =  mean(WTD,na.rm = TRUE)) # Make sure measurement length is the same for both datasets

save(data.annual.met.GS,file="output/daily_Met_mean.Rda") 

# Specify variables (only one year for each of these at the moment)
vars <- c('TA','VPD','PPFD_IN','P','WTD')
nvars <- length(vars)

# Checking distribution of the water quality variables. 
# 1) qq plots
plots_qq <- plot.new()

for (i in 1:nvars){
  
  var <- vars[[i]]
  p <- ggqqplot(na.omit(data.daily.GS), var, ggtheme = theme_bw(),title = var) +
    facet_grid(site ~ year, labeller = "label_both")
  
  plots_qq[[i]] <- p
}

p <- ggarrange(plotlist=plots_qq)
p

# 2) shapiro test. Normally distributed if p > 0.05. Note that most are NOT normally distributed
df <- na.omit(data.daily.GS[,c("site","year",vars)])%>%
  group_by(site) # removed by year here. But should it be included?

df.dist <- list()
for (i in 1:nvars){
  dist <- df%>%dplyr::rename("var" = vars[i]) %>%shapiro_test(var)
  dist$variable <- c(vars[i],vars[i])
  df.dist[[i]] <- dist
}

# Test for homogeneity of variances - p > 0.05 means no significant difference between the two variables. Generally assumption IS met.
df.hv <- list()
for (i in 1:nvars){
  df.hv[[i]] <- df%>%dplyr::rename("var" = vars[i]) %>%var.test(var ~ site, data = .)
  df.hv[[i]]$data.name <- paste0(vars[i]," by site",sep = "")
}

# Create empty plot
plots_bp <- plot.new()

df <- na.omit(data.daily[,c('site','year',vars)])
df$site <- as.factor(df$site)
df$year <- as.factor(df$year)

# Loop through each variables - CHECK NA.OMIT once we have all the data
for (i in 1:nvars){
  
  var <- vars[[i]]
  
  p1 <- ggboxplot(
    df, x = 'site', y = var, fill = 'year',
    palette = colors_sites[c(2,9)],
    lwd=0.2,
    outlier.size=0.3
  )+theme(text = element_text(size = 6))
  p1 
  
  plots_bp[[i]] <- p1
}

p1 <- ggarrange(plotlist=plots_bp)

ggsave("figures/Met_bp.png", p1,units = "cm",height = 8, width = 12, dpi = 320)

# Using the Kruskal-Wallis Test, we can decide whether the population distributions are identical without assuming them to follow the normal distribution.
# https://www.cfholbert.com/blog/nonparametric_two_way_anova/

# One method that is not unduly affected by violations of the normality and homoscedasticity assumptions of parametric ANOVA is to transform the data values to their ranks, and then compute a parametric two-way ANOVA on the data ranks.
# This rank transformation procedure can be used to determine whether the ranks differ from group to group. Post-hoc comparisons can be performed on the data ranks to determine which groups are different from each other. 

# Finally, let’s perform two-way ANOVA on the rank-transformed data. 
# Ranking is one of many procedures used to transform data that do not meet the assumptions of normality. 
# Conover and Iman (1981) provided a review of the four main types of rank transformations. 
# One method replaces each original data value by its rank (from 1 for the smallest to N for the largest, where N is the combined data sample size). 
# This rank-based procedure has been recommended as being robust to non-normal errors, resistant to outliers, and highly efficient for many distributions.

# ANOVA procedure on rank-transformed data and regular data
anova.rnk <- list()
anova.org <- list()
aov.rnk <- list()
aov.org <- list()
lt.rnk <- list()
dist.rnk <- list()
for (i in 1:nvars){
  
  var <- vars[[i]]
  
  aov.rnk[[i]] <- df%>%dplyr::rename("var" = vars[i]) %>% aov(
    rank(var) ~ year * site, data = .,
    contrasts = list(
      site = 'contr.sum',
      year = 'contr.sum'
    )
  )
  anova.rnk[[i]] <- Anova(aov.rnk[[i]], type = 'III') # Really similar to regular anova below!
  
  # Let’s first perform two-way ANOVA on the original data.
  aov.org[[i]] <- df%>%dplyr::rename("var" = vars[i]) %>% aov(
    var ~ year * site, data = .,
    contrasts = list(
      site = 'contr.sum',
      year = 'contr.sum'
    )
  )
  
  anova.org[[i]] <- Anova(aov.org[[i]], type = 'III')
  
  #Use the Levene’s test to check the homogeneity of variances. The function leveneTest() [in car package] will be used:
  # NOTE: if p < 0.05. This means that there is evidence to suggest that the variance across groups is statistically significantly different. 
  # Therefore, we CAN'T assume the homogeneity of variances in the different treatment groups. 
  # if p > 0.05 CAN assume homogeneity of variances
  lt.rnk[[i]] <- df%>%dplyr::rename("var" = vars[i])%>%
    leveneTest(rank(var) ~ year * site, data = .)
  
  # Rename intercept to variable name
  rownames(anova.rnk[[i]])[1] <- vars[i]
  
  # Test for normality. Normally distributed if p > 0.05
  dist <- df%>%dplyr::rename("var" = vars[i]) %>%shapiro_test(var)
  dist$variable <- c(vars[i],vars[i],vars[i],vars[i])
  dist.rnk[[i]] <- dist
}

# Let’s inspect the residuals from each model for 1) Homogeneity of variances and 2) normality. Tests are above.
plots_hv <- plot.new()
plots_qq <- plot.new()
for (i in 1:nvars){
  
  var <- vars[[i]]  
  res.rnk = aov.rnk[[i]]$resid
  
  p <- as.ggplot(~(qqnorm(
    res.rnk, pch = 20, main = paste("Rank-Transformed ",var,sep=""),
    cex.lab = 1, cex.axis = 0.7, cex.main = 1
  )))
  plots_qq[[i]] <- p
  
  p1 <- as.ggplot(~plot(aov.rnk[[i]], 1, main = paste("Rank-Transformed ",var,sep="")))
  plots_hv[[i]] <- p1
}

# ------------------------------------------------------------------------------------------------
# Flux variables - daily data 
data.daily$FCH4_mgC <- data.daily$FCH4_gC*1000
# Plot of time series
vars <- c("FC_gC","FCH4_mgC","GPP_PI_F_DT_gC","GPP_PI_F_NT_gC","Reco_PI_F_DT_gC","Reco_PI_F_NT_gC")
nvars <- length(vars)
plots_ts <- plot.new()
ylabel <- c(expression(paste("FC ", "(gC ","m"^"-2", " d"^"-1",")")),expression(paste("FCH4 ", "(mgC ","m"^"-2", " d"^"-1",")")),
            expression(paste("GPP DT ", "(gC ","m"^"-2", " d"^"-1",")")),expression(paste("GPP NT ", "(gC ","m"^"-2", " d"^"-1",")")),
            expression(paste("RECO DT ", "(gC ","m"^"-2", " d"^"-1",")")),expression(paste("RECO NT ", "(gC ","m"^"-2", " d"^"-1",")")))
subplot_label <- c("(a)","(b)","(c)","(d)","(e)","(f)")
ypos <- c(10,10,10,10,10,10)
# Loop through each variables
for (i in 1:(nvars)){
  
  var <- vars[[i]]
  # Plot time series
  p <- ggplot() +
    geom_line(data = data.daily, aes(.data[["datetime"]], .data[[var]], color = factor(.data[["site"]])), size = 0.5)+
    theme_classic()+
    scale_color_manual(breaks = c('Hogg','Young'),values=colors_sites[c(2,9)])+
    labs(color='Site')+xlab('')+ylab(ylabel[[i]])+ 
    theme(text = element_text(size = 7),axis.text = element_text(size = 8),plot.margin = margin(0, 0.2, 0, 0.2, "cm"))+  
    annotate(geom="text", x=data.daily[["datetime"]][nrow(data.daily)-10], y=ypos[[i]], label=subplot_label[[i]],size = 3)
  
  plots_ts[[i]] <- p
}

p1 <- ggarrange(plotlist=plots_ts,ncol = 1,
                nrow = 6, common.legend = TRUE,hjust = -3.5) # FIX UP FIGURE
p1
ggsave("figures/flux_ts.png", p1,units = "cm",height = 12, width = 12, dpi = 320)

# Focus on GS for now (use only data when WQ data is available)
# use the period of June 1 to Oct 15 which is the period common to both sites for both years where WQ is also available
ind_GS <- which(((data.daily$datetime <"2021-06-01 UTC" | data.daily$datetime >"2021-10-15 UTC") & data.daily$year == 2021)|
                  ((data.daily$datetime <"2022-06-01 UTC" | data.daily$datetime > "2022-10-15 UTC") & data.daily$year == 2022))

data.daily.GS <- data.daily
data.daily.GS[ind_GS,!names(data.daily.GS) %in% c("year","site","datetime")] <- NA

data.annual.flux.GS <- data.daily.GS %>% # Compare results in using 30 min data. Why aren't they exactly the same....
  group_by(site,year) %>%
  dplyr::summarize(FC_gC = sum(FC_gC,na.rm = T),
                   FCH4_gC = sum(FCH4_gC,na.rm = T),
                   FCH4_gC = sum(FCH4_gC,na.rm = T),
                   GPP_PI_F_DT_gC = sum(GPP_PI_F_DT_gC,na.rm = T),
                   GPP_PI_F_NT_gC = sum(GPP_PI_F_NT_gC,na.rm = T),
                   Reco_PI_F_DT_gC = sum(Reco_PI_F_DT_gC,na.rm = T),
                   Reco_PI_F_NT_gC = sum(Reco_PI_F_NT_gC,na.rm = T))

save(data.annual.flux.GS,file="output/daily_Flux_mean.Rda") 

# Compare partitioning approaches
df.Hogg <- data.daily.GS[which(data.daily.GS$site == "Hogg"),]
df.Young <- data.daily.GS[which(data.daily.GS$site == "Young"),]
df.partitioning.Hogg <- data.frame(c(df.Hogg$GPP_PI_F_DT_gC,df.Hogg$GPP_PI_F_NT_gC),
                                   c(df.Hogg$Reco_PI_F_DT_gC,df.Hogg$Reco_PI_F_NT_gC),
                                   c(rep("DT",length(df.Hogg$GPP_PI_F_DT_gC)),rep("NT",length(df.Hogg$GPP_PI_F_DT_gC))),
                                   c(df.Hogg$site,df.Hogg$site))
df.partitioning.Young <- data.frame(c(df.Young$GPP_PI_F_DT_gC,df.Young$GPP_PI_F_NT_gC),
                                    c(df.Young$Reco_PI_F_DT_gC,df.Young$Reco_PI_F_NT_gC),
                                    c(rep("DT",length(df.Young$GPP_PI_F_DT_gC)),rep("NT",length(df.Young$GPP_PI_F_DT_gC))),
                                    c(df.Young$site,df.Young$site))
colnames(df.partitioning.Hogg) <- c("GPP","RECO","Method","site")
colnames(df.partitioning.Young) <- c("GPP","RECO","Method","site")

# GPP
ggplot() +
  geom_point(data = data.daily.GS, aes(GPP_PI_F_NT_gC, GPP_PI_F_DT_gC, color = factor(site)))+
  theme_classic()

# RECO
ggplot() +
  geom_point(data = data.daily.GS, aes(Reco_PI_F_NT_gC, Reco_PI_F_DT_gC, color = factor(site)))+
  theme_classic()

# Manually changed df and variable name
aov.rnk <- df.partitioning.Hogg%>%aov(
  rank(RECO) ~ Method, data = .,
  contrasts = list(
    Method = 'contr.sum'
  )
)
anova.rnk <- Anova(aov.rnk, type = 'III') # Really similar to regular anova below!

# GPP & RECO calculated using the different partitioning approaches. Make sure to test BOTH in all analyses.

# Specify variables (only one year for each of these at the moment)

# Checking distribution of the variables. 
# 1) qq plots
plots_qq <- plot.new()

for (i in 1:nvars){
  
  var <- vars[[i]]
  p <- ggqqplot(na.omit(data.daily.GS), var, ggtheme = theme_bw(),title = var) +
    facet_grid(site ~ year, labeller = "label_both")
  
  plots_qq[[i]] <- p
}

p <- ggarrange(plotlist=plots_qq)
p

# 2) shapiro test. Normally distributed if p > 0.05. Note that most are NOT normally distributed
df <- na.omit(data.daily.GS[,c("site","year",vars)])%>%
  group_by(site) # removed by year here. But should it be included?

df.dist <- list()
for (i in 1:nvars){
  dist <- df%>%dplyr::rename("var" = vars[i]) %>%shapiro_test(var)
  dist$variable <- c(vars[i],vars[i])
  df.dist[[i]] <- dist
}

# Test for homogeneity of variances - p > 0.05 means no significant difference between the two variables. Generally assumption IS met.
df.hv <- list()
for (i in 1:nvars){
  df.hv[[i]] <- df%>%dplyr::rename("var" = vars[i]) %>%var.test(var ~ site, data = .)
  df.hv[[i]]$data.name <- paste0(vars[i]," by site",sep = "")
}

# Create empty plot
plots_bp <- plot.new()

df <- na.omit(data.daily[,c('site','year',vars)])
df$site <- as.factor(df$site)
df$year <- as.factor(df$year)

# Loop through each variables - CHECK NA.OMIT once we have all the data
for (i in 1:nvars){
  
  var <- vars[[i]]
  
  p1 <- ggboxplot(
    df, x = 'site', y = var, fill = 'year',
    palette = colors_sites[c(2,9)],
    lwd=0.2,
    outlier.size=0.3
  )+theme(text = element_text(size = 6))
  p1 
  
  plots_bp[[i]] <- p1
}

p1 <- ggarrange(plotlist=plots_bp)

ggsave("figures/flux_bp.png", p1,units = "cm",height = 8, width = 12, dpi = 320)

# Using the Kruskal-Wallis Test, we can decide whether the population distributions are identical without assuming them to follow the normal distribution.
# https://www.cfholbert.com/blog/nonparametric_two_way_anova/

# One method that is not unduly affected by violations of the normality and homoscedasticity assumptions of parametric ANOVA is to transform the data values to their ranks, and then compute a parametric two-way ANOVA on the data ranks.
# This rank transformation procedure can be used to determine whether the ranks differ from group to group. Post-hoc comparisons can be performed on the data ranks to determine which groups are different from each other. 

# Finally, let’s perform two-way ANOVA on the rank-transformed data. 
# Ranking is one of many procedures used to transform data that do not meet the assumptions of normality. 
# Conover and Iman (1981) provided a review of the four main types of rank transformations. 
# One method replaces each original data value by its rank (from 1 for the smallest to N for the largest, where N is the combined data sample size). 
# This rank-based procedure has been recommended as being robust to non-normal errors, resistant to outliers, and highly efficient for many distributions.

# ANOVA procedure on rank-transformed data and regular data
anova.rnk <- list()
anova.org <- list()
aov.rnk <- list()
aov.org <- list()
lt.rnk <- list()
dist.rnk <- list()
for (i in 1:nvars){
  
  var <- vars[[i]]
  
  aov.rnk[[i]] <- df%>%dplyr::rename("var" = vars[i]) %>% aov(
    rank(var) ~ year * site, data = .,
    contrasts = list(
      site = 'contr.sum',
      year = 'contr.sum'
    )
  )
  anova.rnk[[i]] <- Anova(aov.rnk[[i]], type = 'III') # Really similar to regular anova below!
  
  # Let’s first perform two-way ANOVA on the original data.
  aov.org[[i]] <- df%>%dplyr::rename("var" = vars[i]) %>% aov(
    var ~ year * site, data = .,
    contrasts = list(
      site = 'contr.sum',
      year = 'contr.sum'
    )
  )
  
  anova.org[[i]] <- Anova(aov.org[[i]], type = 'III')
  
  #Use the Levene’s test to check the homogeneity of variances. The function leveneTest() [in car package] will be used:
  # NOTE: if p < 0.05. This means that there is evidence to suggest that the variance across groups is statistically significantly different. 
  # Therefore, we CAN'T assume the homogeneity of variances in the different treatment groups. 
  # if p > 0.05 CAN assume homogeneity of variances
  lt.rnk[[i]] <- df%>%dplyr::rename("var" = vars[i])%>%
    leveneTest(rank(var) ~ year * site, data = .)
  
  # Rename intercept to variable name
  rownames(anova.rnk[[i]])[1] <- vars[i]
  
  # Test for normality. Normally distributed if p > 0.05
  dist <- df%>%dplyr::rename("var" = vars[i]) %>%shapiro_test(var)
  dist$variable <- c(vars[i],vars[i],vars[i],vars[i])
  dist.rnk[[i]] <- dist
}

# Let’s inspect the residuals from each model for 1) Homogeneity of variances and 2) normality. Tests are above.
plots_hv <- plot.new()
plots_qq <- plot.new()
for (i in 1:nvars){
  
  var <- vars[[i]]  
  res.rnk = aov.rnk[[i]]$resid
  
  p <- as.ggplot(~(qqnorm(
    res.rnk, pch = 20, main = paste("Rank-Transformed ",var,sep=""),
    cex.lab = 1, cex.axis = 0.7, cex.main = 1
  )))
  plots_qq[[i]] <- p
  
  p1 <- as.ggplot(~plot(aov.rnk[[i]], 1, main = paste("Rank-Transformed ",var,sep="")))
  plots_hv[[i]] <- p1
}

