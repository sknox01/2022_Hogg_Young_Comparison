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
library(corrplot)
library(factoextra)

# Reconcile with exploratory_plots & clean up exploratory_plots 

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

# water quality data
# Table summary of WQ data
vars <- c('pH','SO4','Specific_cond','DOC','TDN','NO3_NO2_N','NH4_N','DRP','TDP','TP','ABS_280nm')

data.daily.WQ.mean <- data.daily %>%
  group_by(site,year) %>%
  summarise_at(vars(vars), funs(mean = mean(.,na.rm=TRUE)))

vars_names <- c("site","year",vars) # UPDATE LATER AND INCLUDE UNITS
colnames(data.daily.WQ.mean) <- vars_names

data.daily.WQ.mean[,vars] <- round(data.daily.WQ.mean[,vars])

# PCA of WQ parameters - GO OVER ASSUMPTIONS IN MORE DETAIL!
# 1. Extra WQ parameters
vars <- c('site','FCH4_gC','pH','SO4','Specific_cond','DOC','TDN','NO3_NO2_N','NH4_N','DRP','TDP','TP','ABS_280nm')

# Extract only variables of interest and non-NA data
data.PCA.all <- na.omit(data.daily[,vars])
data.PCA <- data.PCA.all[, -c(1:2)]

# Look at correlation between variables - CHECK PCA Assumptions!

# 2. Compute PCA (from http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/)
res.pca <- prcomp(data.PCA, scale = TRUE)

# 3. Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_eig(res.pca)

# 4. Plot biplot (UPDATE COLORS & better format figure)
var_explained <- res.pca$sdev ^ 2 / sum(res.pca$sdev ^ 2)
groups <- as.factor(data.PCA.all$site)

p <- fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "grey33", # Variables color
                pointsize = data.PCA.all$FCH4_gC*1000,
                pointshape = 21,
                fill.ind = groups,
                label = "var",
                addEllipses = F,
                legend.title = list(fill = "Site", size = "FCH4 (mgC m-2 d-1)"),
                invisible = "quali",
                labelsize = 3,
                title = "") + 
  geom_hline(yintercept=0, color='gray70', size=0.3,linetype="dashed") +
  geom_vline(xintercept=0, color='gray70', size=0.3,linetype="dashed") + 
  theme(text = element_text(size = 8),legend.position="bottom")+
  labs(x = paste(paste("PC 1 (", round(var_explained[1]*100,1)),"%)", sep = ""), y = paste(paste("PC 2 (", round(var_explained[2]*100,1)),"%)", sep = ""))
p

ggsave("figures/PCA_WQ.png", p,units = "cm",height = 8, width = 12, dpi = 320)

# Figures for the SI (Make figures nicer)

# Box plots of WQ parameters

# Specify variables (only one year for each of these at the moment)
vars <- c('pH','Specific_cond','DOC','TDN','NO3_NO2_N','NH4_N','DRP','TDP','TP','ABS_280nm')
nvars <- length(vars)

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

# Check if it should be a t-test or Wilcoxon test....
# The results of these experiments indicate that Student’s t-test should definitely be avoided for sample sizes smaller than 20 (https://www.datascienceblog.net/post/statistical_test/parametric_sample_size/)
# See also https://www.datascienceblog.net/post/statistical_test/signed_wilcox_rank_test/

# Confirm statistics! or should it be t-test? Probably t-test eventually
W_test <- list()
for (i in 1:nvars){
  
  var <- vars[[i]]
  W_test[[i]] <- wilcox.test(na.omit(data.daily)[[var]]~na.omit(data.daily)[["site"]])
}

ggsave("figures/WQ_bp.png", p,units = "cm",height = 8, width = 12, dpi = 320)

var <- c('SO4')

p <- ggplot(data.daily, aes(as.factor(.data[["year"]]), .data[[var]],fill = .data[["site"]])) + 
  geom_boxplot() + xlab('') + theme_bw() + theme(text = element_text(size = 6))
p

ggsave("figures/SO4_bp.png", p,units = "cm",height = 5, width = 6, dpi = 320)

# Confirm statistics!
# Two-way anova (http://www.sthda.com/english/wiki/two-way-anova-test-in-r OR https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/)
res.aov2 <- aov(data.daily[[var]]~data.daily[["site"]]*data.daily[["year"]])
summary(res.aov2)

