library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(here)
library(plotly)
library(Metrics)
library(pdp)
library(vip)
library(ranger)
library(caret)
library(corrr)
library(scales)
library(ggpubr)

# Load data
load(here("output/daily_data.Rda")) # Average only for days with more than 50% of data

 #only daily FCH4 data with more than 50% coverage
data.daily$FCH4[data.daily$FCH4_n<48/2] <- NA

# Calculate salinity (ppt) from specific conductivity data
data.daily$salinity_model <- data.daily$Specific_cond_interp*0.0005 - 0.0224

# Create DOC/SO4 variable
data.daily$DOC_SO4_interp <- data.daily$DOC_interp/data.daily$SO4_pred_interp

# Only consider fluxes for days with more than 50% of data

# plot daily data
ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = datetime,y = FCH4,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = DOC_SO4_interp,y = FCH4,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = DOC_interp,y = FCH4,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = TDN_interp,y = FCH4,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = SO4_pred_interp,y = FCH4,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = pH_interp,y = FCH4,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = SO4_pred,y = DOC,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = datetime,y = SO4_pred_interp,color = site)))

# For 2021 only --------------------------------------------------------------------------------------------

## ADD SALINITY/COND!
# Select variables for the model - REDO WITH 2022 DATA! Think more about LE, USTAR and Wind direction
#predictors_model <- c("FCH4","TA","WTD","VPD","USTAR","GPP_PI_F_NT_gC","SO4_pred_interp","TP_interp", "DOC_interp",
#"NO3_NO2_N_interp","NH4_N_interp","pH_interp")

predictors_model <- c("FCH4","TA","WTD","VPD","USTAR","GPP_PI_F_NT_gC","TP_interp",
                      "Specific_cond_interp")


data.daily.model <- na.omit(data.daily[,c(predictors_model,"site","year","datetime")]) # Remove all missing data
data.daily.model$site <- as.factor(data.daily.model$site)
data.daily.model$year <- as.factor(data.daily.model$year)

#predictors <- c("FCH4","TA","WTD","VPD","USTAR","GPP","SO4","TP", "DOC",
#                "NO3_NO2","NH4","pH")

predictors <- c("FCH4","TA","WTD","VPD","USTAR","GPP","TP",
                "Cond")

colnames(data.daily.model)[1:length(predictors_model)] <- predictors

# Correlation plot
# https://www.datanovia.com/en/blog/easy-correlation-matrix-analysis-in-r-using-corrr-package/
res.cor <- correlate(data.daily.model[,predictors])
res.cor %>%
  focus(FCH4) %>%
  mutate(rowname = reorder(term, FCH4)) %>%
  ggplot(aes(rowname, FCH4)) +
  geom_col() + coord_flip() +
  theme_bw()

# Build random forest model ALL data
ML.df <- data.daily.model[,predictors]

# RF model from Knox et al. GCB (2021)

# A RF algorithm was trained for each site using the ranger package in R (R Core Team, 2019; Wright & Ziegler, 2017) with details 
# provided in the Supporting Information. We ranked predictors using
# permutation importance, which avoids bias of other methods (Strobl
# et al., 2007) and scaled importance for site comparisons (Table S4).
# We also provide out-of-bag model fit metrics (coefficient of determination, 
# mean absolute error, and bias) as a further evaluation of
# relative confidence in results between sites (Figures S13 and S14).

# A RF algorithm was trained for each site using the ranger package in R (Wright & Ziegler, 2017; R Core Team, 2019). 
# Each RF was trained on all available data at each site (i.e., we did not create training and test data splits) 
# and out-of-bag data were used for hyperparameter tuning. 
# We consider this appropriate because 1) our objective was to interpret details of the FCH4 signal within each site, 
# rather than to identify a predictive model that can generalize to new conditions (which is the purpose of validation and test splits)
# and 2) RF is already robust against outliers due to its regression to the mean effects (Zhang & Lu, 2012). 
# Hyperparameter tuning was performed for mtry (number of predictors randomly sampled at each decision node) and 
# min.node.size was set to 5 to allow for deep trees. We ranked predictors using permutation importance, 
# which avoids bias of other methods (Strobl et al., 2007), and scaled importances for site comparisons (Table S4). 
# We also provide out-of-bag model fit metrics (coefficient of determination, mean absolute error, and bias) 
# as a further evaluation of relative confidence in results between sites (Figure S13 and S14). 
# Average model performance across sites increased with time scale, ranging from R2 of 0.65 and (mean absolute errors) 
# MAE of 17.2 nmol m-2 s-1 at the daily scale to R2 of 0.99 and MAE of 0.02 nmol m-2 s-1 at the smoother, seasonal scale. 
# To evaluate the sensitivity of variable importance to our fitting approach,
# we compared results with 5-fold data-splitting where we iteratively held out 20% segments of the daily mean time series data. 
# We trained a RF on each fold and re-ranked predictors, this time using the median importance value from the 5 RF models at each site. 
# Results were unchanged at 20 of the 23 sites, and only at three sites (KR-CRK, SE-Deg, and US-Twt) did 
# the first-and second-ranked predictor switch positions.

## Create tune-grid (all combinations of hyper-parameters)
tgrid <- expand.grid(
  mtry = c(2, 3, 4, 5, 6),
  splitrule = "variance", 
  min.node.size = c(5, 50, 100)
)

## Create trainControl object (other )
myControl <- trainControl(
  method = "cv",
  allowParallel = TRUE,
  verboseIter = TRUE,  ## I think this is required for varimp analyses
  returnData = FALSE,
)

## train rf 
set.seed(500)
rf_model <- train(
  FCH4 ~ ., 
  data = ML.df,
  num.trees = 500, # start at 10xn_feat, maintain at 100 below 10 feat
  method = 'ranger',
  trControl = myControl,
  tuneGrid = tgrid,
  importance = 'permutation',  ## or 'impurity'
  metric = "MAE" ## or 'rmse'
)

rf_model$bestTune
rf_model$finalModel

vi <- as.data.frame(importance(rf_model$finalModel))
vi <- tibble::rownames_to_column(vi)
colnames(vi) <- c("Variable","Importance")

#scale values in 'sales' column to be between 0 and 1
#vi$Importance <- rescale(vi$Importance)*100

# Sort in ascending order
vi <- vi %>% arrange(-Importance)  

p_Young_Hogg <- ggplot(vi, aes(reorder(Variable, Importance, sum), Importance)) +
  geom_col()+ coord_flip()+theme_classic()+xlab('')
p_Young_Hogg
 
ggsave("figures/VarImp_Hogg_Young.png", p_Young_Hogg,units = "cm",height = 7, width = 6, dpi = 320)

# https://rpubs.com/vishal1310/QuickIntroductiontoPartialDependencePlots

#par.TA<- partial(rf_model, pred.var = c("TA"), train = ML.df, chull = TRUE, ice = TRUE)
par.TA<- partial(rf_model, pred.var = c("TA"), train = ML.df, chull = TRUE)
plot.TA<- autoplot(par.TA, contour = TRUE)
plot.TA

par.WTD<- partial(rf_model, pred.var = c("WTD"),  train = ML.df, chull = TRUE)
plot.WTD<- autoplot(par.WTD, contour = TRUE)
plot.WTD

#par.salinity_model <- partial(rf_model, pred.var = c("salinity_model"),  train = ML.df, chull = TRUE)
#plot.salinity_model<- autoplot(par.salinity_model, contour = TRUE) +theme_classic()+xlab('salinity (ppt)')+ylab('Predicted FCH4')
#plot.salinity_model

#par.Specific_cond_interp<- partial(rf_model, pred.var = c("Specific_cond_interp"),  train = ML.df, chull = TRUE)
#plot.Specific_cond_interp<- autoplot(par.Specific_cond_interp, contour = TRUE) +theme_classic()+xlab('Specific_cond')+ylab('Predicted FCH4')
#plot.Specific_cond_interp

#par.SO4_pred_interp<- partial(rf_model, pred.var = c("SO4_pred_interp"),  train = ML.df, chull = TRUE, ice = TRUE)
par.SO4_pred_interp<- partial(rf_model, pred.var = c("SO4"),  train = ML.df, chull = TRUE)
plot.SO4_pred_interp<- autoplot(par.SO4_pred_interp, contour = TRUE) +theme_classic()+xlab('SO4')+ylab('Predicted FCH4')
plot.SO4_pred_interp

#ggsave("figures/pdp_SO4_Hogg_Young.png", plot.SO4_pred_interp,units = "cm",height = 7, width = 6, dpi = 320)

par.TP_interp<- partial(rf_model, pred.var = c("TP"),  train = ML.df, chull = TRUE)
plot.TP_interp<- autoplot(par.TP_interp, contour = TRUE)
plot.TP_interp 

par.DOC_interp<- partial(rf_model, pred.var = c("DOC"),  train = ML.df, chull = TRUE)
plot.DOC_interp<- autoplot(par.DOC_interp, contour = TRUE)
plot.DOC_interp 

par.NO3_NO2_N_interp<- partial(rf_model, pred.var = c("NO3_NO2"),  train = ML.df, chull = TRUE)
plot.NO3_NO2_N_interp<- autoplot(par.NO3_NO2_N_interp, contour = TRUE)
plot.NO3_NO2_N_interp 

# Two variable plots if needed - https://journal.r-project.org/archive/2017/RJ-2017-016/RJ-2017-016.pdf

# https://www.sciencedirect.com/science/article/pii/S135223101200533X?casa_token=F34j55vlokYAAAAA:vS0Qk_RH-9FXV2-_gPrfHi16riLJPT8QYoScifNrqSLkuWrgGAiA5uxLdLE4Hw6PDkEWHezYEw

# https://towardsdatascience.com/partial-dependence-plots-with-scikit-learn-966ace4864fc
# PDPs show the average effect on predictions as the value of feature changes.

# https://blogs.sas.com/content/subconsciousmusings/2018/06/12/interpret-model-predictions-with-partial-dependence-and-individual-conditional-expectation-plots/#:~:text=A%20partial%20dependence%20(PD)%20plot,flu%20increases%20linearly%20with%20fever.
# PD plots look at the variable of interest across a specified range. At each value of the variable, the model is evaluated for all observations of the other model inputs, and the output is then averaged. Thus, the relationship they depict is only valid if the variable of interest does not interact strongly with other model inputs.

# https://towardsdatascience.com/how-to-interpret-models-pdp-and-ice-eabed0062e2c
# Partial Dependence Plots (PDP) plots show the marginal contribution of different features on the output. They are used to display either the contribution of a single feature or two features.

# For PDP and ICE: These plots can be used in parallel to understand the predictions’ overall trends and check problematic individual cases. Because PDPs show the output changes on an aggregate level, the more minor variations of particular instances are lost. Therefore it is best to look at both variations

# Build random forest model Young
ML.df.Young <- data.daily.model[(data.daily.model$site == 'Young'),predictors]

## train rf 
set.seed(500)
rf_model_young <- train(
  FCH4 ~ ., 
  data = ML.df.Young,
  num.trees = 500, # start at 10xn_feat, maintain at 100 below 10 feat
  method = 'ranger',
  trControl = myControl,
  tuneGrid = tgrid,
  importance = 'permutation',  ## or 'impurity'
  metric = "MAE" ## or 'rmse'
)

rf_model_young$bestTune
rf_model_young$finalModel

vi <- as.data.frame(importance(rf_model_young$finalModel))
vi <- tibble::rownames_to_column(vi)
colnames(vi) <- c("Variable","Importance")

#scale values in 'sales' column to be between 0 and 1
#vi$Importance <- rescale(vi$Importance)*100

# Sort in ascending order
vi <- vi %>% arrange(-Importance)  

p_Young <- ggplot(vi, aes(reorder(Variable, Importance, sum), Importance)) +
  geom_col()+ coord_flip()+theme_classic()+xlab('')
p_Young

#ggsave("figures/VarImp_Young.png", p,units = "cm",height = 7, width = 6, dpi = 320)

# PDP

par.TA.young<- partial(rf_model_young, pred.var = c("TA"), train = ML.df.Young, chull = TRUE)
plot.TA.young<- autoplot(par.TA.young, contour = TRUE)+theme_classic()+xlab('TA')+ylab('Predicted FCH4')
plot.TA.young

#ggsave("figures/pdp_TA_interp_young.png", plot.TA.young,units = "cm",height = 7, width = 6, dpi = 320)

par.GPP_PI_F_NT_gC.young<- partial(rf_model_young, pred.var = c("GPP"), train = ML.df.Young, chull = TRUE)
plot.GPP_PI_F_NT_gC.young<- autoplot(par.GPP_PI_F_NT_gC.young, contour = TRUE)+theme_classic()+xlab('GPP')+ylab('Predicted FCH4')
plot.GPP_PI_F_NT_gC.young

#ggsave("figures/pdp_GPP_PI_F_NT_gC_interp_young.png", plot.GPP_PI_F_NT_gC.young,units = "cm",height = 7, width = 6, dpi = 320)

par.WTD.young<- partial(rf_model_young, pred.var = c("WTD"),  train = ML.df.Young, chull = TRUE)
plot.WTD.young<- autoplot(par.WTD.young, contour = TRUE)
plot.WTD.young

par.SO4_pred_interp.young<- partial(rf_model_young, pred.var = c("SO4"),  train = ML.df.Young, chull = TRUE, ice = TRUE)
plot.SO4_pred_interp.young<- autoplot(par.SO4_pred_interp.young, contour = TRUE)
plot.SO4_pred_interp.young

par.TP_interp.young<- partial(rf_model_young, pred.var = c("TP"),  train = ML.df.Young, chull = TRUE)
plot.TP_interp.young<- autoplot(par.TP_interp.young, contour = TRUE)+theme_classic()+xlab('TP')+ylab('Predicted FCH4')
plot.TP_interp.young 

#ggsave("figures/pdp_TP_interp_young.png", plot.TP_interp.young,units = "cm",height = 7, width = 6, dpi = 320)

par.DOC_interp.young<- partial(rf_model_young, pred.var = c("DOC"),  train = ML.df.Young, chull = TRUE)
plot.DOC_interp.young<- autoplot(par.DOC_interp.young, contour = TRUE)
plot.DOC_interp.young 

# Build random forest model Hogg
ML.df.Hogg <- data.daily.model[(data.daily.model$site == 'Hogg'),predictors]

## train rf 
set.seed(500)
rf_model_Hogg <- train(
  FCH4 ~ ., 
  data = ML.df.Hogg,
  num.trees = 500, # start at 10xn_feat, maintain at 100 below 10 feat
  method = 'ranger',
  trControl = myControl,
  tuneGrid = tgrid,
  importance = 'permutation',  ## or 'impurity'
  metric = "MAE" ## or 'rmse'
)

rf_model_Hogg$bestTune
rf_model_Hogg$finalModel

vi <- as.data.frame(importance(rf_model_Hogg$finalModel))
vi <- tibble::rownames_to_column(vi)
colnames(vi) <- c("Variable","Importance")

#scale values in 'sales' column to be between 0 and 1
#vi$Importance <- rescale(vi$Importance)*100

# Sort in ascending order
vi <- vi %>% arrange(-Importance)  

p_Hogg <- ggplot(vi, aes(reorder(Variable, Importance, sum), Importance)) +
  geom_col()+ coord_flip()+theme_classic()+xlab('')
p_Hogg

#ggsave("figures/VarImp_Hogg.png", p,units = "cm",height = 7, width = 6, dpi = 320)

# PDP

par.TA.Hogg<- partial(rf_model_Hogg, pred.var = c("TA"), train = ML.df.Hogg, chull = TRUE)
plot.TA.Hogg<- autoplot(par.TA.Hogg, contour = TRUE)
plot.TA.Hogg

par.WTD.Hogg<- partial(rf_model_Hogg, pred.var = c("WTD"),  train = ML.df.Hogg, chull = TRUE)
plot.WTD.Hogg<- autoplot(par.WTD.Hogg, contour = TRUE)
plot.WTD.Hogg

par.SO4_pred_interp.Hogg<- partial(rf_model_Hogg, pred.var = c("SO4"),  train = ML.df.Hogg, chull = TRUE)
plot.SO4_pred_interp.Hogg<- autoplot(par.SO4_pred_interp.Hogg, contour = TRUE)
plot.SO4_pred_interp.Hogg

par.TP_interp.Hogg<- partial(rf_model_Hogg, pred.var = c("TP"),  train = ML.df.Hogg, chull = TRUE)
plot.TP_interp.Hogg<- autoplot(par.TP_interp.Hogg, contour = TRUE)
plot.TP_interp.Hogg 

par.DOC_interp.Hogg<- partial(rf_model_Hogg, pred.var = c("DOC"),  train = ML.df.Hogg, chull = TRUE)
plot.DOC_interp.Hogg<- autoplot(par.DOC_interp.Hogg, contour = TRUE)
plot.DOC_interp.Hogg 

# Save variable importance scores in one figure
p <- ggarrange(p_Young_Hogg,p_Young,ncol = 2, nrow = 1)
p

ggsave("figures/VarImp_all_sig.png", p,units = "cm",height = 12, width = 12, dpi = 320)

# Save pdp plots in one figure
p <- ggarrange(plot.SO4_pred_interp,plot.TP_interp.young,plot.TA.young,plot.GPP_PI_F_NT_gC.young,ncol = 2, nrow = 2)
p

ggsave("figures/pdp_multipanel.png", p,units = "cm",height = 12, width = 12, dpi = 320)


# 2021 & 2021 with subset of data --------------------------------------------------------------------------------
# Select variables for the model - REDO WITH 2022 DATA! Think more about LE, USTAR and Wind direction
predictors_model <- c("FCH4","TA","WTD","VPD","USTAR","GPP_PI_F_NT_gC","SO4_pred_interp","TP_interp")

data.daily.model <- na.omit(data.daily[,c(predictors_model,"site","year","datetime")]) # Remove all missing data
data.daily.model$site <- as.factor(data.daily.model$site)
data.daily.model$year <- as.factor(data.daily.model$year)

predictors <- c("FCH4","TA","WTD","VPD","USTAR","GPP","SO4","TP")

colnames(data.daily.model)[1:length(predictors_model)] <- predictors

# Correlation plot
# https://www.datanovia.com/en/blog/easy-correlation-matrix-analysis-in-r-using-corrr-package/
res.cor <- correlate(data.daily.model[,predictors])
res.cor %>%
  focus(FCH4) %>%
  mutate(rowname = reorder(term, FCH4)) %>%
  ggplot(aes(rowname, FCH4)) +
  geom_col() + coord_flip() +
  theme_bw()

# Build random forest model ALL data
ML.df <- data.daily.model[,predictors]

# RF model from Knox et al. GCB (2021)

# A RF algorithm was trained for each site using the ranger package in R (R Core Team, 2019; Wright & Ziegler, 2017) with details 
# provided in the Supporting Information. We ranked predictors using
# permutation importance, which avoids bias of other methods (Strobl
# et al., 2007) and scaled importance for site comparisons (Table S4).
# We also provide out-of-bag model fit metrics (coefficient of determination, 
# mean absolute error, and bias) as a further evaluation of
# relative confidence in results between sites (Figures S13 and S14).

# A RF algorithm was trained for each site using the ranger package in R (Wright & Ziegler, 2017; R Core Team, 2019). 
# Each RF was trained on all available data at each site (i.e., we did not create training and test data splits) 
# and out-of-bag data were used for hyperparameter tuning. 
# We consider this appropriate because 1) our objective was to interpret details of the FCH4 signal within each site, 
# rather than to identify a predictive model that can generalize to new conditions (which is the purpose of validation and test splits)
# and 2) RF is already robust against outliers due to its regression to the mean effects (Zhang & Lu, 2012). 
# Hyperparameter tuning was performed for mtry (number of predictors randomly sampled at each decision node) and 
# min.node.size was set to 5 to allow for deep trees. We ranked predictors using permutation importance, 
# which avoids bias of other methods (Strobl et al., 2007), and scaled importances for site comparisons (Table S4). 
# We also provide out-of-bag model fit metrics (coefficient of determination, mean absolute error, and bias) 
# as a further evaluation of relative confidence in results between sites (Figure S13 and S14). 
# Average model performance across sites increased with time scale, ranging from R2 of 0.65 and (mean absolute errors) 
# MAE of 17.2 nmol m-2 s-1 at the daily scale to R2 of 0.99 and MAE of 0.02 nmol m-2 s-1 at the smoother, seasonal scale. 
# To evaluate the sensitivity of variable importance to our fitting approach,
# we compared results with 5-fold data-splitting where we iteratively held out 20% segments of the daily mean time series data. 
# We trained a RF on each fold and re-ranked predictors, this time using the median importance value from the 5 RF models at each site. 
# Results were unchanged at 20 of the 23 sites, and only at three sites (KR-CRK, SE-Deg, and US-Twt) did 
# the first-and second-ranked predictor switch positions.

## Create tune-grid (all combinations of hyper-parameters)
tgrid <- expand.grid(
  mtry = c(2, 3, 4, 5, 6),
  splitrule = "variance", 
  min.node.size = c(5, 50, 100)
)

## Create trainControl object (other )
myControl <- trainControl(
  method = "cv",
  allowParallel = TRUE,
  verboseIter = TRUE,  ## I think this is required for varimp analyses
  returnData = FALSE,
)

## train rf 
set.seed(500)
rf_model <- train(
  FCH4 ~ ., 
  data = ML.df,
  num.trees = 500, # start at 10xn_feat, maintain at 100 below 10 feat
  method = 'ranger',
  trControl = myControl,
  tuneGrid = tgrid,
  importance = 'permutation',  ## or 'impurity'
  metric = "MAE" ## or 'rmse'
)

rf_model$bestTune
rf_model$finalModel

vi <- as.data.frame(importance(rf_model$finalModel))
vi <- tibble::rownames_to_column(vi)
colnames(vi) <- c("Variable","Importance")

#scale values in 'sales' column to be between 0 and 1
#vi$Importance <- rescale(vi$Importance)*100

# Sort in ascending order
vi <- vi %>% arrange(-Importance)  

p_Young_Hogg <- ggplot(vi, aes(reorder(Variable, Importance, sum), Importance)) +
  geom_col()+ coord_flip()+theme_classic()+xlab('')
p_Young_Hogg

#ggsave("figures/VarImp_Hogg_Young.png", p,units = "cm",height = 7, width = 6, dpi = 320)

# https://rpubs.com/vishal1310/QuickIntroductiontoPartialDependencePlots

#par.TA<- partial(rf_model, pred.var = c("TA"), train = ML.df, chull = TRUE, ice = TRUE)
par.TA<- partial(rf_model, pred.var = c("TA"), train = ML.df, chull = TRUE)
plot.TA<- autoplot(par.TA, contour = TRUE)
plot.TA

par.WTD<- partial(rf_model, pred.var = c("WTD"),  train = ML.df, chull = TRUE)
plot.WTD<- autoplot(par.WTD, contour = TRUE)
plot.WTD

#par.salinity_model <- partial(rf_model, pred.var = c("salinity_model"),  train = ML.df, chull = TRUE)
#plot.salinity_model<- autoplot(par.salinity_model, contour = TRUE) +theme_classic()+xlab('salinity (ppt)')+ylab('Predicted FCH4')
#plot.salinity_model

#par.Specific_cond_interp<- partial(rf_model, pred.var = c("Specific_cond_interp"),  train = ML.df, chull = TRUE)
#plot.Specific_cond_interp<- autoplot(par.Specific_cond_interp, contour = TRUE) +theme_classic()+xlab('Specific_cond')+ylab('Predicted FCH4')
#plot.Specific_cond_interp

#par.SO4_pred_interp<- partial(rf_model, pred.var = c("SO4_pred_interp"),  train = ML.df, chull = TRUE, ice = TRUE)
par.SO4_pred_interp<- partial(rf_model, pred.var = c("SO4"),  train = ML.df, chull = TRUE)
plot.SO4_pred_interp<- autoplot(par.SO4_pred_interp, contour = TRUE) +theme_classic()+xlab('SO4')+ylab('Predicted FCH4')
plot.SO4_pred_interp

#ggsave("figures/pdp_SO4_Hogg_Young.png", plot.SO4_pred_interp,units = "cm",height = 7, width = 6, dpi = 320)

par.TP_interp<- partial(rf_model, pred.var = c("TP"),  train = ML.df, chull = TRUE)
plot.TP_interp<- autoplot(par.TP_interp, contour = TRUE)
plot.TP_interp 

# par.DOC_interp<- partial(rf_model, pred.var = c("DOC"),  train = ML.df, chull = TRUE)
# plot.DOC_interp<- autoplot(par.DOC_interp, contour = TRUE)
# plot.DOC_interp 
# 
# par.NO3_NO2_N_interp<- partial(rf_model, pred.var = c("NO3_NO2"),  train = ML.df, chull = TRUE)
# plot.NO3_NO2_N_interp<- autoplot(par.NO3_NO2_N_interp, contour = TRUE)
# plot.NO3_NO2_N_interp 

# Two variable plots if needed - https://journal.r-project.org/archive/2017/RJ-2017-016/RJ-2017-016.pdf

# https://www.sciencedirect.com/science/article/pii/S135223101200533X?casa_token=F34j55vlokYAAAAA:vS0Qk_RH-9FXV2-_gPrfHi16riLJPT8QYoScifNrqSLkuWrgGAiA5uxLdLE4Hw6PDkEWHezYEw

# https://towardsdatascience.com/partial-dependence-plots-with-scikit-learn-966ace4864fc
# PDPs show the average effect on predictions as the value of feature changes.

# https://blogs.sas.com/content/subconsciousmusings/2018/06/12/interpret-model-predictions-with-partial-dependence-and-individual-conditional-expectation-plots/#:~:text=A%20partial%20dependence%20(PD)%20plot,flu%20increases%20linearly%20with%20fever.
# PD plots look at the variable of interest across a specified range. At each value of the variable, the model is evaluated for all observations of the other model inputs, and the output is then averaged. Thus, the relationship they depict is only valid if the variable of interest does not interact strongly with other model inputs.

# https://towardsdatascience.com/how-to-interpret-models-pdp-and-ice-eabed0062e2c
# Partial Dependence Plots (PDP) plots show the marginal contribution of different features on the output. They are used to display either the contribution of a single feature or two features.

# For PDP and ICE: These plots can be used in parallel to understand the predictions’ overall trends and check problematic individual cases. Because PDPs show the output changes on an aggregate level, the more minor variations of particular instances are lost. Therefore it is best to look at both variations

# Build random forest model Young
ML.df.Young <- data.daily.model[(data.daily.model$site == 'Young'),predictors]

## train rf 
set.seed(500)
rf_model_young <- train(
  FCH4 ~ ., 
  data = ML.df.Young,
  num.trees = 500, # start at 10xn_feat, maintain at 100 below 10 feat
  method = 'ranger',
  trControl = myControl,
  tuneGrid = tgrid,
  importance = 'permutation',  ## or 'impurity'
  metric = "MAE" ## or 'rmse'
)

rf_model_young$bestTune
rf_model_young$finalModel

vi <- as.data.frame(importance(rf_model_young$finalModel))
vi <- tibble::rownames_to_column(vi)
colnames(vi) <- c("Variable","Importance")

#scale values in 'sales' column to be between 0 and 1
#vi$Importance <- rescale(vi$Importance)*100

# Sort in ascending order
vi <- vi %>% arrange(-Importance)  

p_Young <- ggplot(vi, aes(reorder(Variable, Importance, sum), Importance)) +
  geom_col()+ coord_flip()+theme_classic()+xlab('')
p_Young

#ggsave("figures/VarImp_Young.png", p,units = "cm",height = 7, width = 6, dpi = 320)

# PDP

par.TA.young<- partial(rf_model_young, pred.var = c("TA"), train = ML.df.Young, chull = TRUE)
plot.TA.young<- autoplot(par.TA.young, contour = TRUE)+theme_classic()+xlab('TA')+ylab('Predicted FCH4')
plot.TA.young

#ggsave("figures/pdp_TA_interp_young.png", plot.TA.young,units = "cm",height = 7, width = 6, dpi = 320)

par.GPP_PI_F_NT_gC.young<- partial(rf_model_young, pred.var = c("GPP"), train = ML.df.Young, chull = TRUE)
plot.GPP_PI_F_NT_gC.young<- autoplot(par.GPP_PI_F_NT_gC.young, contour = TRUE)+theme_classic()+xlab('GPP')+ylab('Predicted FCH4')
plot.GPP_PI_F_NT_gC.young

#ggsave("figures/pdp_GPP_PI_F_NT_gC_interp_young.png", plot.GPP_PI_F_NT_gC.young,units = "cm",height = 7, width = 6, dpi = 320)

par.WTD.young<- partial(rf_model_young, pred.var = c("WTD"),  train = ML.df.Young, chull = TRUE)
plot.WTD.young<- autoplot(par.WTD.young, contour = TRUE)
plot.WTD.young

par.SO4_pred_interp.young<- partial(rf_model_young, pred.var = c("SO4"),  train = ML.df.Young, chull = TRUE, ice = TRUE)
plot.SO4_pred_interp.young<- autoplot(par.SO4_pred_interp.young, contour = TRUE)
plot.SO4_pred_interp.young

par.TP_interp.young<- partial(rf_model_young, pred.var = c("TP"),  train = ML.df.Young, chull = TRUE)
plot.TP_interp.young<- autoplot(par.TP_interp.young, contour = TRUE)+theme_classic()+xlab('TP')+ylab('Predicted FCH4')
plot.TP_interp.young 

#ggsave("figures/pdp_TP_interp_young.png", plot.TP_interp.young,units = "cm",height = 7, width = 6, dpi = 320)

# par.DOC_interp.young<- partial(rf_model_young, pred.var = c("DOC"),  train = ML.df.Young, chull = TRUE)
# plot.DOC_interp.young<- autoplot(par.DOC_interp.young, contour = TRUE)
# plot.DOC_interp.young 

# Build random forest model Hogg
ML.df.Hogg <- data.daily.model[(data.daily.model$site == 'Hogg'),predictors]

## train rf 
set.seed(500)
rf_model_Hogg <- train(
  FCH4 ~ ., 
  data = ML.df.Hogg,
  num.trees = 500, # start at 10xn_feat, maintain at 100 below 10 feat
  method = 'ranger',
  trControl = myControl,
  tuneGrid = tgrid,
  importance = 'permutation',  ## or 'impurity'
  metric = "MAE" ## or 'rmse'
)

rf_model_Hogg$bestTune
rf_model_Hogg$finalModel

vi <- as.data.frame(importance(rf_model_Hogg$finalModel))
vi <- tibble::rownames_to_column(vi)
colnames(vi) <- c("Variable","Importance")

#scale values in 'sales' column to be between 0 and 1
#vi$Importance <- rescale(vi$Importance)*100

# Sort in ascending order
vi <- vi %>% arrange(-Importance)  

p_Hogg <- ggplot(vi, aes(reorder(Variable, Importance, sum), Importance)) +
  geom_col()+ coord_flip()+theme_classic()+xlab('')
p_Hogg

#ggsave("figures/VarImp_Hogg.png", p,units = "cm",height = 7, width = 6, dpi = 320)

# PDP

par.TA.Hogg<- partial(rf_model_Hogg, pred.var = c("TA"), train = ML.df.Hogg, chull = TRUE)
plot.TA.Hogg<- autoplot(par.TA.Hogg, contour = TRUE)
plot.TA.Hogg

par.WTD.Hogg<- partial(rf_model_Hogg, pred.var = c("WTD"),  train = ML.df.Hogg, chull = TRUE)
plot.WTD.Hogg<- autoplot(par.WTD.Hogg, contour = TRUE)
plot.WTD.Hogg

par.SO4_pred_interp.Hogg<- partial(rf_model_Hogg, pred.var = c("SO4"),  train = ML.df.Hogg, chull = TRUE)
plot.SO4_pred_interp.Hogg<- autoplot(par.SO4_pred_interp.Hogg, contour = TRUE)
plot.SO4_pred_interp.Hogg

par.TP_interp.Hogg<- partial(rf_model_Hogg, pred.var = c("TP"),  train = ML.df.Hogg, chull = TRUE)
plot.TP_interp.Hogg<- autoplot(par.TP_interp.Hogg, contour = TRUE)
plot.TP_interp.Hogg 

# par.DOC_interp.Hogg<- partial(rf_model_Hogg, pred.var = c("DOC"),  train = ML.df.Hogg, chull = TRUE)
# plot.DOC_interp.Hogg<- autoplot(par.DOC_interp.Hogg, contour = TRUE)
# plot.DOC_interp.Hogg 

# Save variable importance scores in one figure
p <- ggarrange(p_Young_Hogg,p_Young,ncol = 2, nrow = 1)
p

#ggsave("figures/VarImp_all_sig.png", p,units = "cm",height = 12, width = 12, dpi = 320)

# Save pdp plots in one figure
p <- ggarrange(plot.SO4_pred_interp,plot.TP_interp.young,plot.TA.young,plot.GPP_PI_F_NT_gC.young,ncol = 2, nrow = 2)
p

#ggsave("figures/pdp_multipanel.png", p,units = "cm",height = 12, width = 12, dpi = 320)
