library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(here)
library(plotly)
library(Metrics)
library(pdp)
library(vip)
library(ranger)

# Load data
load(here("output/daily_data.Rda"))

# plot daily data
ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = datetime,y = FCH4_gC,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = datetime,y = NO3_NO2_N_interp,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = TP_interp,y = FCH4_gC,color = site)))

# from https://github.com/yeonukkim/EC_FCH4_gapfilling
# Select variables for the model - REDO WITH 2022 DATA! Add LE here? Or remove from Tigramite analysis?
predictors <- c("FCH4_gC","TA","WTD","VPD","USTAR","GPP_PI_F_NT_gC","SO4_pred_interp","TP_interp","DOC_interp","NH4_N_interp","NO3_NO2_N_interp","pH_interp")
# predictors <- c("FCH4_gC","TA","WTD","VPD","USTAR","GPP_PI_F_NT_gC","SO4_pred_interp","TP_interp","pH_interp","year")

data.daily.model <- na.omit(data.daily[,c(predictors,"site","year","datetime")]) # Remove all missing data
data.daily.model$site <- as.factor(data.daily.model$site)
data.daily.model$year <- as.factor(data.daily.model$year)

ggplotly(ggplot()+
           geom_point(data = data.daily.model, aes(x = datetime,y = FCH4_gC,color = site)))

# Build random forest model
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
  FCH4_gC ~ ., 
  data = ML.df,
  num.trees = 500, # start at 10xn_feat, maintain at 100 below 10 feat
  method = 'ranger',
  trControl = myControl,
  tuneGrid = tgrid,
  importance = 'permutation',  ## or 'impurity'
  metric = "MAE" ## or 'rmse'
)

rf_model$bestTune

p <- vip(rf_model_final)+xlab('')+theme_classic()
p

# Get performance for final model
set.seed(500)
rf_model_final = ranger(formula = FCH4_gC ~ ., data = ML.df,
            num.trees = 500,
            mtry = 4,
            min.node.size = 5,
            splitrule = "variance",
            importance = 'permutation') 

# variable importance - rename variable names
p <- vip(rf_model_final)+xlab('')+theme_classic()
p

ggsave("figures/VarImp.png", p,units = "cm",height = 7, width = 6, dpi = 320)

# https://rpubs.com/vishal1310/QuickIntroductiontoPartialDependencePlots

par.TA<- partial(rf_model, pred.var = c("TA"), train = ML.df, chull = TRUE)
plot.TA<- autoplot(par.TA, contour = TRUE)
plot.TA

par.WTD<- partial(rf_model, pred.var = c("WTD"),  train = ML.df, chull = TRUE)
plot.WTD<- autoplot(par.WTD, contour = TRUE)
plot.WTD

par.SO4_pred_interp<- partial(rf_model, pred.var = c("SO4_pred_interp"),  train = ML.df, chull = TRUE)
plot.SO4_pred_interp<- autoplot(par.SO4_pred_interp, contour = TRUE)
plot.SO4_pred_interp

par.TP_interp<- partial(rf_model, pred.var = c("TP_interp"),  train = ML.df, chull = TRUE)
plot.TP_interp<- autoplot(par.TP_interp, contour = TRUE)
plot.TP_interp 

par.DOC_interp<- partial(rf_model, pred.var = c("DOC_interp"),  train = ML.df, chull = TRUE)
plot.DOC_interp<- autoplot(par.DOC_interp, contour = TRUE)
plot.DOC_interp 

# https://www.sciencedirect.com/science/article/pii/S135223101200533X?casa_token=F34j55vlokYAAAAA:vS0Qk_RH-9FXV2-_gPrfHi16riLJPT8QYoScifNrqSLkuWrgGAiA5uxLdLE4Hw6PDkEWHezYEw

# https://towardsdatascience.com/partial-dependence-plots-with-scikit-learn-966ace4864fc
# PDPs show the average effect on predictions as the value of feature changes.

# https://blogs.sas.com/content/subconsciousmusings/2018/06/12/interpret-model-predictions-with-partial-dependence-and-individual-conditional-expectation-plots/#:~:text=A%20partial%20dependence%20(PD)%20plot,flu%20increases%20linearly%20with%20fever.
# PD plots look at the variable of interest across a specified range. At each value of the variable, the model is evaluated for all observations of the other model inputs, and the output is then averaged. Thus, the relationship they depict is only valid if the variable of interest does not interact strongly with other model inputs.

# https://towardsdatascience.com/how-to-interpret-models-pdp-and-ice-eabed0062e2c
# Partial Dependence Plots (PDP) plots show the marginal contribution of different features on the output. They are used to display either the contribution of a single feature or two features.

# For PDP and ICE: These plots can be used in parallel to understand the predictions’ overall trends and check problematic individual cases. Because PDPs show the output changes on an aggregate level, the more minor variations of particular instances are lost. Therefore it is best to look at both variations

