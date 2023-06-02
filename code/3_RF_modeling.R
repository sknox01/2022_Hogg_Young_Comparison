library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(here)
library(psych)
library(rstatix)
library(nlme)
library(lme4)
library(AICcmodavg)
library(plotly)

# Load data
load(here("output/daily_data.Rda"))

# plot daily data
ggplotly(ggplot()+
  geom_point(data = data.daily, aes(x = datetime,y = FCH4_gC,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = datetime,y = TP_interp,color = site)))

ggplotly(ggplot()+
           geom_point(data = data.daily, aes(x = TP_interp,y = FCH4_gC,color = site)))

# from https://github.com/yeonukkim/EC_FCH4_gapfilling
# Select variables for the model
vars <- c("FCH4_gC","TA","WTD","GPP_PI_F_NT_gC","SO4_pred_interp","TP_interp","DOC_interp","NH4_N_interp","NO3_NO2_N_interp")
data.daily.model <- na.omit(data.daily[,c(vars,"site","year","datetime")]) # Remove all missing data
data.daily.model$site <- as.factor(data.daily.model$site)
data.daily.model$year <- as.factor(data.daily.model$year)

ggplotly(ggplot()+
           geom_point(data = data.daily.model, aes(x = datetime,y = FCH4_gC,color = site)))

# Build random forest model
predictors <- c("FCH4_gC","TA","WTD","GPP_PI_F_NT_gC","SO4_pred_interp","TP_interp","DOC_interp","NH4_N_interp","NO3_NO2_N_interp")
                
ML.df <- data.daily.model[,predictors]

# 75% of data used for model tuning/validation
index <- createDataPartition(ML.df$FCH4_gC, p=0.75, list=F) 
train_set <- ML.df[index,]
test_set <- ML.df[-index,]

tgrid <- data.frame(mtry = c(1,2,3,4,5,6,9))
#Add parallel processing for the fast processing if you want to
library(parallel)
library(doParallel)
cluster <- makeCluster(6)
registerDoParallel(cluster)
# RF_FCH4 <- train(FCH4_gC ~ ., data = train_set[,predictors],
# 								 method = "rf",
# 								 preProcess = c("medianImpute"),                #impute missing met data with median
# 								 trControl=trainControl(method = "repeatedcv",   #three-fold cross-validation for model parameters 3 times
# 								 											number = 3,                #other option: "cv" without repetition
# 								 											repeats = 3),
# 								 tuneGrid = tgrid,
# 								 na.action = na.pass,
# 								 allowParallel=TRUE, # This requires parallel packages. Otherwise you can choose FALSE.
# 								 ntree=500, # can generate more trees
# 								 importance = TRUE)
# 
# RF_FCH4$bestTune
# RF_FCH4$results

set.seed(500)
RF_FCH4 <- train(FCH4_gC ~ ., data = train_set[,predictors],
                 method = "rf",
                 preProcess = c("medianImpute"),  # impute missing met data with median (but it will not be applied since we used gap-filled predictors)
                 trControl = trainControl(method = "none"),
                 tuneGrid=data.frame(mtry=3), # use known mtry value.
                 na.action = na.pass,
                 allowParallel=FALSE,
                 ntree=500, # can generate more trees
                 importance = TRUE)


# variable importance
plot(varImp(RF_FCH4, scale = FALSE), main="variable importance")

#generate FCH4_rf predictions for test set
test_set$FCH4_rf <- predict(RF_FCH4, test_set, na.action = na.pass)
regrRF <- lm(test_set$FCH4_rf ~ test_set$FCH4_gC); 
print(summary(regrRF))

library(Metrics)
## 
## Attaching package: 'Metrics'
## The following objects are masked from 'package:caret':
## 
##     precision, recall
print(paste0('MAE: ' , mae(y_test,predictions) ))
## [1] "MAE: 742.401258870433"
print(paste0('MSE: ' ,caret::postResample(predictions , y_test)['RMSE']^2 ))
## [1] "MSE: 1717272.6547428"
print(paste0('R2: ' ,caret::postResample(predictions , y_test)['Rsquared'] ))
## [1] "R2: 0.894548902990278"

ggplot(test_set, aes(x=FCH4_gC, y=FCH4_rf)) + geom_abline(slope = 1, intercept = 0)+
  geom_point() + geom_smooth(method = "lm") + ggtitle("testset")

# whole dataset
result <- data.frame(FCH4 = ML.df$FCH4_gC) # you can add datetime column here if you want to.
result$FCH4_RF_model <- predict(RF_FCH4, ML.df, na.action = na.pass) # FCH4 RF model
result$FCH4_RF_filled <- ifelse(is.na(result$FCH4),result$FCH4_RF_model,result$FCH4) # gap-filled column (true value when it is, gap-filled value when missing)
result$FCH4_RF_residual <- ifelse(is.na(result$FCH4),NA,result$FCH4_RF_model - result$FCH4) # residual (model - obs). can be used for random uncertainty analysis

# time series
result$datetime <- data.daily.model$datetime
result$site <- data.daily.model$site

result %>% ggplot(aes(datetime,FCH4_RF_model,color = as.factor(site))) + geom_line() +
  geom_point(aes(datetime,FCH4,color = as.factor(site)))+
  theme_bw() + ylab(expression(paste("FCH4 (gC ", m^-2, d^-1,")")))

# https://www.r-bloggers.com/2021/10/random-forest-in-r-2/
  
# https://rpubs.com/vishal1310/QuickIntroductiontoPartialDependencePlots
library(pdp)

par.TA<- partial(RF_FCH4, pred.var = c("TA"), chull = TRUE)
plot.TA<- autoplot(par.TA, contour = TRUE)
plot.TA

par.WTD<- partial(RF_FCH4, pred.var = c("WTD"), chull = TRUE)
plot.WTD<- autoplot(par.WTD, contour = TRUE)
plot.WTD

par.SO4_pred_interp<- partial(RF_FCH4, pred.var = c("SO4_pred_interp"), chull = TRUE)
plot.SO4_pred_interp<- autoplot(par.SO4_pred_interp, contour = TRUE)
plot.SO4_pred_interp

par.TP_interp<- partial(RF_FCH4, pred.var = c("TP_interp"), chull = TRUE)
plot.TP_interp<- autoplot(par.TP_interp, contour = TRUE)
plot.TP_interp # https://www.sciencedirect.com/science/article/pii/S135223101200533X?casa_token=F34j55vlokYAAAAA:vS0Qk_RH-9FXV2-_gPrfHi16riLJPT8QYoScifNrqSLkuWrgGAiA5uxLdLE4Hw6PDkEWHezYEw

# https://towardsdatascience.com/partial-dependence-plots-with-scikit-learn-966ace4864fc
# PDPs show the average effect on predictions as the value of feature changes.

# https://blogs.sas.com/content/subconsciousmusings/2018/06/12/interpret-model-predictions-with-partial-dependence-and-individual-conditional-expectation-plots/#:~:text=A%20partial%20dependence%20(PD)%20plot,flu%20increases%20linearly%20with%20fever.
# PD plots look at the variable of interest across a specified range. At each value of the variable, the model is evaluated for all observations of the other model inputs, and the output is then averaged. Thus, the relationship they depict is only valid if the variable of interest does not interact strongly with other model inputs.

# https://towardsdatascience.com/how-to-interpret-models-pdp-and-ice-eabed0062e2c
# Partial Dependence Plots (PDP) plots show the marginal contribution of different features on the output. They are used to display either the contribution of a single feature or two features.

# For PDP and ICE: These plots can be used in parallel to understand the predictions’ overall trends and check problematic individual cases. Because PDPs show the output changes on an aggregate level, the more minor variations of particular instances are lost. Therefore it is best to look at both variations

