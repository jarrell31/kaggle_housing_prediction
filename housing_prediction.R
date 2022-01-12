#install.packages("")
library(dplyr)
library(ggplot2)
library(mice)
library(reshape2)
library(caret)
library(broom)
library(car)
library(vtreat)
library(kernlab)
library(classifierplots)
library(car)
library(glmnet)
library(gridExtra)

options(scipen=999)

train <- read.csv("C:\\Users\\Owner\\Desktop\\shiny_resume\\housing_prediction\\train.csv")
#test <- read.csv("C:\\Users\\Owner\\Desktop\\shiny_resume\\housing_prediction\\test.csv")

colSums(is.na(train))
summary(train)
str(train)

#Understanding categorical vars
table(train$MSSubClass)
train$MSSubClass <- factor(train$MSSubClass)
ggplot(train, aes(x = MSSubClass, y = SalePrice)) + geom_boxplot()
#
table(train$MSZoning)
train$MSZoning <- factor(train$MSZoning)
ggplot(train, aes(x = MSZoning, y = SalePrice)) + geom_boxplot()
#
table(train$Street)
train$Street <- factor(train$Street)
ggplot(train, aes(x = Street, y = SalePrice)) + geom_boxplot()
#
table(train$Alley)
train$Alley[is.na(train$Alley)] <- "NA"
train$Alley <- factor(train$Alley)
ggplot(train, aes(x = Alley, y = SalePrice)) + geom_boxplot()
#
table(train$LotShape)
train$LotShape <- factor(train$LotShape)
ggplot(train, aes(x = LotShape, y = SalePrice)) + geom_boxplot()
#
table(train$LandContour)
train$LandContour <- factor(train$LandContour)
ggplot(train, aes(x = LandContour, y = SalePrice)) + geom_boxplot()
#
table(train$Utilities)
#
table(train$LotConfig)
train$LotConfig <- factor(train$LotConfig)
ggplot(train, aes(x = LotConfig, y = SalePrice)) + geom_boxplot()
#
table(train$LandSlope)
train$LandSlope <- factor(train$LandSlope)
ggplot(train, aes(x = LandSlope, y = SalePrice)) + geom_boxplot()
#
table(train$Neighborhood)
train$Neighborhood <- factor(train$Neighborhood)
ggplot(train, aes(x = Neighborhood, y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
#
table(train$Condition1)
train$Condition1 <- factor(train$Condition1)
ggplot(train, aes(x = Condition1, y = SalePrice)) + geom_boxplot()
#
table(train$Condition2)
train$Condition2 <- factor(train$Condition2)
ggplot(train, aes(x = Condition2, y = SalePrice)) + geom_boxplot()
#
table(train$BldgType)
train$BldgType <- factor(train$BldgType)
ggplot(train, aes(x = BldgType, y = SalePrice)) + geom_boxplot()
#
table(train$HouseStyle)
train$HouseStyle <- factor(train$HouseStyle)
ggplot(train, aes(x = HouseStyle, y = SalePrice)) + geom_boxplot()
#
table(train$OverallQual)
train$OverallQual <- factor(train$OverallQual, ordered = T, levels = c(1:10))
ggplot(train, aes(x = OverallQual, y = SalePrice)) + geom_boxplot()
#
table(train$OverallCond)
train$OverallCond <- factor(train$OverallCond, order = T, levels = c(1:9))
ggplot(train, aes(x = OverallCond, y = SalePrice)) + geom_boxplot()
#
table(train$RoofStyle)
train$RoofStyle <- factor(train$RoofStyle)
ggplot(train, aes(x = RoofStyle, y = SalePrice)) + geom_boxplot()
#
table(train$RoofMatl)
train$RoofMatl <- factor(train$RoofMatl)
ggplot(train, aes(x = RoofMatl, y = SalePrice)) + geom_boxplot()
#
table(train$Exterior1st)
train$Exterior1st <- factor(train$Exterior1st)
ggplot(train, aes(x = Exterior1st, y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
#
table(train$Exterior2nd)
train$Exterior2nd <- factor(train$Exterior2nd)
ggplot(train, aes(x = Exterior2nd, y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
#
table(train$MasVnrType)
train$MasVnrType[is.na(train$MasVnrType)] <- "NA"
train$MasVnrType <- factor(train$MasVnrType)
ggplot(train, aes(x = MasVnrType, y = SalePrice)) + geom_boxplot()
#
table(train$ExterQual)
train$ExterQual <- factor(train$ExterQual)
ggplot(train, aes(x = ExterQual, y = SalePrice)) + geom_boxplot()
#
# table(train$ExterCond)
# train$ExterCond <- factor(train$ExterCond)
# ggplot(train, aes(x = ExterCond, y = SalePrice)) + geom_boxplot()#condition and quality explain the same thing?
#
table(train$Foundation)
train$Foundation <- factor(train$Foundation)
ggplot(train, aes(x = Foundation, y = SalePrice)) + geom_boxplot()
#
table(train$BsmtQual)
train$BsmtQual[is.na(train$BsmtQual)] <- "NA"
train$BsmtQual <- factor(train$BsmtQual)
ggplot(train, aes(x = BsmtQual, y = SalePrice)) + geom_boxplot()
#
# table(train$BsmtCond)
# train$BsmtCond[is.na(train$BsmtCond)] <- "NA"
# train$BsmtCond <- factor(train$BsmtCond)
# ggplot(train, aes(x = BsmtCond, y = SalePrice)) + geom_boxplot() #does bsmt qual do explain same thing?
#
table(train$BsmtExposure)
train$BsmtExposure[is.na(train$BsmtExposure)] <- "NA"
train$BsmtExposure <- factor(train$BsmtExposure)
ggplot(train, aes(x = BsmtExposure, y = SalePrice)) + geom_boxplot()
#
table(train$BsmtFinType1)
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "NA"
train$BsmtFinType1 <- factor(train$BsmtFinType1)
ggplot(train, aes(x = BsmtFinType1, y = SalePrice)) + geom_boxplot()
#
table(train$BsmtFinType2)
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "NA"
train$BsmtFinType2 <- factor(train$BsmtFinType2)
ggplot(train, aes(x = BsmtFinType2, y = SalePrice)) + geom_boxplot()
#
# table(train$Heating)
# train$Heating <- factor(train$Heating)
# ggplot(train, aes(x = Heating, y = SalePrice)) + geom_boxplot() #use var? 97% uses gasA
#
table(train$HeatingQC)
train$HeatingQC <- factor(train$HeatingQC)
ggplot(train, aes(x = HeatingQC, y = SalePrice)) + geom_boxplot()
#
table(train$CentralAir)
train$CentralAir <- ifelse(train$CentralAir == "Y", 1,0 )
train$CentralAir <- factor(train$CentralAir)
ggplot(train, aes(x = CentralAir, y = SalePrice)) + geom_boxplot()
#
table(train$Electrical)
train$Electrical[is.na(train$Electrical)] <- "NA"
train$Electrical <- factor(train$Electrical)
ggplot(train, aes(x = Electrical, y = SalePrice)) + geom_boxplot()
#
train$BsmtHalfBath <- ifelse(train$BsmtHalfBath > 0, train$BsmtHalfBath * 0.5, train$BsmtHalfBath)
train$HalfBath <- ifelse(train$HalfBath > 0, train$HalfBath * 0.5, train$HalfBath)
train$total_bath <- train$BsmtFullBath + train$BsmtHalfBath + train$FullBath + train$HalfBath
table(train$total_bath)
train <- train %>% mutate(total_bath2 = if_else(total_bath >= 4, 4,
                                                if_else(total_bath < 4 & total_bath >= 3, 3,
                                                        if_else(total_bath < 3 & total_bath >= 2, 2,
                                                                if_else(total_bath < 2, 1, 0)))))
train$total_bath2 <- factor(train$total_bath2, ordered = T, levels = (1:4))
ggplot(train, aes(x = total_bath2, y = SalePrice)) + geom_boxplot()
#
table(train$BedroomAbvGr)
train <- train %>% mutate(BedroomAbvGr2 = if_else(BedroomAbvGr >= 4, 5,
                                                if_else(BedroomAbvGr == 3, 4,
                                                        if_else(BedroomAbvGr == 2, 3,
                                                                if_else(BedroomAbvGr == 1, 2, 
                                                                        if_else(BedroomAbvGr == 0, 1, 0))))))
train$BedroomAbvGr2 <- factor(train$BedroomAbvGr2, ordered = T, levels = (1:5))
ggplot(train, aes(x = BedroomAbvGr2, y = SalePrice)) + geom_boxplot() 
#
# table(train$KitchenAbvGr) #who cares?
# train$KitchenAbvGr <- factor(train$KitchenAbvGr)
# ggplot(train, aes(x = KitchenAbvGr, y = SalePrice)) + geom_boxplot()
#
table(train$KitchenQual)
train$KitchenQual <- factor(train$KitchenQual)
ggplot(train, aes(x = KitchenQual, y = SalePrice)) + geom_boxplot()
#
table(train$TotRmsAbvGrd)
train <- train %>% mutate(TotRmsAbvGrd2 = if_else(TotRmsAbvGrd >= 10, 10,
                                                        if_else(TotRmsAbvGrd == 9, 9,
                                                                if_else(TotRmsAbvGrd == 8, 8,
                                                                        if_else(TotRmsAbvGrd == 7, 7,
                                                                                if_else(TotRmsAbvGrd == 6, 6,
                                                                                        if_else(TotRmsAbvGrd == 5, 5,
                                                                                                if_else(TotRmsAbvGrd == 4, 4,
                                                                                                        if_else(TotRmsAbvGrd == 3, 3,
                                                                                                                if_else(TotRmsAbvGrd == 2, 2,
                                                                                                                        if_else(TotRmsAbvGrd == 1, 1, 0)))))))))))
train$TotRmsAbvGrd2 <- factor(train$TotRmsAbvGrd2, ordered = T, levels = c(1:10))
ggplot(train, aes(x = TotRmsAbvGrd2, y = SalePrice)) + geom_boxplot()
#
table(train$Functional)
train$Functional <- factor(train$Functional)
ggplot(train, aes(x = Functional, y = SalePrice)) + geom_boxplot()
#
table(train$Fireplaces)
train <- train %>% mutate(Fireplaces2 = if_else(Fireplaces >= 3, 4,
                                                  if_else(Fireplaces == 2, 3,
                                                          if_else(Fireplaces == 1, 2,
                                                                  if_else(Fireplaces == 0, 1, 0)))))
train$Fireplaces2 <- factor(train$Fireplaces2, ordered = T, levels = c(1:4))
ggplot(train, aes(x = Fireplaces2, y = SalePrice)) + geom_point() #3+
#
table(train$FireplaceQu)
train$FireplaceQu[is.na(train$FireplaceQu)] <- "NA"
train$FireplaceQu <- factor(train$FireplaceQu)
ggplot(train, aes(x = FireplaceQu, y = SalePrice)) + geom_boxplot()
#
table(train$GarageType)
train$GarageType[is.na(train$GarageType)] <- "NA"
train$GarageType <- factor(train$GarageType)
ggplot(train, aes(x = GarageType, y = SalePrice)) + geom_boxplot()
#
table(train$GarageFinish)
train$GarageFinish[is.na(train$GarageFinish)] <- "NA"
train$GarageFinish <- factor(train$GarageFinish)
ggplot(train, aes(x = GarageFinish, y = SalePrice)) + geom_boxplot()
#
table(train$GarageCars)
train <- train %>% mutate(GarageCars2 = if_else(GarageCars >= 3, 4,
                                                if_else(GarageCars == 2, 3,
                                                        if_else(GarageCars == 1, 2,
                                                                if_else(GarageCars == 0, 1, 0)))))
train$GarageCars2 <- factor(train$GarageCars2, ordered = T, levels = c(1:4))
ggplot(train, aes(x = GarageCars2, y = SalePrice)) + geom_boxplot() 
#
table(train$GarageQual)
train$GarageQual[is.na(train$GarageQual)] <- "NA"
train$GarageQual <- factor(train$GarageQual)
ggplot(train, aes(x = GarageQual, y = SalePrice)) + geom_boxplot()
#
table(train$PavedDrive)
train$PavedDrive <-factor(train$PavedDrive)
ggplot(train, aes(x = PavedDrive, y = SalePrice)) + geom_boxplot()
#
table(train$PoolQC)
train$has_pool <- ifelse(train$PoolArea > 0, 1, 0)
train$has_pool <- factor(train$has_pool)
ggplot(train, aes(x = has_pool, y = SalePrice)) + geom_boxplot()
#
table(train$Fence)
train$Fence[is.na(train$Fence)] <- "NA"
train$Fence <- factor(train$Fence)
ggplot(train, aes(x = Fence, y = SalePrice)) + geom_boxplot()
#
table(train$MiscFeature)
train$MiscFeature[is.na(train$MiscFeature)] <- "NA"
train$MiscFeature <- factor(train$MiscFeature)
ggplot(train, aes(x = MiscFeature, y = SalePrice)) + geom_boxplot()
#
# table(train$MoSold)
# train$MoSold <- factor(train$MoSold)
# ggplot(train, aes(x = MoSold, y = SalePrice)) + geom_boxplot() #not sig
#
# table(train$YrSold)
# train$YrSold <- factor(train$YrSold)
# ggplot(train, aes(x = YrSold, y = SalePrice)) + geom_boxplot() #not sig
#
table(train$SaleType)
train$SaleType <- factor(train$SaleType)
ggplot(train, aes(x = SaleType, y = SalePrice)) + geom_boxplot()
#
table(train$SaleCondition)
train$SaleCondition <- factor(train$SaleCondition)
ggplot(train, aes(x = SaleCondition, y = SalePrice)) + geom_boxplot()

#understanding continuous vars
ggplot(train, aes(x = LotFrontage, y = SalePrice)) + geom_point() #259 missing values.
train$LotFrontage[is.na(train$LotFrontage)] <- 0
sum(is.na(train$LotFrontage))
#
ggplot(train, aes(x = LotArea, y = SalePrice)) + geom_point()
#
table(train$YearBuilt)
str(train$YearBuilt)
train$house_age <- 2021 - train$YearBuilt
ggplot(train, aes(x = house_age, y = SalePrice)) + geom_point()
#
table(train$YearRemodAdd)
str(train$YearRemodAdd)
train$remod_age <- 2021 - train$YearRemodAdd
ggplot(train, aes(x = remod_age, y = SalePrice)) + geom_point()
#
table(train$MasVnrArea)
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
ggplot(train, aes(x = MasVnrArea, y = SalePrice)) + geom_point()
#
ggplot(train, aes(x = BsmtFinSF1, y = SalePrice)) + geom_point() 
#
ggplot(train, aes(x = BsmtFinSF2, y = SalePrice)) + geom_point() 
#
ggplot(train, aes(x = BsmtUnfSF, y = SalePrice)) + geom_point()
#
ggplot(train, aes(x = TotalBsmtSF, y = SalePrice)) + geom_point() #use this rather than all bsmt vars. filter outlier?
boxplot(train$TotalBsmtSF)
#
ggplot(train, aes(x = X1stFlrSF, y = SalePrice)) + geom_point()
ggplot(train, aes(x = X2ndFlrSF, y = SalePrice)) + geom_point()
train$total_sf <- train$X1stFlrSF + train$X2ndFlrSF
ggplot(train, aes(x = total_sf, y = SalePrice)) + geom_point()
#
#ggplot(train, aes(x = LowQualFinSF, y = SalePrice)) + geom_point() #useful?
#
ggplot(train, aes(x = GrLivArea, y = SalePrice)) + geom_point() # highly correlated with total_sf?
#
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 1980
train$garage_age <- 2021 - train$GarageYrBlt
ggplot(train, aes(x = garage_age, y = SalePrice)) + geom_point()
#
ggplot(train, aes(x = GarageArea, y = SalePrice)) + geom_point()
#
ggplot(train, aes(x = WoodDeckSF, y = SalePrice)) + geom_point()
#
ggplot(train, aes(x = OpenPorchSF, y = SalePrice)) + geom_point()
#
ggplot(train, aes(x = EnclosedPorch, y = SalePrice)) + geom_point()
#
ggplot(train, aes(x = X3SsnPorch, y = SalePrice)) + geom_point()
#
ggplot(train, aes(x = ScreenPorch, y = SalePrice)) + geom_point()
#
ggplot(train, aes(x = MiscVal, y = SalePrice)) + geom_point()

#reorder cols
train_modeling <- train %>% select(Id, SalePrice 
                                   ,MSSubClass
                                   ,MSZoning
                                   ,Street
                                   ,Alley
                                   ,LotShape
                                   ,LandContour
                                   ,LotConfig
                                   ,LandSlope
                                   ,Neighborhood
                                   ,Condition1
                                   ,Condition2
                                   ,BldgType
                                   ,HouseStyle
                                   ,OverallQual
                                   ,OverallCond
                                   ,RoofStyle
                                   ,RoofMatl
                                   ,Exterior1st
                                   ,Exterior2nd
                                   ,MasVnrType
                                   ,ExterQual
                                   ,Foundation
                                   ,BsmtQual
                                   ,BsmtExposure
                                   ,BsmtFinType1
                                   ,BsmtFinType2
                                   ,HeatingQC
                                   ,CentralAir
                                   ,Electrical
                                   ,total_bath2
                                   ,BedroomAbvGr2
                                   ,KitchenQual
                                   ,TotRmsAbvGrd2
                                   ,Functional
                                   ,Fireplaces2
                                   ,FireplaceQu
                                   ,GarageType
                                   ,GarageFinish
                                   ,GarageCars2
                                   ,GarageQual
                                   ,PavedDrive
                                   ,has_pool
                                   ,Fence
                                   ,MiscFeature
                                   ,SaleType
                                   ,SaleCondition
                                   ,LotFrontage #start continuous
                                   ,LotArea
                                   ,house_age
                                   ,remod_age
                                   ,MasVnrArea
                                   ,BsmtFinSF1
                                   ,BsmtFinSF2
                                   ,BsmtUnfSF
                                   ,TotalBsmtSF
                                   ,total_sf
                                   ,GrLivArea
                                   ,garage_age
                                   ,GarageArea
                                   ,WoodDeckSF
                                   ,OpenPorchSF
                                   ,EnclosedPorch
                                   ,X3SsnPorch
                                   ,ScreenPorch
                                   ,MiscVal)


colSums(is.na(train_modeling))
str(train_modeling)

train_modeling$SalePrice <- log(train_modeling$SalePrice)

#feature variance
feature_variance <- nearZeroVar(train_modeling, saveMetrics = T)

#categorical variable treatment
my_treatment <- designTreatmentsZ(
  dframe = train_modeling,
  varlist = colnames(train_modeling),
  minFraction = 0.1
)

train_modeling_treated <- prepare(my_treatment, train_modeling)
test_modeling_treated <- prepare(my_treatment, test_modeling)
dim(train_modeling_treated)
colnames(train_modeling_treated)

train_modeling_treated <- train_modeling_treated %>% select(-contains("_catP"))
test_modeling_treated <- test_modeling_treated %>% select(-contains("_catP"))

#correlation matrix
corr_matrix <- cor(train_modeling_treated)
high_corr <- findCorrelation(corr_matrix, cutoff = 0.90)
train_modeling_treated <- train_modeling_treated[,-high_corr]
melted_corrmat <- melt(corr_matrix)
melted_corrmat <- melted_corrmat %>% filter(Var1 != Var2)

ggplot(data = melted_corrmat, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#linear combos
linear_combos <- findLinearCombos(train_modeling_treated)
colnames(train_modeling_treated[linear_combos$remove])
train_modeling_treated <- train_modeling_treated[,-c(linear_combos$remove)]

#
set.seed(8632)
train_split <- train_modeling_treated %>% sample_frac(0.8)
test <- train_modeling_treated %>% anti_join(train_split)

#
sale_price_log <- train_split$SalePrice
sale_price_log_test <- test$SalePrice
train_modeling_treated <- train_split[,-c(1,2)]
test_modeling_treated <- test[,-c(1,2)]

#stepwise regression
step_control <- trainControl(method = "cv",
                             number = 3,
                             returnResamp = "final")

set.seed(1234)
step_fit <- train(train_modeling_treated, sale_price_log, 
                  method = "leapForward",
                  tuneGrid = data.frame(nvmax = 5:50),
                  trControl = step_control,
                  trace = FALSE)
step_fit$bestTune

step_coef <- tidy(coef(step_fit$finalModel, step_fit$bestTune[1,]))

step_coef <- step_coef[-1,]

lm_formula <- as.formula(paste("y ~ ", paste(step_coef$names, collapse = "+"), sep = ""))
train_modeling_treated$y <- sale_price_log
step_lm <- lm(lm_formula, data = train_modeling_treated)
summary(step_lm)
#plot(step_lm)
vif(step_lm)

step_pred_test <- predict(step_lm, test_modeling_treated)
postResample(pred = step_pred_test, obs = sale_price_log_test)

#support vector machine
x_selected <- train_modeling_treated[,(colnames(train_modeling_treated) %in% step_coef$names)]
grid <- expand.grid(.C = c(1,2,3))

svm <- train(x_selected, sale_price_log,
             method = "svmLinear",
             trControl = step_control, #same as mlr model
             tuneGrid = grid,
             metric = "MAE")
svm
svm_fit <- ksvm(as.matrix(x_selected), sale_price_log,
                kernel = "vanilladot", #specifies linear model. see ??kernlab::ksvm
                prob.model = F,
                kpar = "automatic",
                C = svm$bestTune)
svm_pred_train <- kernlab::predict(svm_fit, x_selected, type = "response")
postResample(pred = svm_pred_train, obs = sale_price_log)

x_selected_test <- test_modeling_treated[,(colnames(train_modeling_treated) %in% step_coef$names)]
svm_pred_test <- kernlab::predict(svm_fit, x_selected_test, type = "response")
postResample(pred = svm_pred_test, obs = sale_price_log_test)

residTest <- sale_price_log_test - svm_pred_test
car::qqPlot(residTest)

#Ridge
ridge <- cv.glmnet(as.matrix(x_selected), sale_price_log,
                   nfolds = 5,
                   type.measure = "mae",
                   alpha = 0)

ridge_pred <- predict(ridge, as.matrix(x_selected), type = "response",)
postResample(pred = ridge_pred, obs = sale_price_log)

ridge_pred_test <- predict(ridge, as.matrix(x_selected_test), type = "response")
postResample(pred = ridge_pred_test, obs = sale_price_log_test)

#LASSO 
lasso <- cv.glmnet(as.matrix(x_selected), sale_price_log,
                   nfolds = 5,
                   type.measure = "mae",
                   alpha = 1)

lasso_pred <- predict(lasso, as.matrix(x_selected), type = "response",)
postResample(pred = lasso_pred, obs = sale_price_log)

lasso_pred_test <- predict(lasso, as.matrix(x_selected_test), type = "response")
postResample(pred = lasso_pred_test, obs = sale_price_log_test)

#Reverse transform natural log
#lm
results_lm <- data.frame(exp(step_pred_test), exp(test$SalePrice))
colnames(results_lm) <- c("predicted_lm", "actual")
lm_plot <- ggplot(results_lm, aes(predicted_lm, actual)) + geom_point() + geom_smooth()

#svm
results_svm <- data.frame(exp(svm_pred_test), exp(test$SalePrice))
colnames(results_svm) <- c("predicted_svm", "actual")
svm_plot <- ggplot(results_svm, aes(predicted_svm, actual)) + geom_point() + geom_smooth()

#Ridge
results_ridge <- data.frame(exp(ridge_pred_test), exp(test$SalePrice))
colnames(results_ridge) <- c("predicted_ridge", "actual")
ridge_plot <- ggplot(results_ridge, aes(predicted_ridge, actual)) + geom_point() + geom_smooth()

#LASSO
results_lasso <- data.frame(exp(lasso_pred_test), exp(test$SalePrice))
colnames(results_lasso) <- c("predicted_lasso", "actual")
lasso_plot <- ggplot(results_lasso, aes(predicted_lasso, actual)) + geom_point() + geom_smooth()

grid.arrange(lm_plot, svm_plot, ridge_plot, lasso_plot, ncol = 2)

#
results_all <- rbind(
postResample(results_lm$predicted_lm, results_lm$actual),
postResample(results_svm$predicted_svm, results_svm$actual),
postResample(results_ridge$predicted_ridge, results_ridge$actual),
postResample(results_lasso$predicted_lasso, results_lasso$actual))

results_all <- data.frame(results_all, model = c("lm", "svm", "ridge", "lasso"))
results_all
selected_model <- results_all %>% filter(MAE == min(MAE))
selected_model$model
