

library(Metrics)
library(gplots)
library(graphics)
library(corrplot)
library(olsrr)
library(ggpubr)
library(rstatix)
library(tidyverse)
library(visdat)
library(GGally)
library(usmap)
library(mice)
library(VIM)
library(plotly)
library(caret)
library(e1071)
library(class)
library(mapproj)
library(stringr)
library(table1)
library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr') 
library('mice')
library('randomForest') 
library('data.table')
library('gridExtra')
library('corrplot') 
library('GGally')
library('e1071')
library(MASS)
library(data.table)
library(ggplot2)
library(randomForest)
library(dplyr)
library(corrplot)
library(knitr)
library(kableExtra)
library(easyGgplot2)
library(caret)
library(olsrr)
library(DataExplorer)
#Read the train and test csv from github
house_train=read.csv("https://raw.githubusercontent.com/RashmiAPatel19/6371-Statistics-Project/main/train.csv",header=TRUE)
head(house_train)
dim(house_train)
colnames(house_train)
house_test=read.csv("https://raw.githubusercontent.com/RashmiAPatel19/6371-Statistics-Project/main/test.csv",header=TRUE)
head(house_test)
dim(house_test)
colnames(house_test)

##########################################################################################################################


############## Cleaning of TRAIN SET ###########


summary(house_train$SalePrice)
# Dataset with NAmes BrkSide and Edwards neighborhood

data=data.frame(names=house_train$Neighborhood,Int_names=as.integer(as.factor(house_train$Neighborhood)))
head(data,80)
# Check for the total number of columns that are character and numeric in type
numeric_var_train=sum(sapply(house_train[,1:81],is.numeric))
numeric_var_train
char_var_train=sum(sapply(house_train[,1:81],is.character))
char_var_train
# Check for the names of  columns that are character and numeric in type
numeric_varname_train=which(sapply(house_train[,1:81],is.numeric))
numeric_varname_train
char_varname_train=which(sapply(house_train[,1:81],is.character))
char_varname_train

# Represent the total NA values as TRUE and FALSE     
table(is.na(house_train))

# Represent the columns having total NA values  in table form
missing_values_train <- colSums(sapply(house_train, is.na))
missing_values_train<- data.frame(house_Variables_train = names(missing_values_train), house_NA_Count_train = missing_values_train); rownames(missing_values_train) <- c()
missing_values_train<- missing_values_train %>% filter(house_NA_Count_train > 0)
kable(missing_values_train, "html") %>%
  kable_styling(full_width = F)
length(missing_values_train$house_Variables_train)
sum(missing_values_train$house_NA_Count_train)

# Substitute the NA with None in charcter and 0 in numeric
character_impute_train <- c("LotFrontage", "MasVnrArea","GarageYrBlt")
house_train[,character_impute_train] <- apply(house_train[,character_impute_train], 2, 
                                              function(x) {
                                                replace(x, is.na(x), 0)
                                              }
)

numeric_impute_train <- c("Alley", "MasVnrType","BsmtQual", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "FireplaceQu",  "GarageType", "GarageFinish", 
                          "GarageQual", "GarageCond", "BsmtCond","PoolQC","Fence","MiscFeature","Electrical")
house_train[,numeric_impute_train] <- apply(house_train[,numeric_impute_train], 2, 
                                            function(x) {
                                              replace(x, is.na(x), "None")
                                            }
)
colnames(house_train)
dim(house_train)

# Unique values in each columns
for(i in colnames(house_train)){
  print(unique(house_train[,i]))
}

# Converting character to integer
var_facs_train <- c("SaleCondition","SaleType","MiscFeature","Fence","PoolQC","PavedDrive","GarageCond","GarageQual","GarageFinish",
                    "GarageType","FireplaceQu","Functional","KitchenQual","Electrical","CentralAir","HeatingQC","Heating","BsmtFinType2",
                    "BsmtFinType1","BsmtExposure","BsmtCond","BsmtQual","Foundation","ExterCond","ExterQual","MasVnrArea","MasVnrType","Exterior2nd",
                    "Exterior1st","RoofMatl","RoofStyle","HouseStyle","BldgType","Condition2","Condition1","Neighborhood","LandSlope","LotConfig","Utilities",
                    "LandContour","LotShape","Alley","Street","MSZoning")

house_train[,var_facs_train] <- lapply(house_train[,var_facs_train] , factor, ordered = FALSE)
# Loop for converting character in integer
for(i in colnames(house_train)){
  house_train[,i]=as.integer(house_train[,i])
  str(house_train[,i])
}



# Plot the graphs and perform EDA
plot_histogram(house_train[,2:81])

ggplot(house_train,aes(x=SalePrice))+geom_histogram()
ggplot(house_train,aes(x=log(SalePrice)))+geom_histogram()

ggplot(house_train,aes(x=GrLivArea))+geom_histogram()
ggplot(house_train,aes(x=log(GrLivArea)))+geom_histogram()

ggplot(house_train,aes(x=as.integer(as.factor(house_train$Neighborhood))))+geom_histogram()
ggplot(house_train,aes(x=log(as.integer(as.factor(house_train$Neighborhood)))))+geom_histogram()

ggplot(house_train,aes(x=GrLivArea,y=SalePrice))+geom_point()+geom_smooth(method = "lm")
cor(house_train$GrLivArea,house_train$SalePrice)

ggplot(house_train,aes(x=log(GrLivArea),y=log(SalePrice)))+geom_point()+geom_smooth(method = "lm")
cor(log(house_train$GrLivArea),log(house_train$SalePrice))

table(is.na(house_train))

write.csv(house_train,file = "clean_train.csv")
##########################################################################################################################


############## Cleaning of TEST SET ###########




# Check for the total number of columns that are character and numeric in type
numeric_var_test=sum(sapply(house_test[,1:81],is.numeric))
numeric_var_test
char_var_test=sum(sapply(house_test[,1:81],is.character))
char_var_test
# Check for the names of  columns that are character and numeric in type
numeric_varname_test=which(sapply(house_test[,1:81],is.numeric))
numeric_varname_test
char_varname_test=which(sapply(house_test[,1:81],is.character))
char_varname_test

# Represent the total NA values as TRUE and FALSE     
table(is.na(house_test))

# Represent the columns having total NA values  in table form
missing_values_test <- colSums(sapply(house_test, is.na))
missing_values_test<- data.frame(house_Variables_test = names(missing_values_test), house_NA_Count_test = missing_values_test); rownames(missing_values_test) <- c()
missing_values_test<- missing_values_test %>% filter(house_NA_Count > 0)
kable(missing_values_test, "html") %>%
  kable_styling(full_width = F)
length(missing_values_test$house_Variables)
sum(missing_values_test$house_NA_Count)

# Substitute the NA with None in charcter and 0 in numeric
character_impute_test <- c("LotFrontage", "MasVnrArea","GarageYrBlt")
house_test[,character_impute_test] <- apply(house_test[,character_impute_test], 2, 
                                            function(x) {
                                              replace(x, is.na(x), 0)
                                            }
)

numeric_impute_test <- c("Alley", "MasVnrType","BsmtQual", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "FireplaceQu",  "GarageType", "GarageFinish", 
                         "GarageQual", "GarageCond", "BsmtCond","PoolQC","Fence","MiscFeature","Electrical")
house_test[,numeric_impute_test] <- apply(house_test[,numeric_impute_test], 2, 
                                          function(x) {
                                            replace(x, is.na(x), "None")
                                          }
)
colnames(house_test)
dim(house_test)

# Unique values in each columns
for(i in colnames(house_test)){
  print(unique(house_test[,i]))
}

# Converting character to integer
var_facs_test <- c("SaleCondition","SaleType","MiscFeature","Fence","PoolQC","PavedDrive","GarageCond","GarageQual","GarageFinish",
                   "GarageType","FireplaceQu","Functional","KitchenQual","Electrical","CentralAir","HeatingQC","Heating","BsmtFinType2",
                   "BsmtFinType1","BsmtExposure","BsmtCond","BsmtQual","Foundation","ExterCond","ExterQual","MasVnrArea","MasVnrType","Exterior2nd",
                   "Exterior1st","RoofMatl","RoofStyle","HouseStyle","BldgType","Condition2","Condition1","Neighborhood","LandSlope","LotConfig","Utilities",
                   "LandContour","LotShape","Alley","Street","MSZoning")

house_test[,var_facs_test] <- lapply(house_test[,var_facs_test] , factor, ordered = FALSE)
# Loop for converting character in integer
for(i in colnames(house_test)){
  house_test[,i]=as.integer(house_test[,i])
  str(house_test[,i])
}

table(is.na(house_test))


write.csv(house_train,file = "clean_train.csv")

######################################################
house_train$isTrainSet=TRUE
house_test$isTrainSet=FALSE
dim(house_train)
dim(house_test)



house_test$SalePrice=rep(NA,1459)

house_full=rbind(house_train,house_test)
dim(house_full)

train=house_full[house_full$isTrainSet==TRUE,]
test=house_full[house_full$isTrainSet==FALSE,]
dim(train)
dim(test)
############################# creating model
train.model=train%>%filter(Neighborhood=="8"|Neighborhood=="4"|Neighborhood=="13")

train.model=createDataPartition(y=train$SalePrice, p = 0.8, list = FALSE)
train.data  <- train[train.model, ]
test.data <- train[-train.model, ]
dim(train.data)
dim(test.data)
str(test.data)
#######################################################################################

all.model=lm(log(SalePrice)~.-isTrainSet,data=train.data)
summary(all.model)
forward_selection=ols_step_forward_p(all.model,penter = 0.002,details=TRUE)
forward_selection
backward_selection=ols_step_backward_p(all.model,prem = 0.0001,details=TRUE)
backward_selection
stepwise_selection=ols_step_both_p(all.model,prem = 0.00001,penter = 0.002,details=TRUE)
stepwise_selection

fit.model1=lm(log(SalePrice)~OverallQual+log(GrLivArea)+GarageCars+SaleCondition+TotalBsmtSF+BldgType+BsmtFinSF1+Fireplaces+YearBuilt+OverallCond,data=train.data)
summary(fit.model1)# better one forward selection

prediction=predict(fit.model1,newdata=test.data)
prediction

value=2.718^prediction
value

rmse.model=rmse(test.data$SalePrice,value)
rmse.model

table=data.frame(Id=test.data$Id,ObsSalePrice=test.data$SalePrice,PredSalePrice=value)
table

predictiontest=predict(fit.model1,newdata=test)
predictiontest

pred_value=2.718^predictiontest
pred_value

length(predictiontest)

output.df=data.frame(Id=test$Id, SalePrice=pred_value)
head(output.df)
dim(output.df)
table(is.na(output.df))
write.csv(output.df,file="kaggle_submission_.csv",row.names = FALSE)

####################################################################################
# fit.model2=lm(log(SalePrice)~OverallQual+log(GrLivArea)+GarageCars+CentralAir+BsmtFullBath+MSSubClass+YearRemodAdd+Fireplaces+YearBuilt+OverallCond,data=train.data)
# summary(fit.model2)
# 
# fit.model3=lm(log(SalePrice)~OverallQual+log(GrLivArea)+GarageCars+CentralAir+BsmtFullBath+MSSubClass+YearRemodAdd+Fireplaces+YearBuilt+OverallCond,data=train.data)
# summary(fit.model3)# stepwise selection
# 
# forward_selection=ols_step_forward_p(all.model,penter = 0.002,details=TRUE)
# forward_selection
# backward_selection=ols_step_backward_p(all.model,prem = 0.0001,details=TRUE)
# backward_selection
# stepwise_selection=ols_step_both_p(all.model,prem = 0.0001,penter = 0.002,details=TRUE)
# stepwise_selection


#######################################################################################

# Base Model
# base.model=lm(log(SalePrice)~log(GrLivArea),data=train.data)
# summary(base.model)
# 
# #Additive Model
# additive.model=lm(log(SalePrice)~log(GrLivArea)+NeighborhoodEdwards,data=train.data)
# summary(additive.model)
# #############################
# # cross validation leave one out mthod 
# train.control <- trainControl(method = "LOOCV")
# # Train the model
# model1 <- train(SalePrice ~.-isTrainSet, data = train.data, method = "lm",
#                trControl = train.control)
# # Summarize the results
# summary(model1)
# 
# ols_step_forward_p(model1,penter = 0.3,details=TRUE)
# 
# ols_step_forward_p(housemodel,penter = 0.3,details=TRUE)
# ols_step_backward_p(housemodel,prem = 0.3,details=TRUE)
# 
# ##########################################################
# 
# 
# 
# housemodel=lm(log(SalePrice)~log(GrLivArea)+OverallQual+,data=train.data)
# summary(housemodel)
# 
# predictmodel=predict(housemodel,newdata=test.data)
# predictmodel
# 
# val=2.718^predictmodel
# val
# 
# rmse.model=rmse(test.data$SalePrice,val)/mean(test.data$SalePrice)
# rmse.model
# 
# table=data.frame(Id=test.data$Id,ObsSalePrice=test.data$SalePrice,PredSalePrice=val)
# table
# ##################################################################################################################################
# 
# # DATE 04/09/2021
# rf.feature=randomForest(SalePrice~.-Id,data=train)
# varImpPlot(rf.feature)
# 
# plot_histogram(train)
# 
# house.model1=lm(SalePrice~GrLivArea+OverallQual,data=train)
# summary(house.model1)
# 
# predict.model1=predict(house.model1,newdata=test)
# predict.model1
# 
# house.model2=lm(log(SalePrice)~log(GrLivArea)+OverallQual,data=train)
# summary(house.model2)
# 
# predict.model2=predict(house.model2,newdata=test)
# predict.model2
# 
# 
# rmse1=rmse(predict.model1,test$SalePrice)
# rmse1
