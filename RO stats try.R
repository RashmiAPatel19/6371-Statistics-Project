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


house_test$SalePrice=rep(NA,1459)
house=bind_rows(house_train,house_test)
dim(house)



##################################################################################################################################







set.seed(42)
partition <- createDataPartition(y = house_train$SalePrice, p = 0.7, list = F)
trainingdata = house_train[partition, ]
test <- house_train[-partition, ]
partitiontraining <- createDataPartition(y = trainingdata$SalePrice, p = 0.8, list = F)
training <- trainingdata[partitiontraining, ]
validation <- trainingdata[-partitiontraining, ]

fit.lm.model=lm(SalePrice~.,data=training)
summary(fit.lm.model)


predict.lm.model=predict(fit.lm.model,newdata=validation)
error=predict.lm.model-validation$SalePrice
error
RMSE_Model=sqrt(mean(error^2))
RMSE_Model
ptest <- predict(fit.lm.model, test)
error1 <- (ptest- test$SalePrice)
error1
RMSE_NewData <- sqrt(mean(error1^2))
RMSE_NewData


Method <- c("Train/Test Split")
ModelRMSE <- c(RMSE_Model)
RMSENewData <- c(RMSE_NewData)

table1 <- data.frame(Method, ModelRMSE, RMSENewData)
table1

rf.feature=randomForest(SalePrice~.-Id,data=house_train)
varImpPlot(rf.feature)



#################################################################
set.seed(42)
partition <- createDataPartition(y = house_train$SalePrice, p = 0.7, list = F)
trainingdata = house_train[partition, ]
test <- house_train[-partition, ]
partitiontraining <- createDataPartition(y = trainingdata$SalePrice, p = 0.8, list = F)
training <- trainingdata[partitiontraining, ]
validation <- trainingdata[-partitiontraining, ]

fit.lm.model=lm(SalePrice~OverallQual+GrLivArea+GarageCars+ExterQual+TotalBsmtSF+GarageArea+X1stFlrSF+
                  YearBuilt+X2ndFlrSF+BsmtQual,data=training)
summary(fit.lm.model)

predict.lm.model=predict(fit.lm.model,newdata=validation)
error=predict.lm.model-validation$SalePrice
error
RMSE_Model=sqrt(mean(error^2))
RMSE_Model
ptest <- predict(fit.lm.model, test)
error1 <- (ptest- test$SalePrice)
error1
RMSE_NewData <- sqrt(mean(error1^2))
RMSE_NewData


Method <- c("Train/Test Split")
ModelRMSE <- c(RMSE_Model)
RMSENewData <- c(RMSE_NewData)

table1 <- data.frame(Method, ModelRMSE, RMSENewData)
table1

rf.feature=randomForest(SalePrice~.-Id,data=house_train)
varImpPlot(rf.feature)


ols_step_forward_p(fit.lm.model,penter = 0.15,details=TRUE)

ols_step_backward_p(fit.lm.model,prem = 0.15,details=TRUE)

######################################################



############for specific neighborhood
train_neighborhood=house_train%>%filter(Neighborhood==8|Neighborhood==4|Neighborhood==13)
head(train_neighborhood)
dim(train_neighborhood)


set.seed(200)
partition <- createDataPartition(y = train_neighborhood$SalePrice, p = 0.7, list = F)
trainingdata = train_neighborhood[partition, ]
test <- train_neighborhood[-partition, ]
partitiontraining <- createDataPartition(y = trainingdata$SalePrice, p = 0.8, list = F)
training <- trainingdata[partitiontraining, ]
validation <- trainingdata[-partitiontraining, ]

fit.lm.model=lm(SalePrice~.,data=training)
summary(fit.lm.model)


predict.lm.model=predict(fit.lm.model,newdata=validation)
error=predict.lm.model-validation$SalePrice
error
RMSE_Model=sqrt(mean(error^2))
RMSE_Model
ptest <- predict(fit.lm.model, test)
error1 <- (ptest- test$SalePrice)
length(error1)
RMSE_NewData <- sqrt(mean(error1^2))
RMSE_NewData

pred_test=predict(fit.lm.model, house_test)
error2 <- (ptest- test$SalePrice)
error2
RMSE_test <- sqrt(mean(error2^2))
RMSE_test

Method <- c("Train/Test Split")
ModelRMSE <- c(RMSE_Model)
RMSENewData <- c(RMSE_NewData)

table1 <- data.frame(Method, ModelRMSE, RMSENewData)
table1

rf.feature=randomForest(SalePrice~.-Id,data=train_neighborhood)
varImpPlot(rf.feature)

plot_histogram(train_neighborhood$LotArea)
######### Linear model after random forest feature selection

rf.lm.model=lm(log(SalePrice)~log(GrLivArea)+OverallQual+log(X1stFlrSF)+TotalBsmtSF+log(LotArea)+log(GarageArea)+YearBuilt+log(LotFrontage)+log(X2ndFlrSF)+YearRemodAdd,data=training)
summary(rf.lm.model)

ols_step_forward_p(fit.lm.model,penter = 0.7,details=TRUE)

ols_step_backward_p(fit.lm.model,prem = 0.6,details=TRUE)

ols_step_both_p(fit.lm.model,penter = 0.3,prem = 0.3,details=TRUE)





##########################################
test_neighborhood=house_test%>%filter(Neighborhood==8|Neighborhood==4|Neighborhood==13)
head(test_neighborhood)
dim(test_neighborhood)

test=predict(fit.lm.model,test)
error_test=test-house_test$SalePrice
error_test