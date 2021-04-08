# https://www.kaggle.com/pradeeptripathi/predicting-house-prices-using-r
# https://github.com/susanli2016/Data-Analysis-with-R/blob/master/Predict-House-Price.Rmd
# https://jackdry.com/house-prices-advanced-regression-techniques-kaggle
# https://rstudio-pubs-static.s3.amazonaws.com/358187_2157f6fed1e243d998c5cae2af2eca55.html 
# https://www.dataquest.io/blog/kaggle-getting-started/ 
# http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/ 

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

summary(house_train$SalePrice)


# Check for the total number of columns that are character and numeric in type
sum(sapply(house_train[,1:81],is.numeric))
sum(sapply(house_train[,1:81],is.character))

# Check for the names of  columns that are character and numeric in type
which(sapply(house_train[,1:81],is.numeric))
which(sapply(house_train[,1:81],is.character))

# Represent the total NA values as TRUE and FALSE     
table(is.na(house_train))

# Represent the columns having total NA values  in table form
missing_values <- colSums(sapply(house_train, is.na))
missing_values<- data.frame(house_Variables = names(missing_values), house_NA_Count = missing_values); rownames(missing_values) <- c()
missing_values<- missing_values %>% filter(house_NA_Count > 0)
kable(missing_values, "html") %>%
  kable_styling(full_width = F)
length(missing_values$house_Variables)
sum(missing_values$house_NA_Count)

# Substitute the NA with None in charcter and 0 in numeric
character_impute <- c("LotFrontage", "MasVnrArea","GarageYrBlt")
house_train[,character_impute] <- apply(house_train[,character_impute], 2, 
                    function(x) {
                      replace(x, is.na(x), 0)
                    }
)

numeric_impute <- c("Alley", "MasVnrType","BsmtQual", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "FireplaceQu",  "GarageType", "GarageFinish", 
       "GarageQual", "GarageCond", "BsmtCond","PoolQC","Fence","MiscFeature","Electrical")
house_train[,numeric_impute] <- apply(house_train[,numeric_impute], 2, 
                    function(x) {
                      replace(x, is.na(x), "None")
                    }
)

table(is.na(house_train))

# Add the Sale Price Column in Test set
house_test$SalePrice=rep(0,1459)

# Combine the train and test set
house=bind_rows(house_train,house_test)
dim(house)

plot_histogram(house$SalePrice)

house_train$log_houseSalePrice=log(house_train$SalePrice)
plot_histogram(house_train$log_houseSalePrice)

house_analysis1=house_train%>%select(Neighborhood,GrLivArea,SalePrice,log_houseSalePrice)%>%filter(Neighborhood=="NAmes"|Neighborhood=="Edwards"|Neighborhood=="BrkSide")
head(house_analysis1)
unique(house_analysis1$Neighborhood)
dim(house_analysis1)
str(house_analysis1)


analysis1=house_train%>%select(-c(Id))%>%filter(Neighborhood=="NAmes"|Neighborhood=="Edwards"|Neighborhood=="BrkSide")
dim(analysis1)
colnames(analysis1)
houseTest=house_test%>%select(Neighborhood,GrLivArea,SalePrice)%>%filter(Neighborhood=="NAmes"|Neighborhood=="Edwards"|Neighborhood=="BrkSide")

fit=lm(log_houseSalePrice~.-SalePrice,data=analysis1)
summary(fit)

model_predict=predict(fit,newdata=houseTest)
mean_predict=mean(model_predict)
mean_predict

str(houseTest$SalePrice)
#random forest feature selection
rf=randomForest(log_houseSalePrice~.-SalePrice,data =analysis1)
importance(rf)
varImpPlot(rf)
# Using stepAIC function 
stepAIC(fit, direction = "both", trace = FALSE)
stepAIC(fit, direction = "backward", trace = FALSE)
stepAIC(fit, direction = "forward", trace = FALSE)


# feature selection using forward selection 
ols_step_forward_p(fit,penter = 0.05,details = TRUE)



# feature selection using backward selection
ols_step_backward_p(fit,penter = 0.7,details = TRUE)




# feature selection using stepwise selection