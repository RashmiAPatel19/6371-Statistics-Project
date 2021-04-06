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

library(data.table)
library(ggplot2)
library(randomForest)
library(dplyr)
library(corrplot)
library(knitr)
library(kableExtra)
library(easyGgplot2)
library(caret)

house_train=read.csv("https://raw.githubusercontent.com/RashmiAPatel19/6371-Statistics-Project/main/train.csv",header=TRUE)
head(house_train)
dim(house_train)
colnames(house_train)
house_test=read.csv("https://raw.githubusercontent.com/RashmiAPatel19/6371-Statistics-Project/main/test.csv",header=TRUE)
head(house_test)
dim(house_test)
colnames(house_test)

summary(house_train$SalePrice)



sum(sapply(house_train[,1:81],is.numeric))
sum(sapply(house_train[,1:81],is.character))

which(sapply(house_train[,1:81],is.numeric))
which(sapply(house_train[,1:81],is.character))
      
table(is.na(house_train))


missing_values <- colSums(sapply(house_train, is.na))

# Set table
 missing_values<- data.frame(house_Variables = names(missing_values), house_NA_Count = missing_values); rownames(missing_values) <- c()

# Remove variables that don't have missing values
 missing_values<- missing_values %>% filter(house_NA_Count > 0)

kable(missing_values, "html") %>%
  kable_styling(full_width = F)

length(missing_values$house_Variables)
sum(missing_values$house_NA_Count)

y <- c("LotFrontage", "MasVnrArea","GarageYrBlt")
house_train[,y] <- apply(house_train[,y], 2, 
                    function(x) {
                      replace(x, is.na(x), 0)
                    }
)

y <- c("Alley", "MasVnrType","BsmtQual", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "FireplaceQu",  "GarageType", "GarageFinish", 
       "GarageQual", "GarageCond", "BsmtCond","PoolQC","Fence","MiscFeature","Electrical")
house_train[,y] <- apply(house_train[,y], 2, 
                    function(x) {
                      replace(x, is.na(x), "None")
                    }
)


table(is.na(house_train))

house_test$SalePrice=rep(0,1459)
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

houseTest=house_test%>%select(Neighborhood,GrLivArea,SalePrice)%>%filter(Neighborhood=="NAmes"|Neighborhood=="Edwards"|Neighborhood=="BrkSide")

fit=lm(log_houseSalePrice~.,data=house_analysis1)
summary(fit)
predict(fit,newdata=houseTest)

str(houseTest$SalePrice)

