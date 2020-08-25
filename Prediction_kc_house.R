setwd("D:\\Big Data Management\\Intro_To_B_A\\ex3")
kc_house = read.csv("kc_house.csv",stringsAsFactors = F)
### Q 1

###a fixing variables types
kc_house$waterfront = as.factor(kc_house$waterfront)
kc_house$condition = as.factor(kc_house$condition)


### b using stargazer to summarize the data
library(stargazer)
stargazer(kc_house,type="html",title=" summary",
          out=("kc_house.htm"))
summary(kc_house)

### c :
###1 replacing missing values in sqft_living with their predicted value by linear regression
m.linear = lm(sqft_living ~ sqft_above, data=kc_house)
predict_sqft_living = predict(m.linear, newdata = kc_house,type = "response")
na_sqft = which(is.na(kc_house$sqft_living))
kc_house$sqft_living[na_sqft] = predict_sqft_living[na_sqft]
kc_house$sqft_living




### 2 replacing missing values in condition with the mode of this variable
na_condition = which(is.na(kc_house$condition))
summary(kc_house$condition) ###  checking which condition is the most common
kc_house$condition[na_condition] = 3
summary(kc_house$condition) ### checking there are no NA's

### 3 replacing missing values in yr_built with the median of yr_built
summary(kc_house$yr_built)
na_yr_built =which(is.na(kc_house$yr_built))
kc_house$yr_built[na_yr_built] = median(kc_house$yr_built, na.rm = T)
kc_house$yr_built

### Q 2

###a creating decision tree to predict price
set.seed(100)

sampleSize = floor( 0.7 * nrow(kc_house) )

trainIndex = sample(nrow(kc_house), size = sampleSize, replace = F )

train = kc_house[trainIndex, ]
test = kc_house[-trainIndex, ]

install.packages("rpart")
library(rpart)
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")

library(rattle)
library(rpart.plot)
library(RColorBrewer)

tree = rpart(price ~ bathrooms + bedrooms + sqft_living + grade + sqft_basement + yr_built + floors + condition 
             , data = train, method = "anova",  parms = list(split = "information"),
             control = rpart.control(maxdepth = 5))
fancyRpartPlot(tree)

### b generating price predictions 
test$price_predict_tree = predict(tree, newdata = test)

###c calculating MAE
abs_sum = 0 
for ( i in 1:nrow(test)){
  abs_sum = abs_sum + abs(test$price[i]- test$price_predict_tree[i])
}
mae_tree = abs_sum / nrow(test)
mae_tree

### Q 3
###a creating linear model to predict price
price_linear_model = lm(price ~ bathrooms + bedrooms + 
                          sqft_living + grade + sqft_basement + yr_built + floors + condition,
                        data = train)

###b creating a table of the regression output
stargazer(price_linear_model,type="html",title=" linear regression model",
          out=("linear_regression.htm"))

###c price prediction using the linear model
test$price_predict_lm = predict(price_linear_model, newdata = test)

###d calculating MAE
abs_sum2 = 0 
for ( i in 1:nrow(test)){
  abs_sum2 = abs_sum2 + abs(test$price[i]- test$price_predict_lm[i])
}
mae_lm = abs_sum2 / nrow(test)
mae_lm

###Q 4
###b using cross validation to determine which modelperforms better
AllShuffled = kc_house[sample(nrow(kc_house),replace=F),  ]

## create 20 equally sized folds
folds = cut( 1:nrow(AllShuffled), 
             breaks = 20, labels=F)

accuracyLinear = rep(0,20)
accuracyTree = rep(0,20)

for (i in 1:20) {
  ## segment the data by fold
  
  testIndexes = which(folds == i)
  
  testData = AllShuffled[testIndexes, ]
  trainData = AllShuffled[-testIndexes, ]
  ### run linear model
  linearModel = lm(price ~ bathrooms + bedrooms + 
                     sqft_living + grade + sqft_basement + yr_built + floors + condition,
                   data = trainData)
  linearPred = predict(linearModel, newdata = testData)
  
  abs_sum3 = 0 ### calculate MAE
  for ( j in 1:nrow(testData)){
    abs_sum3 = abs_sum3 + abs(testData$price[j]- linearPred[j])
  }
  mae_lm3 = abs_sum3 / nrow(testData)
  accuracyLinear[i] = mae_lm3
  
  ## run tree model
  tree3 = rpart(price ~ bathrooms + bedrooms + sqft_living + grade + sqft_basement 
                + yr_built + floors + condition 
               , data = trainData, method = "anova",  parms = list(split = "information"),
               control = rpart.control(maxdepth = 5))
  treePred = predict(tree3, newdata = testData)
  
  abs_sum4 = 0 ### calculate MAE
  for ( j in 1:nrow(testData)){
    abs_sum4 = abs_sum4 + abs(testData$price[j]- treePred[j])
  }
  mae_tree4 = abs_sum4 / nrow(testData) 
  accuracyTree[i] = mae_tree4
}
mean(accuracyLinear)
sd(accuracyLinear)

mean(accuracyTree)
sd(accuracyTree)