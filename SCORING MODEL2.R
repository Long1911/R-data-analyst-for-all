

# SESSION 5: SUPERVISED LEARNING: SCORING MODELs #
# Project: Housing price prediction

# Outcome:
# - Linear Regression model
# - Multiple Linear Regression
#   + Forward and Backward Selection
#   + Model Evaluation: RMSE and R2
#   + Collinearity check
#   + Train-test split
#   + In- and Out-sample Evaluation
#   + Bias and Variance Trade-Off
############################################

# Load data
housing <- read.csv("data/housing_data.csv")
head(housing)
names(housing)
# Question 1
mean(housing$SalePrice)
sd(housing$SalePrice)
hist(housing$SalePrice,40)
# Train-test split
set.seed(123)
N <- nrow(housing)
train_index <- runif(N) # Uniform random [0,1]
train_index <- train_index < 0.7
train <- housing[train_index, ]
test <- housing[!train_index, ]
# Question 2,3,4
plot(housing$LotArea,housing$SalePrice) # Outlier
# Question 2
model1 <- lm(SalePrice ~ LotArea+
               Neighborhood+Condition2+BldgType+HouseStyle+OverallQual+
               +YearBuilt+YearRemodAdd+RoofMatl+Exterior1st,
             data=train)
summary(model1) # remove: MSSubClass,Street,MSZoning,Condition1,LotConfig,LandSlope,
# remove: LotFrontage,OverallCond,RoofStyle,LotShape,LandContour
# Evaluation
ypred <- predict(model1,newdata = test) # Error
# Data transformation
# Convert RoofMatl because it has too many values with few observation (only value CompShg has many)
train$RoofMatl_new <- ifelse(train$RoofMatl=="CompShg",train$RoofMatl,"Others")
test$RoofMatl_new <- ifelse(test$RoofMatl=="CompShg",test$RoofMatl,"Others")
# Remove one observation in test because train$Exterior1st does not have value "AsphShn"
test <- test[test$Exterior1st != "AsphShn", ]
# Re-run the model
model1 <- lm(SalePrice ~ LotArea+
               Neighborhood+Condition2+BldgType+HouseStyle+OverallQual+
               +YearBuilt+YearRemodAdd+RoofMatl_new+Exterior1st,
             data=train)
summary(model1)
# Evaluation
ypred <- predict(model1,newdata = test) # Error
sd(test$SalePrice-ypred)  # RMSE = 38903
1-var(test$SalePrice-ypred)/var(test$SalePrice)  # R2 = 74.6%
# # Question 3: Backward Selection
# model2 <- lm(SalePrice ~ .-Utilities-Street-LandSlope-RoofMatl_new-Street-
#                MSSubClass-MSZoning-Condition1-LotConfig-LotFrontage-OverallCond-
#                RoofStyle-LotShape-LandContour-Neighborhood-Condition2-RoofMatl-
#                BldgType-HouseStyle-OverallQual-YearBuilt-YearRemodAdd-Exterior1st, data = train)
# summary(model2)
# Use Age, KM, Weight
names(train) # 
library("FNN")
train[,c(1,3,4,16,17,18,19)] <- scale(train[,c(1,3,4,16,17,18,19)]) # Scale all Xi
test[,c(1,3,4,16,17,18,19)] <- scale(test[,c(1,3,4,16,17,18,19)]) # Scale all Xi
knn.model <- knn.reg(train[,c(1,3,4,16,17,18,19)],test[,c(1,3,4,16,17,18,19)],
                     train$SalePrice,k=17)
ypred <- knn.model$pred
# Evaluation
sd(test$SalePrice-ypred)  # RMSE = 40597
1-var(test$SalePrice-ypred)/var(test$SalePrice)  # R2 = 72.3%
# k = ?
output <- c()
for (i in 1:50){
  knn.model <- knn.reg(train[,c(1,3,4,16,17,18,19)],test[,c(1,3,4,16,17,18,19)],
                       train$SalePrice,k=i)
  ypred <- knn.model$pred
  # Evaluation
  output <- c(output, sd(test$SalePrice-ypred))
}
plot(output,type = "b", col = "red")  # Optimal k = 1
which.min(output)
