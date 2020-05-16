

# SESSION 5: SUPERVISED LEARNING: SCORING MODELs #
# Part of this section is taken from Chapter 3 in 
# the reference JOHANNES LEDOLTER's book: "DATA MINING AND BUSINESS ANALYTICS WITH R"

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
# Clean environment
rm(list = ls())

toyota <- read.csv("data/ToyotaCorolla.csv")
dim(toyota)  # 1436 old car, 10 features
head(toyota)

# EDA
plot(toyota$Age, toyota$Price)

# First model ----
model1 <- lm(Price ~ Age, data = toyota)
summary(model1)
abline(model1, col = "red")

# Exercise: predict old car price: Age = 10, Age = 20
-170.934*10 + 20294.059
-170.934*20 + 20294.059

# RMSE
sd(toyota$Price - (-170.934*toyota$Age + 20294.059)) # 1745

# Null model
sd(toyota$Price)  # 3626

# R2
1 - (sd(toyota$Price - (-170.934*toyota$Age + 20294.059))/sd(toyota$Price))^2

# Model 2: forward selection ----
model2 <- lm(Price ~ Age + KM + HP+ CC + Weight, 
             data = toyota)
summary(model2) # Remove MetColor, Automatic, Doors
# Categorical variable
table(toyota$FuelType)
# Model 3: 
model3 <- lm(Price ~ Age + KM + HP+ CC + Weight + FuelType, 
             data = toyota)
summary(model3)

# Test if CC is realy non-correlated
test_model <- lm(Price ~ CC, data = toyota)
summary(test_model)  # a_CC > 0

# Collinearity check ---- 
# install.packages("car")
library("car")
vif(model3) # FuelType is collinear! -> use model 2
# Model 4:
model4 <- lm(Price ~ Age+KM+HP+CC+Weight+I(Age^2)+I(1/KM), 
             data = toyota)
summary(model4)

# Plot multi scatter
pairs(toyota[,-c(4)])  # remove FuelType

# Train-test split ----
set.seed(123)
N <- nrow(toyota)
train_index <- runif(N) # Uniform random [0,1]
train_index <- train_index < 0.7
train <- toyota[train_index, ]
test <- toyota[!train_index, ]  # Not train_index
# 1017 train, 419 test
# Training
model4 <- lm(Price ~ Age+KM+HP+CC+Weight+I(Age^2)+I(1/KM), 
             data = train)
summary(model4)

# Prediction
ypred <- predict(model4, newdata = test)

# Out-sample Evaluation
sd(test$Price-ypred)  # RMSE = 1283
1-var(test$Price-ypred)/var(test$Price)  # R2 = 87%

# K-Nearest Neighbors (KNN) ----
#install.packages("FNN")
library("FNN")
plot(train$Age,train$KM) 

# Scaling data (Use Age, KM, Weight)
train[,c(2,3,10)] <- scale(train[,c(2,3,10)]) # Scale all Xi
test[,c(2,3,10)] <- scale(test[,c(2,3,10)]) # Scale all Xi

# Modeling
knn.model <- knn.reg(train[,c(2,3,10)],test[,c(2,3,10)],
                     train$Price,k=3)
ypred <- knn.model$pred

# Out-sample Evaluation
sd(test$Price-ypred)  # RMSE = 1439
1-var(test$Price-ypred)/var(test$Price)  # R2 = 83.9%

# Parameter optimization (k = ?)
output <- c()
for (i in 1:50){
  knn.model <- knn.reg(train[,c(2,3,10)],test[,c(2,3,10)],
                       train$Price,k=i)
  ypred <- knn.model$pred
  # Evaluation
  output <- c(output, sd(test$Price-ypred))
}
plot(output,type = "b", col = "red")
# Re-run model
knn.model <- knn.reg(train[,c(2,3,10)],test[,c(2,3,10)],
                     train$Price,k=11)
ypred <- knn.model$pred

# Evaluation
sd(test$Price-ypred)  # RMSE = 1439
1-var(test$Price-ypred)/var(test$Price)  # R2 = 83.9%



