

# SESSION 7: SUPERVISED LEARNING: CLASSIFICATION MODELs #
# Outcome:
# - Logistics Regression model 
#   + Train-test split
#   + ROC, AUC
# - Decision Tree model
#   + Entropy
#   + Bias and Variance Trade-Off
# - Ensemble models:
#   + Bagging
#   + Random Forest
# - Naive Bayes model
############################################

# Clean environment
rm(list = ls())

# Supervised model - Logistic Regression ----
credit <- read.csv("data/germancredit_data.csv")
head(credit)
credit$rent_new <- ifelse(credit$rent,1,0)

# Train-test split
set.seed(123)
N <- nrow(credit)
train_index <- runif(N) # Uniform random [0,1]
train_index <- train_index < 0.7
train <- credit[train_index, ]
test <- credit[!train_index, ]  # Not train_index

# Re-run last logistic model
model2 <- glm(Default ~ duration +checkingstatus1+
                history+purpose+savings+others+
                otherplans+installment+age, 
              data = train, family = binomial)
summary(model2) # remove: age,status,employ,residence,
# property,cards,rent_new,foreign, housing

# Evaluation - In-sample
ypred <- predict(model2, type = "response")  # to return Default Probability
hist(ypred)
classification <- ifelse(ypred > 0.4,"YES","NO")

# Confusion Matrix
table(train$Default,classification)
# 
temp <- table(train$Default,classification)
(temp[1,1]+temp[2,2])/sum(temp)  # Accuracy = 77%
temp[2,2]/(temp[2,2]+temp[1,2])  # Precision = 60.4%
temp[2,2]/(temp[2,2]+temp[2,1])  # Recall = 62.8%
temp[1,1]*20 - temp[2,1]*40  # 5180tr = 413*20 - 77*40

# Higher Recall?
classification <- ifelse(ypred > 0.35,"YES","NO")
# Confusion Matrix
table(train$Default,classification)
# 
temp <- table(train$Default,classification)
(temp[1,1]+temp[2,2])/sum(temp)  # Accuracy = 76.3%
temp[2,2]/(temp[2,2]+temp[1,2])  # Precision = 57.9%
temp[2,2]/(temp[2,2]+temp[2,1])  # Recall = 70.5%
temp[1,1]*20 - temp[2,1]*40  # 5400tr = 392*20 - 61*40

# Exercise: Run a for loop to find highest profit

# ROC: Receiving Operation Curve
# install.packages("ROCR")
library("ROCR")
pred <- prediction(ypred,train$Default)
plot(performance(pred,"tpr","fpr"))
abline(0,1,lty=3)
performance(pred,"auc")@y.values[[1]]  # 81.5%

# Put this Evaluation into a function to used later
class_eval <- function(ypred,label){
  library("ROCR")
  pred <- prediction(ypred,label)
  plot(performance(pred,"tpr","fpr"))
  abline(0,1,lty=3)
  performance(pred,"auc")@y.values[[1]]
}

# Re-run Out-sample evaluation
ypred <- predict(model2,newdata=test,type="response")
class_eval(ypred,test$Default)

# Decision Tree model ----
#install.packages("rpart")
#install.packages("rpart.plot")
library(ggplot2)
ggplot(train)+geom_point(aes(x=age,y=duration,
                  color=Default),alpha=0.7)
library("rpart")
library("rpart.plot")
tree1 <- rpart(Default~age+duration,data=train,
               method="class",
               control=rpart.control(minsplit=40,cp=0.01))
plot(tree1)
rpart.plot(tree1,extra=1,tweak=1.2)

# Entropy
498/(498+207) # A: p_0
207/(498+207) # A: p_1
-(498/(498+207)*log(498/(498+207))+207/(498+207)*log(207/(498+207))) #E_A=0.605
-(243/(243+66)*log(243/(243+66))+66/(243+66)*log(66/(243+66))) #0.518
-(255/(255+141)*log(255/(255+141))+141/(255+141)*log(141/(255+141))) #0.651
0.605 - (243+66)/(498+207)*0.518 - (255+141)/(498+207)*0.651 # InfoGain=0.0122

# Prediction
ypred <- predict(tree1)[,2]
class_eval(ypred,train$Default)
ypred <- predict(tree1,newdata = test)[,2]
class_eval(ypred,test$Default)

# Tree 2
tree2 <- rpart(Default~.-rent_new,data=train,
               method="class",
               control=rpart.control(minsplit=10,cp=0.01))
plot(tree2)
rpart.plot(tree2,extra=1,tweak=1.2)
ypred <- predict(tree2)[,2]
class_eval(ypred,train$Default)
ypred <- predict(tree2,newdata = test)[,2]
class_eval(ypred,test$Default)

# Ensemble model: Bagging ----
set.seed(123)
#install.packages("randomForest")
library("randomForest")
train$Default <- as.factor(train$Default)
bagging <- randomForest(Default ~ .-rent_new,data=train,
                        mtry=21,ntree=200,importance=TRUE)
bagging
ypred <- predict(bagging,type="prob")[,2]
class_eval(ypred,train$Default)
ypred <- predict(bagging,newdata = test,type="prob")[,2]
class_eval(ypred,test$Default)

# Ensemble model: Random Forest ----
set.seed(123)
#install.packages("randomForest")
library("randomForest")
train$Default <- as.factor(train$Default)
rf <- randomForest(Default ~ .-rent_new,data=train,
                        mtry=5,ntree=200,importance=TRUE)
rf
ypred <- predict(rf,type="prob")[,2]
class_eval(ypred,train$Default)
ypred <- predict(rf,newdata = test,type="prob")[,2]
class_eval(ypred,test$Default)

# Naive Bayes model ----
table(train$checkingstatus1)
table(train$purpose)
# Probability estimation
prop.table(table(train$Default,train$checkingstatus1),1)
prop.table(table(train$Default,train$purpose),1)
prop.table(table(train$Default))
# 
#install.packages("e1071")
library("e1071")  # Only use categorical variables
nb <- naiveBayes(Default~.-duration-amount-installment-residence-
                   age-cards-liable-rent_new,data=train)
ypred <- predict(nb,newdata = train,type="raw")[,2]
class_eval(ypred,train$Default)
ypred <- predict(nb,newdata = test,type="raw")[,2]
class_eval(ypred,test$Default)
