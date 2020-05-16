
# SESSION 8: DATA PREPARATION #
# Outcome:
# - Missing data
#   + Remove
#   + Imputation
# - Wrong data
# - Transform data
# - Practice building model after Data preparation
# - Statistical models
#   + Confidence invertal
#   + Hypothesis testing
############################################

# Clean environment
rm(list = ls())

# Cleaning data
rm(list = ls())
custdata <- read.table("data/custdata.tsv", header = TRUE,
                       sep = "\t")
summary(custdata)
# missing: is.employed, recent.move, housing.type,num.vehicle
colSums(is.na(custdata))  # count missing
# missing at categorical variables: is.employed
table(custdata$is.employed,useNA = "always")
# convert missing
custdata$is.employed <- ifelse(is.na(custdata$is.employed),"unknown",
                               custdata$is.employed)
table(custdata$is.employed,useNA = "always")
# housing.type missing
temp <- custdata[is.na(custdata$housing.type),]
colSums(is.na(temp))
# remove 56 obs.
custdata2 <- custdata[!is.na(custdata$housing.type),]

# Wrong data
hist(custdata2$age,40)
convert_value <- median(custdata2$age[custdata2$age>0&custdata2$age<100])
convert_value <- median(custdata2[custdata2$age>0&custdata2$age<100,c("age")])
# Median imputation
custdata2$age[custdata2$age==0|custdata2$age>100] <- convert_value

# Transform data
# - log transform
custdata2$income.log <- log(custdata2$income) #NA: negative and zero value
custdata2$income[custdata2$income<=0]
custdata2 <- custdata2[custdata2$income >= 0, ]  # remove 1 negative income
custdata2$income.log <- log(custdata2$income+0.1) # Add a small value
# - categorical to continuous (wrong format)
# - continuous to categorical
custdata2$num.vehicles.cat <- as.factor(custdata2$num.vehicles)
# - Grouping state.of.res
custdata2$state.of.res.new <- ifelse(custdata2$state.of.res %in%
              c("Iowa","Connecticut"),custdata2$state.of.res,"Others")

# Exercise: Running Classification models to forcast health.ins ----
table(custdata2$health.ins)
# Train-test split
set.seed(123)
N <- nrow(custdata2)
train_index <- runif(N) # Uniform random [0,1]
train_index <- train_index < 0.7
train <- custdata2[train_index, ]
test <- custdata2[!train_index, ] 
# Logistic
# Decision tree
# Bagging
# Random Forest
library("randomForest")
train$health.ins <- as.factor(train$health.ins)
train$is.employed <- as.factor(train$is.employed)
test$is.employed <- as.factor(test$is.employed)
train$state.of.res.new <- as.factor(train$state.of.res.new)
test$state.of.res.new <- as.factor(test$state.of.res.new)
rf <- randomForest(health.ins ~ sex+is.employed+income+
                     marital.stat+housing.type+recent.move+
                     num.vehicles+age+state.of.res.new,
                   data=train,
                   mtry=2,ntree=300,importance=TRUE)
rf
plot(rf)

# Evaluation
class_eval <- function(ypred,label){
  library("ROCR")
  pred <- prediction(ypred,label)
  plot(performance(pred,"tpr","fpr"))
  abline(0,1,lty=3)
  performance(pred,"auc")@y.values[[1]]
}

ypred <- predict(rf,type="prob")[,2]
class_eval(ypred,train$health.ins)
ypred <- predict(rf,newdata = test,type="prob")[,2]
class_eval(ypred,test$health.ins)
importance(rf)

# Naive Bayes 
library("e1071")  # Only use categorical variables
nb <- naiveBayes(health.ins~sex+is.employed+marital.stat+
                   housing.type+recent.move+num.vehicles.cat+
                   state.of.res.new,data=train)
ypred <- predict(nb,newdata = train,type="raw")[,2]
class_eval(ypred,train$health.ins)
ypred <- predict(nb,newdata = test,type="raw")[,2]
class_eval(ypred,test$health.ins)

# STATISTICS modes ----
s <- mean(custdata2$income)  #56379
sigma <- sd(custdata2$income)
n <- length(custdata2$income)
# lower bound
s - 1.96*sigma/sqrt(n)  #52147
# upper bound
s + 1.96*sigma/sqrt(n)  #60611

# HYPOTHESIS TESTING
custdata <- read.table("data/custdata.tsv", header = TRUE,
                       sep = "\t")
s <- mean(custdata$income)  #53504.77
s
sigma <- sd(custdata$income)
n <- length(custdata$income)
# lower bound
s - 1.96*sigma/sqrt(n)  #49446.4
# upper bound
s + 1.96*sigma/sqrt(n)  #57563.14

# HYPOTHESIS TESTING: is mu > 50k?
t.test <- (s-50000)/(sigma/sqrt(n))
t.test #1.69

# 
x <- custdata$income[custdata$housing.type == "Rented"]
x <- x[!is.na(x)]  # 364 customers Rented
s <- mean(x)  #36750.88
s
# is mu > 35k
n <- length(x)
sigma <- sd(x)
t.test <- (s-35000)/(sigma/sqrt(n))
t.test #1.69

# More Hypothesis testing exercises ----
x <- c(37,23,23,20,21,28,29,23,24,28,24,27,21,23)
s <- mean(x)
s  # 25 
# is mu > 22
n <- length(x)
sigma <- sd(x)
t.test <- (s-22)/(sigma/sqrt(n))
t.test #2.58 -> H1

# Using R-function
t.test(x, alternative = c("greater"), mu=22) # t = 2.5851

# 2-samples test
x <- custdata$income[custdata$housing.type == "Homeowner free and clear"]
x <- x[!is.na(x)]  # 157 customers Homeowner free and clear
mean(x) #55376.12
y <- custdata$income[custdata$housing.type == "Homeowner with mortgage/loan"]
y <- y[!is.na(y)]  # 364 customers Homeowner with mortgage/loan
mean(y) #74756.87
# Test if mu_x < mu_y?
t.test(x,y,alternative = c("less"))
#p-value = 0.002012 -> H1




