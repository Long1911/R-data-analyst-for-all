

# SESSION 6: SUPERVISED LEARNING: CLASSIFICATION MODELs #
# Outcome:
# - Logistics Regression model
#   + Sigmoid function
#   + Model Evaluation: Confusion matrix, Accuracy, Precision & Recall
############################################
# Clean environment
rm(list = ls())

# Load data ----
credit <- read.csv("data/germancredit_data.csv")
head(credit)

# 1st Logistics Regression ----
model1 <- glm(Default ~ duration + age, data = credit, family = binomial)
summary(model1)

# Sigmoid function - Examples
# P(Default): KH1: Age = 30, Duration 12
exp(0.037493*12-0.018306*30-1.024794)/(1+exp(0.037493*12-0.018306*30-1.024794)) #0.245
# P(Default): KH2: Age = 30, Duration 24
exp(0.037493*24-0.018306*30-1.024794)/(1+exp(0.037493*24-0.018306*30-1.024794)) #0.337
# P(Default): KH2: Age = 40, Duration 24
exp(0.037493*24-0.018306*40-1.024794)/(1+exp(0.037493*24-0.018306*40-1.024794)) #0.297

# Prediction
ypred <- predict(model1, type = "response")  # to return Default Probability
hist(ypred)

# Classification
classification <- ifelse(ypred > 0.4,"YES","NO")

# Confusion Matrix
table(credit$Default,classification)
temp <- table(credit$Default,classification)

# Evaluation: Accuracy, Precision and Recall
(temp[1,1]+temp[2,2])/sum(temp)  # Accuracy = 0.691
temp[2,2]/(temp[1,2]+temp[2,2])  # Precision = 0.47
temp[2,2]/(temp[2,1]+temp[2,2])  # Recall = 0.256

# Compute profit with model1 (not giving loan to YES)
614*20-223*40  # 3360 tr

# New threshold
classification <- ifelse(ypred > 0.35,"YES","NO")

# New Confusion Matrix
table(credit$Default,classification)
temp <- table(credit$Default,classification)
(temp[1,1]+temp[2,2])/sum(temp)  # Accuracy = 0.664
temp[2,2]/(temp[1,2]+temp[2,2])  # Precision = 0.428
temp[2,2]/(temp[2,1]+temp[2,2])  # Recall = 0.356
# Compute profit with model1 (not giving loan to YES)
557*20-193*40  # 3420

# Model2: forward selection ----
model2 <- glm(Default ~ duration +checkingstatus1+
                history+purpose+savings+installment+others+
                otherplans+housing+foreign+rent, data = credit, family = binomial)
summary(model2) # remove: age,status,employ,residence,property,cards,
#remove: job,liable,tele
table(credit$rent)

# Transform rent??
credit$rent_new <- ifelse(credit$rent,1,0)
model2 <- glm(Default ~ duration +checkingstatus1+
                history+purpose+savings+installment+others+
                otherplans+housing+foreign+rent_new, data = credit, family = binomial)
summary(model2) # remove: age,status,employ,residence,property,cards,
#remove: job,liable,tele

# Prediction
ypred <- predict(model2, type = "response")  # to return Default Probability
hist(ypred)

# Classification
classification <- ifelse(ypred > 0.4,"YES","NO")

# Confusion Matrix
table(credit$Default,classification)
573*20-114*40  # 6900tr
