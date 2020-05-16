

# SESSION 10: NEURAL NETWORK model #
# Outcome:
# - Artificial Neural network model (ANN)
# - Training ANN
# - Practice with real dataset
# - Limit of ANN and Introduction to Deep Learning
############################################

# Neural network model ----
# input
X <- matrix(c(0,0,1,0,1,1,1,0,1,1,1,0),ncol=3,byrow=TRUE)
y <- c(0,1,1,0)
cbind(X,y)
# Initialize W1
n_hidden <- 3
set.seed(123)
W1_rand <- runif(ncol(X)*n_hidden)
W1_rand <- matrix(W1_rand,nrow =ncol(X),byrow=TRUE)
W1_rand
# Initialize neural net, b1 = 0, b2 = 0 (for simplicity)
my_nn <- list(
  input = X,
  weights1 = W1_rand,
  weights2 = matrix(runif(n_hidden),ncol=1),
  y = y, # label
  output = matrix(rep(0,times=ncol(X)),ncol=1)
)
my_nn
# hidden layer
my_nn$input %*% my_nn$weights1
# Activation function
sigmoid <- function(x) {1/(1+exp(-x))}
# hidden layer out
sigmoid(my_nn$input %*% my_nn$weights1)
# output input
sigmoid(my_nn$input %*% my_nn$weights1) %*% my_nn$weights2
# output output  
my_nn$output <- sigmoid(sigmoid(my_nn$input %*% my_nn$weights1) %*% my_nn$weights2)
my_nn$output
# Loss
loss_function <- function(nn){sum((nn$y-nn$output)^2)}
loss_function(my_nn)
# Derivative
sigmoid_derivative <- function(x) {x*(1-x)}
# Feed forward
feedforward <- function(nn){
  nn$layer1 <- sigmoid(nn$input %*% nn$weights1)
  nn$output <- sigmoid(nn$layer1 %*% nn$weights2)
  nn
}
# test
feedforward(my_nn)
# Backpropagation
backprop <- function(nn) {
  # application of the chain rule to find derivative of the loss function with 
  # respect to weights2 and weights1
  d_weights2 <- (
    t(nn$layer1) %*%
      # `2 * (nn$y - nn$output)` is the derivative of the sigmoid loss function
      (2 * (nn$y - nn$output) *
         sigmoid_derivative(nn$output))
  )
  d_weights1 <- ( 2 * (nn$y - nn$output) * sigmoid_derivative(nn$output)) %*% 
    t(nn$weights2)
  d_weights1 <- d_weights1 * sigmoid_derivative(nn$layer1)
  d_weights1 <- t(nn$input) %*% d_weights1
  # update the weights using the derivative (slope) of the loss function
  nn$weights1 <- nn$weights1 + d_weights1
  nn$weights2 <- nn$weights2 + d_weights2
  nn
}

# Training
# number of times to perform feedforward and backpropagation
n <- 1500
# data frame to store the results of the loss function.
# this data frame is used to produce the plot in the 
# next code chunk
loss_df <- data.frame(
  iteration = 1:n,
  loss = vector("numeric", length = n)
)
for (i in seq_len(n)) {
  my_nn <- feedforward(my_nn)
  my_nn <- backprop(my_nn)
  # store the result of the loss function.  We will plot this later
  loss_df$loss[i] <- loss_function(my_nn)
}
# print the predicted outcome next to the actual outcome
data.frame(
  "Predicted" = round(my_nn$output, 3),
  "Actual" = y
)
# plot the cost
library("ggplot2")
ggplot(data = loss_df, aes(x=iteration, y=loss))+geom_line()

# Implement on German credit ----
credit <- read.csv("data/germancredit_data.csv")
head(credit)
X <- scale(credit[,c("duration","installment","age")])
y <- credit$Default
# Initialize W1
n_hidden <- 3
set.seed(123)
W1_rand <- runif(ncol(X)*n_hidden)
W1_rand <- matrix(W1_rand,nrow =ncol(X),byrow=TRUE)
W1_rand
# Initialize neural net, b1 = 0, b2 = 0 (for simplicity)
my_nn <- list(
  input = X,
  weights1 = W1_rand,
  weights2 = matrix(runif(n_hidden),ncol=1),
  y = y, # label
  output = matrix(rep(0,times=ncol(X)),ncol=1)
)
n <- 1500
loss_df <- data.frame(
  iteration = 1:n,
  loss = vector("numeric", length = n)
)
for (i in seq_len(n)) {
  my_nn <- feedforward(my_nn)
  my_nn <- backprop(my_nn)
  # store the result of the loss function.  We will plot this later
  loss_df$loss[i] <- loss_function(my_nn)
}
# print the predicted outcome next to the actual outcome
data.frame(
  "Predicted" = round(my_nn$output, 3),
  "Actual" = y
)
# plot the cost
library("ggplot2")
ggplot(data = loss_df, aes(x=iteration, y=loss))+geom_line()
class_eval <- function(ypred,label){
  library("ROCR")
  pred <- prediction(ypred,label)
  plot(performance(pred,"tpr","fpr"))
  abline(0,1,lty=3)
  performance(pred,"auc")@y.values[[1]]
}
# Out-sample
ypred <- my_nn$output
class_eval(ypred,y)
