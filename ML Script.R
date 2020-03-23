# Binary Classification using Perceptron, SVM, and Decision Tree

# Raw Data Preprocessing

#  Raw data is in libsvm format
# filename: an input file name
# dimensionality: a number of columns, excluding label

# returns a matrix with a label in the first row

read.libsvm = function( filename, dimensionality ) {
  
  content = readLines(filename )
  num_lines = length( content )
  yx = matrix( 0, num_lines, dimensionality + 1 )
  
  # loop over lines
  for ( i in 1:num_lines ) {
    
    # split by spaces
    line = as.vector( strsplit( content[i], ' ' )[[1]])
    
    # save label
    yx[i,1] = as.numeric( line[[1]] )
    
    # loop over values
    for ( j in 2:length( line )) {
      
      # split by colon
      index_value = strsplit( line[j], ':' )[[1]]
      
      index = as.numeric( index_value[1] ) + 1		# +1 because label goes first
      value = as.numeric( index_value[2] )
      
      yx[i, index] = value
    }
  }
  
  return(yx)
}


train <- read.libsvm("https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/a4a", 123)
dim(train)
names(train)


write.csv(train, "C:\\Github\\Machine_Learning\\MyD.csv", row.names = FALSE)

train1 <- read.csv("a4aTraining.csv", header = TRUE)
names(train1)

#Class variables function
Classes <- function(data){
  Class_variables <- sapply(data, function(x) class(x)) 
  return(Class_variables)
}
Classes(train1)


test <- read.libsvm("a4a.t", 123)
dim(test)
names(test)

write.csv(test, "C:\\Github\\Machine_Learning\\MyDt.csv", row.names = FALSE)

test1 <- read.csv("a4aTesting.csv", header = TRUE)
Classes(test1)



# Decision Tree (Binary Classification)
#Decision Tree Algo using the C5.0 algorithm by J. Ross Quinlanb (Industry Standard) - Divide and Conquer

curve(-x * log2(x) - (1 - x) * log2(1 - x),
      col = "darkred", xlab = "x", ylab = "Entropy", lwd = 3) #Illustration of entropy; 50-50 split results in maximum entropy


library(dplyr)
a4a_train <- train1 %>%
  mutate_at(vars(Label), 
            funs(factor))   #Transforms the label integer variable to a factor variable

a4a_test <- test1 %>%
  mutate_at(vars(Label), 
            funs(factor))

Classes(a4a_train)
Classes(a4a_test)

#Decision tree algo that implements entropy criterion

library(C50)
model <- C5.0(a4a_train[-1], a4a_train$Label) #Decision tree model
model
summary(model)

# Model performance evaluation
str(a4a_test)

model_pred <- predict(model, a4a_test)

library(gmodels)

#Confusion Matrix

CrossTable(a4a_test$Label, model_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual')) 

(mean(model_pred == a4a_test$Label))*100 #Classification Accuracy is approx. 83%


# Model performance improvement - Boosting the accuracy of decision trees
#C5.0 algorithm improved upon the C4.5 algo through the adoption of adaptive boosting.
#Research suggested that "trials = 10" improves tree accuracy by 25%)

model_boost <- C5.0(a4a_train[-1], a4a_train$Label, trials = 10) 
model_boost
model_boost_pred <- predict(model_boost, a4a_test)
CrossTable(a4a_test$Label, model_boost_pred, prop.r = F, prop.c = F, prop.chisq = F,
           dnn = c("predicted","actual"))

(mean(model_boost_pred == a4a_test$Label))*100 #Classification Accuracy is approx. 84.3%
#Boosting the tree barely produced a significant improvement in the tree




# Support Vector Machine (Binary Classification) - Finding optimal separating hyperplane while maximizing margin

#install.packages("e1071")

library(e1071)

set.seed(7)
svm_model <- svm(a4a_train$Label~., data = a4a_train, kernel = "linear", scale = TRUE) #linear Kernel
summary(svm_model)

#Confusion Matrix
pred1 <- predict(svm_model, a4a_test)
pred_table <- table(Predicted = pred1, Actual = a4a_test$Label)
pred_table

(mean(pred1 == a4a_test$Label))*100 #Classification Accuracy is approx. 84%(RBF), 84.4% (Linear), 83.7%(Sigmoid)





#Perceptron Algorithm

# write function that takes in the data frame, learning rate - eta, and number of epochs - n.iter and updates the weight factor. 
# To obtain the final weight and the number of epochs required for the weight to converge

#Here we separate the attributes from the class

x <- a4a_train[-1] 
names(x) <- tolower(names(x))

# create species labels
y <- train1$Label   

perceptron <- function(x, y, eta, n_iter) {
  
  # initialize weight vector
  weight <- rep(0, dim(x)[2] + 1)
  errors <- rep(0, n_iter)
  
  
  # loop over number of epochs niter
  for (jj in 1:n_iter) {
    
    # loop through training data set
    for (ii in 1:length(y)) {
      
      # Predict binary label using Heaviside activation 
      # function
      z <- sum(weight[2:length(weight)] * 
                 as.numeric(x[ii,])) + weight[1]
      if(z < 0) {
        y_pred <- -1
      } else {
        y_pred <- 1
      }
      
      # Change weight - the formula doesn't do anything 
      # if the predicted value is correct
      weight_diff <- eta * (y[ii] - y_pred) * 
        c(1, as.numeric(x[ii, ]))
      weight <- weight + weight_diff
      
      # Update error function
      if ((y[ii] - y_pred) != 0.0) {
        errors[jj] <- errors[jj] + 1
      }
      
    }
  }
  
  # weight to decide between the two species 
  print(weight)
  return(errors)
}

err_train_a4a <- perceptron(x,y,0.9,10)

# Model evaluation













# Multi-class Classification using Perceptron, SVM, and Decision Tree

#Decision Tree Algo using the C5.0 algorithm by J. Ross Quinlanb (Industry Standard) - Divide and Conquer

iris = read.csv("Iris - data.csv", header = TRUE)
names(iris)
Classes(iris)
str(iris)

set.seed(7)
iris_sampling <- sample(150,120)
str(iris_sampling) #looks randomized

iris_train <- iris[iris_sampling,]
iris_test <- iris[-iris_sampling,]

iris_model <- C5.0(iris_train[-5], iris_train$Species)
iris_model 
summary(iris_model) #Training error is 2.5%

#Evaluate Model Performance

iris_pred <- predict(iris_model, iris_test)
library(gmodels)
CrossTable(iris_test$Species, iris_pred, prop.r = FALSE,
           prop.c = FALSE, prop.chisq = FALSE,
           dnn = c("predicted", "actual"))

(mean(iris_pred == iris_test$Species))*100 #Classification Accuracy is approx. 97%





# Support Vector Machine (Multi-Class Classification) - Finding optimal separating hyperplane while maximizing margin

#Visualization of the Iris data

library(ggplot2)
qplot(iris$PetalLength..cm., iris$PetalWidth..cm., data = iris,
      color = iris$Species)



library(e1071)

set.seed(7)
iris_model1 <- svm(iris_train$Species~., data = iris_train, kernel = "linear") #linear Kernel
summary(iris_model1)

#Confusion Matrix
pred2 <- predict(iris_model1, iris_test)
pred_table1 <- table(Predicted = pred2, Actual = iris_test$Species)
pred_table1

(mean(pred2 == iris_test$Species))*100 #Classification Accuracy is 100%(RBF), 100%(Linear), 96.7%(Sigmoid)







#Perceptron Algorithm

summary(iris)
#create sub-dataframe

iris_subdf1 <- iris[1:100, c(1,2,3,4,5)]
names(iris_subdf1)
#generate a training a training and testing data set from the iris sub-frame

set.seed(7)
sbf_sample <- sample(100,70)

str(sbf_sample) #looks randomized

iris_subdf1_train <- iris_subdf1[sbf_sample,] #70 observations
iris_subdf1_test <- iris_subdf1[-sbf_sample,] #30 observations

str(iris_subdf1_test)






library(ggplot2)
ggplot(iris_subdf1, aes(x = iris_subdf1$SepalLength..cm., y = iris_subdf1$PetalLength..cm.)) + 
  geom_point(aes(colour=iris_subdf1$Species, shape = iris_subdf1$Species), size = 3) +
  xlab("sepal length") + 
  ylab("Petal width") + 
  ggtitle("Iris Species")   #The data looks linearly separable



iris_subdf1_train[, 6] <- 1     #initialize
iris_subdf1_train[iris_subdf1_train[, 5] == "Iris-setosa", 6] <- -1  #setosa is now -1

x <- iris_subdf1_train[, c(1,2,3,4)]    #attributes
y <- iris_subdf1_train[, 6]          #class values
tail(y)


# write function that takes in the data frame, learning rate - eta, and number of epochs - n.iter and updates the weight factor. 
# To obtain the final weight and the number of epochs required for the weight to converge

#Here we separate the attributes from the class

perceptron_iris <- function(x, y, eta, n_iter) {
  
  # initialize weight vector
  weight <- rep(0, dim(x)[2] + 1)
  errors <- rep(0, n_iter)
  
  
  # loop over number of epochs niter
  for (jj in 1:n_iter) {
    
    # loop through training data set
    for (ii in 1:length(y)) {
      
      # Predict binary label using Heaviside activation 
      # function
      z <- sum(weight[2:length(weight)] * 
                 as.numeric(x[ii,])) + weight[1]
      if(z < 0) {
        y_pred <- -1
      } else {
        y_pred <- 1
      }
      
  # Change weight - the formula doesn't do anything 
  # if the predicted value is correct
      weight_diff <- eta * (y[ii] - y_pred) * 
        c(1, as.numeric(x[ii, ]))
      weight <- weight + weight_diff
      
      # Update error function
      if ((y[ii] - y_pred) != 0.0) {
        errors[jj] <- errors[jj] + 1
      }
      
    }
  }
  
  # weight to decide between the two species 
  print(weight)
  return(errors)
}

iris_subdf1_train_err <- perceptron_iris(x,y,1,40)

#Visualization

plot(1:40,iris_subdf1_train_err, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors")
title("Errors vs epoch - learning rate eta = 1")



#install.packages("optimbase")

library(optimbase)

# Perceptron evaluation

w1 <- c(-2.0,-5.2,-11.8,18.2,8.2)     #weight for classifying setosa vs others




#Let us test the accuracy of the first perceptron


iris_subdf1_test[, 6] <- 1     #initialize
iris_subdf1_test[iris_subdf1_test[, 5] == "Iris-setosa", 6] <- -1  #setosa is now -1

x <- iris_subdf1_test[, c(1,2,3,4)]    #attributes
y <- iris_subdf1_test[, 6]          #class values

x[,5] <- 1

colnames(x) <- NULL
p1<-zeros(30, 1)
for (ii in 1:30) {
  p1[ii,1]<-w1%*%as.double(x[ii,])
}
p1[p1 >= 0] = 1
p1[p1< 0] = -1

pred_accuracy = sum(p1==y)/30
pred_accuracy  #Class accuracy of 53.3% on classifying setosa vs others

















#Hyperplane for iris-viginica versus iris-setosa OR iris-vesicolor




#Data visualization using ggplot2

ggplot(iris, aes(x = iris$SepalLength..cm., y = iris$PetalLength..cm.)) + 
  geom_point(aes(colour=iris$Species, shape=iris$Species), size = 3) +
  xlab("sepal length") + 
  ylab("petal length") + 
  ggtitle("Species vs sepal and petal lengths") #Doesn't seem to be linearly separable


#Keeping all the attributes

iris_subdf2 <- iris[, c(1,2,3,4,5)]
names(iris_subdf2) <- c("sepal", "petal", "species")


set.seed(7)
sbf_sample2 <- sample(150,111)

str(sbf_sample2) #looks randomized

iris_subdf2_train <- iris_subdf2[sbf_sample2,] #111 observations
iris_subdf2_test <- iris_subdf2[-sbf_sample2,] #39 observations

str(iris_subdf2_test)

# Training the second perceptron


iris_subdf2_train[, 6] <- 1     #initialize
iris_subdf2_train[iris_subdf2_train[, 5] == "Iris-virginica", 6] <- -1  #Virginica is now 1

x <- iris_subdf2_train[, c(1,2,3,4)]    #attributes
y <- iris_subdf2_train[, 6]          #class values


# compute and plot error

irissubdf2_train_err <- perceptron_iris(x, y, 0.01, 50)

#Visualization

plot(1:50, irissubdf2_train_err, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors")
title("Errors in differentiating Virginica vs epoch - learning rate eta = 0.01") #Minimum error is 2, but the weight converged

w2 <- c(0.180,0.490,0.768,-0.970,-0.468)  #Weight of the second perceptron

# Model Evaluation

#Let us test the accuracy of the second perceptron


iris_subdf2_test[, 6] <- 1     #initialize
iris_subdf2_test[iris_subdf2_test[, 5] == "Iris-virginica", 6] <- -1  #Virginica is now -1

x <- iris_subdf2_test[, c(1,2,3,4)]    #attributes
y <- iris_subdf2_test[, 6]          #class values

x[,5] <- 1

colnames(x) <- NULL
p1<-zeros(39, 1)
for (ii in 1:39) {
  p1[ii,1]<-w1%*%as.double(x[ii,])
}
p1[p1 >= 0] = 1
p1[p1< 0] = -1

pred_accuracy = sum(p1==y)/39
pred_accuracy  #Class accuracy of 28.2% on classifying setosa vs others

