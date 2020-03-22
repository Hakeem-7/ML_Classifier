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
svm_model <- svm(a4a_train$Label~., data = a4a_train, kernel = "linear") #linear Kernel
summary(svm_model)

#Confusion Matrix
pred1 <- predict(svm_model, a4a_test)
pred_table <- table(Predicted = pred1, Actual = a4a_test$Label)
pred_table

(mean(pred1 == a4a_test$Label))*100 #Classification Accuracy is approx. 84%(RBF), 84.4% (Linear), 83.7%(Sigmoid)




#Perceptron Algorithm



















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






