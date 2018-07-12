---
output:
  html_document: default
  pdf_document: default
  word_document: default
---
# Peer-graded Assignment: Prediction Assignment Writeup


## Objectives
The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases

## Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

## Data Source
The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har.

## Reference
Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013. 

###Set up RStudio to work with this assignment.
```{r}
#load libraries
library(knitr)
library(caret)
library(randomForest)
library(gbm)
library(doMC)
registerDoMC(cores = 8)
#set echo to be = TRUE
opts_chunk$set(echo = TRUE)
```


###Load data for processing and analysis
Data loaded from primary CSV files, NAs removed, pml-training.csv partitioned 60/40 for training and test partitions for machine learning model development.
```{r}
training_data <- read.csv('./pml-training.csv', header=T, na.strings=c("NA","#DIV/0!", ""))
validation_data <- read.csv('./pml-testing.csv', header=T, na.strings=c("NA","#DIV/0!", ""))

training_data<-training_data[,colSums(is.na(training_data)) == 0]
validation_data <-validation_data[,colSums(is.na(validation_data)) == 0]

# Variables that do not impact movement analysis (user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, num_window) deleted.
training_data   <-training_data[,-c(1:7)]
validation_data <-validation_data[,-c(1:7)]

set.seed(54321)
div_data <- createDataPartition(y=training_data$classe, p=0.6, list=FALSE)
model_train <- training_data[div_data, ]
model_test <- training_data[-div_data, ]
```


## Random Forest Model
First ML technique used is random forest.  This choice is based on familiarity with this technique from other projects.
```{r}
Model_RF <- randomForest(classe ~. , data=model_train, method="class")

Predictions_RF <- predict(Model_RF, model_test, type = "class")

confusionMatrix(Predictions_RF, model_test$classe)
```


##GBM Model
Second choice of ML technique for this project.  Will compare with random forest.
```{r}

GBM_Model <- train(classe ~ ., method = "gbm", data = model_train, verbose = FALSE, trControl = trainControl(method = "cv", number = 3))

##set verbose to false to suppress data output on iterations for a cleaner report.
##change to TRUE if full information desired

Predictions_GBM <- predict(GBM_Model, model_test)

confusionMatrix(Predictions_GBM, model_test$classe)
```

##Prediction Model Selection
The random forest model has a predictive accuracy of 99.41% and a very high sensitivity (98.8%-99.79%) and specificity (99.69%-100%) with this dataset (p < 0.001).

The GBM model has a predictive accuracy of 96.32% and a very high sensitivity (94.20%-98.03%) and specificity (98.44%-99.73%) with this dataset (p < 0.001).

The random forest model has superiority in this comparison, so it was selected for further testing.  

## Test model for predictive ability
```{r}
finalPredictions <- predict(Model_RF, newdata=validation_data)
finalResults <- data.frame(
  problem_id=validation_data$problem_id,
  predicted=finalPredictions)
print(finalResults)
```