# test ML models 2) Random forest
# this program runs Random Forest model with k-fold CV and LOOCV

# Load the necessary packages
library(randomForest)
library(lattice)
library(caret)
library(rpart)
library(tidyverse)

# Load the sample data
library(readxl)
srData <- read_excel("data/srNpredictorsW14_19_5.xlsx") %>%  
  select(myIncObs, dap, cumETin_14_19_5, avgETin_14_19_5, 
         avgHTR_14_19_5, nHrST25.35_14_19_5, nHrST25.30_14_19_5, 
         nHrSTa30_14_19_5,nHrRHa80_14_19_5, nHrRHa85_14_19_5, nHrRHa90_14_19_5,
         nHrRHa80T25.30_14_19_5, nHrRHa85T25.30_14_19_5, nHrRHa90T25.30_14_19_5,
         cumDRnI_14_19_5, avgDRnI_14_19_5, avgRH_14_19_5, avgST_14_19_5, 
         avgSTmin_14_19_5, avgSTmx_14_19_5, avgGWET_14_19_5,
         cumGWET_14_19_5, nDaysGWET_14_19_5)


# Define the model specification for RandomForest with 10-fold CV
set.seed(123)
model_spec <- train(myIncObs ~ ., data = srData, method = "rf", 
                    trControl = trainControl(method = "cv", number = 10))
model_spec

# store final model
model <- model_spec$finalModel
model

# Predict the response variable on the training data
predictions <- predict(model, srData)

# Evaluate the performance of the model using cross-validation
mean_squared_error <- mean((predictions - srData$myIncObs)^2)
print(paste("Mean Squared Error:", mean_squared_error))

# Calculate the mean decrease in impurity (MDI) for each predictor
mdi <- data.frame(feature = names(srData)[-1], importance = model$importance[,1])
mdi <- mdi[order(mdi$importance, decreasing = TRUE), ]
mdi

# Visualization: plot the varImportance graph
library(caret)
# Plot the feature importances
varImpPlot(model, sort = T)
varImp(model, sort = T)


##-----------------------------------------------------------------------------
# comparing 3 k-fold cross validation (10, 5 and 20)

# Define the fit control parameters for the random forest 
fitControl <- trainControl(method = "cv", number = 10)
# Fit the random forest using 10-fold cross-validation
set.seed(123)
model_spec10 <- train(myIncObs ~ ., data = srData, method = "rf", trControl = fitControl)
model_10_fold <- model_spec$finalModel

# Fit the random forest using 5-fold cross-validation
fitControl <- trainControl(method = "cv", number = 5)
set.seed(123)
model_spec5 <- train(myIncObs ~ ., data = srData, method = "rf", trControl = fitControl)
model_5_fold <- model_spec5$finalModel

# Fit the random forest using 20-fold cross-validation
fitControl <- trainControl(method = "cv", number = 20)
set.seed(123)
model_spec20 <- train(myIncObs ~ ., data = srData, method = "rf", trControl = fitControl)
model_20_fold <- model_spec20$finalModel

# Compare the results of the three random forest models
varImpPlot(model_10_fold, sort = T)
varImpPlot(model_5_fold, sort = T)
varImpPlot(model_20_fold, sort = T)


# Now LOOCV method-----------------------
library(rpart)
library(caret)

# LOOCV using rpart
fitControl <- trainControl(method = "LOOCV")
# Fit the random forest model using leave-one-out-cross-validation
set.seed(123)
model_specLC <- train(myIncObs ~ ., data = srData, method = "rf", trControl = fitControl)
model_LOOCV <- model_specLC$finalModel
varImpPlot(model_LOOCV, sort = T)

# Compare the results of the four models--------------------------------------------
cat("\nLeaveOneOut cross-validation results:")
print(model_specLC)
cat("10-fold cross-validation results:")
print(model_spec10)
cat("\n5-fold cross-validation results:")
print(model_spec5)
cat("\n20-fold cross-validation results:")
print(model_spec20)


##______________________________________________________________________________
#Try classification type model
# Create a factor variable with the classes
srData$myIncObs_class <- cut(srData$myIncObs, breaks = c(-1, 5, 15, 100), labels = c("low", "medium", "high"))
srData_c<- subset(srData, select = -c(myIncObs))


# Define the model specification
model_specCL <- train(myIncObs_class ~ ., data = srData_c, method = "rf", 
                      trControl = trainControl(method = "cv", number = 10))

# Fit the model
set.seed(123)
modelRF_C <- model_specCL$finalModel

# Predict the response variable on the training data
predictionsRF_C <- predict(modelRF_C, srData_c)

# Calculate the confusion matrix
confusion_matrix <- confusionMatrix(srData$myIncObs_class, predictionsRF_C)

# Create a confusion matrix plot
plot(confusion_matrix$table, main = "Confusion Matrix Plot")


# Plot the feature importances
varImpPlot(modelRF_C, sort = T)
