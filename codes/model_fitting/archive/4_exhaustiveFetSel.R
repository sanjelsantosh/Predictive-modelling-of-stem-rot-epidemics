
# this program runs exhaustive feature selection

# Load the sample data
library(dplyr)
library(readxl)

# load data from sheet1
srData <- read_excel("data/srNpredictorsW14_19_5.xlsx")%>%  
  select(myIncObs, dap, cCover_num,  
         avgHTR_14_19_5, nHrST25.35_14_19_5, nHrST25.30_14_19_5, 
         nHrSTa30_14_19_5, nHrRHa80_14_19_5, nHrRHa85_14_19_5, nHrRHa90_14_19_5,
         nHrRHa80T25.30_14_19_5, nHrRHa85T25.30_14_19_5, nHrRHa90T25.30_14_19_5,
         cumDRnI_14_19_5, avgDRnI_14_19_5, avgRH_14_19_5, avgST_14_19_5, 
         avgGWET_14_19_5, cumGWET_14_19_5, nDaysGWET_14_19_5)

#---------------------------------------------------------------------------
library(rJava)
library(leaps)
library(glmulti)

set.seed(123) # for reproducibility
  
# Perform exhaustive search using AIC
glmulti.out <- glmulti(myIncObs ~ ., data = srData, 
                       level = 1, #starts by fitting model with only 1 predictor,
                       crit = "aic")                     #then with 2 and so on
                       

summary(glmulti.out)
# write glmulti.out to a csv file
#capture.output(glmulti.out, file = "glmulti_output.csv")


#--------------------------------------------------------------------
# Test the performance of the model using cross-validation.
library(lattice)
library(caret)

# create the formula for the best model
best_formula <- myIncObs ~ 1 + dap + cCover_num + nHrST25.30_14_19_5 + nHrSTa30_14_19_5 +
  nHrRHa80_14_19_5 + nHrRHa80T25.30_14_19_5 + avgST_14_19_5

# create training control object
train_5_control <- trainControl(method = "cv", number = 5) # for 5-fold cross-validation
train_10_control <- trainControl(method = "cv", number = 10) # for 10-fold cross-validation
train_20_control <- trainControl(method = "cv", number = 20) # for 20-fold cross-validation

# train the model with cross-validation
cv_5_model <- train(best_formula, data = srData, method = "glm", trControl = train_5_control)
cv_10_model <- train(best_formula, data = srData, method = "glm", trControl = train_10_control)
cv_20_model <- train(best_formula, data = srData, method = "glm", trControl = train_20_control)

# view the results
summary(cv_5_model)
summary(cv_10_model)
summary(cv_20_model)


library(ggplot2)

# get the predicted values
pred <- predict(cv_10_model, newdata = srData)

# create a data frame with observed and predicted values
data <- data.frame(observed = srData$myIncObs, predicted = pred)

# create scatter plot of observed vs predicted values
ggplot(data, aes(x = predicted, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Predicted values", y = "Observed values", 
       title = "Observed vs Predicted values (10-fold CV)")

