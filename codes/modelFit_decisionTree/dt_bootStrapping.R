# test ML models 1) Decision Tree
# this program runs decision tree model with bootstrapping 

library(tidyverse)
library(boot)
library(caret)
library(rpart)

# Load the sample data
library(readxl)
srData <- read_excel("data/srNpredictorsW14_19_5.xlsx") %>%
  select(myIncObs, Location, year, cCover_num, dap, cumETin, avgETin,
         avgHTR, nHrST25.35, nHrST25.30, nHrSTa30,nHrRHa80, nHrRHa85, nHrRHa90,
         nHrRHa80ST25.30, nHrRHa85ST25.30, nHrRHa90ST25.30, cumDRnI, avgDRnI,
         avgRH, avgST, avgSTmin, avgSTmx, avgGWET,
         cumGWET, nDaysGWET) %>%
  rename(canCover = cCover_num,
         location = Location)

# Convert categorical columns to factors
srData$location <- as.factor(srData$location)
srData$year <- as.factor(srData$year)
srData$canCover <- as.factor(srData$canCover)

# Define the bootstrapping method 
fitControl <- trainControl(method = "boot", number = 1000, p = 0.8)

# --------------------------------------------------------------------------------------------
# p refers to the proportion of the original dataset that is used for each bootstrap sample. 
# In this case, p = 0.8 means that each bootstrap sample will be created by randomly selecting 
# 80% of the original data, and the remaining 20% will be left out. 
# The number argument specifies the number of bootstrap samples to be taken.
# --------------------------------------------------------------------------------------------

# Fit the decision tree model using bootstrapping
set.seed(123)
model_boost <- train(myIncObs ~ ., data = srData, method = "rpart", 
                     trControl = fitControl, control = rpart.control(cp=0.001), 
                     metric = "RMSE")

model_boost$finalModel
model_boost$results
#plot the decision tree
library(rpart.plot)                     
tree <- T1 <- rpart.plot(model_boost$finalModel, type =2 , extra = 1, tweak = 1,
                         clip.right.labs = FALSE, shadow.col = "gray",
                         branch.col = "black", main ="Decision tree- boost")

# plot varImp of the bootstrapping result
plot(varImp(model_boost), main = "dTmodel-boost")
