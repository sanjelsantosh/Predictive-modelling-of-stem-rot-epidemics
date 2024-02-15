# Decision tree fitting with 10, 5, 20 and LOOCV cross validation methods

library(tidyverse)
library(lattice)
library(caret)
library(rpart)
library(rpart.plot)


# Load the sample data
library(readxl)

srData <- read_excel("data/data_for_model/srNpredictorsW14_19_5.xlsx") |> 
  select(myIncObs, location, year, canCover, dap, 
         cumETin, avgETin, cumSRad, avgHTR, nHrST25.35, nHrST25.30, nHrSTa30,
         nHrRHa80, nHrRHa85, nHrRHa90, nHrRHa80ST25.30, nHrRHa85ST25.30, nHrRHa90ST25.30, 
         cumDRnI, avgDRnI, avgRH, avgST, avgSTmin, avgSTmx, avgGWET, cumGWET, nDaysGWET)

# Convert categorical columns to factors
columns_to_factor <- c("location", "year", "canCover")
srData[columns_to_factor] <- lapply(srData[columns_to_factor], as.factor)


#no categorial predictors are included
# srData <- read_excel("data/srNpredictorsW14_19_5.xlsx") %>%  
#   select(myIncObs, dap, cumETin, avgETin, 
#          avgHTR, nHrST25.35, nHrST25.30, nHrSTa30,nHrRHa80, nHrRHa85, nHrRHa90,
#          nHrRHa80ST25.30, nHrRHa85ST25.30, nHrRHa90ST25.30, cumDRnI, avgDRnI, 
#          avgRH, avgST, avgSTmin, avgSTmx, avgGWET,
#          cumGWET, nDaysGWET)


# Convert categorical columns to factors
# Convert factors to numeric values

# Define the control parameters for the decision tree
control <- rpart.control(cp = 0.001, maxdepth = 2)

# Define the fit control parameters for the decision tree
fitControl <- trainControl(method = "cv", number = 10)

# Fit the decision tree model using 10-fold cross-validation
set.seed(123)
model_10_fold <- train(myIncObs ~ ., data = srData, method = "rpart", trControl = fitControl, control = control)
rpart.plot(model_10_fold$finalModel)

# Fit the decision tree model using 5-fold cross-validation
fitControl <- trainControl(method = "cv", number = 5)
model_5_fold <- train(myIncObs ~ ., data = srData, method = "rpart", trControl = fitControl, control = control)

# Fit the decision tree model using 20-fold cross-validation
fitControl <- trainControl(method = "cv", number = 20)
model_20_fold <- train(myIncObs ~ ., data = srData, method = "rpart", trControl = fitControl, control = control)

# LOOCV using rpart
fitcontrol <- trainControl(method = "LOOCV")
model_LOOCV <- train(myIncObs ~ ., data = srData, method = "rpart", trControl = fitcontrol, control = control)


# Compare the fit indices of the three models
results_10_fold <- model_10_fold$results
results_5_fold <- model_5_fold$results
results_20_fold <- model_20_fold$results
results_LOOCV <- model_LOOCV$results

# Print the three model results with k-fold variation
cat("10-fold cross-validation results:")
print(results_10_fold)
cat("\n5-fold cross-validation results:")
print(results_5_fold)
cat("\n20-fold cross-validation results:")
print(results_20_fold)
cat("\nLeave on out cross-validation results:")
print(results_LOOCV)

# print the decision tree
library(rpart.plot)
rpart.plot(model_10_fold$finalModel, type =1, main = "10-fold-CV")
rpart.plot(model_5_fold$finalModel, type = 1, main = "5-fold-CV")
rpart.plot(model_20_fold$finalModel,type = 5, main = "20-fold-CV")
rpart.plot(model_LOOCV$finalModel, type = 5,main = "model LOOCV")

#plot variable importance
plot(varImp(model_LOOCV), main = "model LOOCV")
plot(varImp(model_10_fold), main = "model 10_fold")
plot(varImp(model_5_fold), main = "model 5_fold")
plot(varImp(model_20_fold), main = "model 20_fold")

#
#windows(height = 2, width = 3)
#ggsave("T1.png", width = 3, height = 2, units = "in", dpi = 600)
#-----------------------------------
# Store variable importance from each model
varImp_10_fold <- varImp(model_10_fold)
varImp_5_fold <- varImp(model_5_fold)
varImp_20_fold <- varImp(model_20_fold)
varImp_LOOCV <- varImp(model_LOOCV)

# Combine variable importance data frames
varImp_all <- data.frame(
  Variable = row.names(varImp_10_fold$importance),
  `10-Fold` = varImp_10_fold$importance[, "Overall"],
  `20-Fold` = varImp_20_fold$importance[, "Overall"],
  `5-Fold` = varImp_5_fold$importance[, "Overall"],
  LOOCV = varImp_LOOCV$importance[, "Overall"]
)

# subset top ten for features for variable importance graph
varImp_clean <- varImp_all[c(-7, -8),]
varImp_10 <- varImp_clean[1:8,]
# Convert data frame to long format
varImp_all_long <- tidyr::gather(varImp_10, `Method (CV)`, Importance, -Variable)

# Create horizontal barplot
ggplot(varImp_all_long, aes(x = Importance, y = Variable, fill = `Method (CV)`)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.5) +
  xlab("Importance") + 
  ylab("") +
  #ggtitle("Variable Importance by Cross-Validation Method") +
  theme_classic() +
  scale_fill_manual(labels = c("LOOCV", "10-fold", "20-fold", "5-fold"),
                    values = c("#8DD3C7", "lightpink1", "bisque2", "skyblue1")) +
  #values = c("gold3", "skyblue1", "hotpink4", "palegreen4")) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.position = c(0.95, 0.05),  # Set legend position
        legend.justification = c("right", "bottom"),  # Set justification
        legend.box.just = "right")  # Align legend box to the right

# ============================================================================
#ggsave("varImp_plot.png", width = 6.4, height = 4.5, units = "in", dpi = 300)

#windows(height = 5.5, width = 6)


# Define the order of levels for Method (CV)

# Calculate accuracy for the three models
acc_10_fold <- postResample(predict(model_10_fold, srData), srData$myIncObs)
acc_5_fold <- postResample(predict(model_5_fold, srData), srData$myIncObs)
acc_20_fold <- postResample(predict(model_20_fold, srData), srData$myIncObs)
acc_LOOCV <- postResample(predict(model_LOOCV, srData), srData$myIncObs)

# Visualize the accuracy results
df <- data.frame(Model = c(rep("10-fold", length(acc_10_fold)),
                           rep("5-fold", length(acc_5_fold)),
                           rep("20-fold", length(acc_20_fold)),
                           rep("LOOCV", length(acc_LOOCV))),
                 Accuracy = c(acc_10_fold, acc_5_fold, acc_20_fold, acc_LOOCV))

ggplot(df, aes(x = Model, y = Accuracy)) + 
  geom_boxplot() + 
  labs(title = "Accuracy Comparison", y = "Accuracy")


#Print accuracy values
acc_10_fold
acc_5_fold 
acc_20_fold
acc_LOOCV 
# Set plot dimensions and resolution


