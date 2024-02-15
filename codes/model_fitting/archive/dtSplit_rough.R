library(tidyverse)
library(lattice)
library(caret)
library(rpart)

# Load the sample data
library(readxl)
srData <- read_excel("data/srNpredictorsW14_19_5.xlsx") %>%
  select(myIncObs, Location, year, cCover_num, dap, cumETin, avgETin,
         avgHTR, nHrT25.35, nHrT25.30, nHrTa30,nHrRHa80, nHrRHa85, nHrRHa90,
         nHrRHa80ST25.30, nHrRHa85ST25.30, nHrRHa90ST25.30, cumDRnI, avgDRnI,
         avgRH, avgST, avgSTmin, avgSTmx, avgGWET,
         cumGWET, nDaysGWET)

# Convert categorical columns to factors
srData$Location <- as.factor(srData$Location)
srData$year <- as.factor(srData$year)
srData$cCover_num <- as.factor(srData$cCover_num)

# These are the predictors with only nummeric data type
# srData <- read_excel("data/srNpredictorsW14_19_5.xlsx") %>%  
#   select(myIncObs, dap, cumETin, avgETin, 
#          avgHTR, nHrST25.35, nHrST25.30, nHrSTa30,nHrRHa80, nHrRHa85, nHrRHa90,
#          nHrRHa80ST25.30, nHrRHa85ST25.30, nHrRHa90ST25.30, cumDRnI, avgDRnI, 
#          avgRH, avgST, avgSTmin, avgSTmx, avgGWET,
#          cumGWET, nDaysGWET)


# Define the control parameters for the decision tree
control <- rpart.control(cp = 0.001, maxdepth = 2)

# Fit the decision tree with 10-fold cross-validation
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)
dt_1 <- train(myIncObs ~ ., data = srData, method = "rpart", 
              trControl = ctrl, control = control)

# Plot the decision tree from the final model with text labels (no complexity parameter table)
library(rpart.plot)
rpart.plot(dt_1$finalModel)

# try other settings for the decision tree model
tree1 <- rpart.plot(dt_1$finalModel, type = 2, extra = 1, tweak = 1.2,
                    clip.right.labs = FALSE, shadow.col = "gray",
                    branch.col = "black", main = "Decision Tree with One Split")


# Add a second split to the tree
CP <- dt_1$finalModel$cptable[2, "CP"]
dt_2 <- rpart(myIncObs ~ ., data = srData, method = "class",
              control = rpart.control(cp = CP))

# Plot the decision tree with two splits and similar aesthetics
tree2 <- rpart.plot(dt_2, type = 2, extra = 0)
                    
#===============================================================================

# Set seed and fit the first decision tree with 5-fold cross-validation
set.seed(123)
# Define the control parameters for the decision tree
control <- rpart.control(cp = 0.001, maxdepth = 4)

ctrl <- trainControl(method = "cv", number = 5)
dt_10 <- train(myIncObs ~ ., data = srData, method = "rpart", trControl = ctrl, control = control)

# Plot the first decision tree with similar aesthetics
tree1 <- rpart.plot(dt_10$finalModel, type = 0, extra = 1, tweak = 1.2,
                     main = "Decision Tree with One Split")

# Extract the complexity parameter (CP) value for the second split
cp <- dt_10$finalModel$cptable[2, "CP"]

# Fit the second decision tree with two splits using the extracted CP value
dt_2 <- rpart(myIncObs ~ ., data = srData, method = "class",
              control = rpart.control(cp = cp))

# Plot the second decision tree with similar aesthetics
tree2 <- rpart.plot(dt_2, type = 0, extra = 1, tweak = 1.2,
                    clip.right.labs = FALSE, shadow.col = "gray",
                    branch.col = "black", main = "Decision Tree with Two Splits")


#-------------------------------------------------------------------------------
# Get the mean and count of each leaf node in the second tree
leaf_stats <- data.frame(predict(dt_2, type = "prob", method = "class"),
                         node = 1:length(dt_2$frame$var))
leaf_stats <- leaf_stats[dt_2$frame$var == "<leaf>", ]
colnames(leaf_stats) <- c("Prob_0", "Prob_1", "Count", "Node")
leaf_stats$Mean <- leaf_stats$Prob_1

# Display the mean and count of each leaf node in the second tree
print(leaf_stats[, c("Mean", "Count")])





