# This program conducts regularized regression

# Load required libraries
library(glmnet)
library(dplyr)

# Load the sample data
library(readxl)
srData <- read_excel("data/srNpredictorsW14_19_5.xlsx") %>%  
  select(myIncObs, Location, year, cCover_num, dap, cumETin, avgETin, 
         avgHTR, nHrST25.35, nHrST25.30, nHrSTa30,nHrRHa80, nHrRHa85, nHrRHa90,
         nHrRHa80ST25.30, nHrRHa85ST25.30, nHrRHa90ST25.30, cumDRnI, avgDRnI, 
         avgRH, avgST, avgSTmin, avgSTmx, avgGWET,
         cumGWET, nDaysGWET)

# Convert categorical columns to factors
srData$Location <- as.factor(srData$Location)
srData$year <- as.factor(srData$year)
srData$cCover_num <- as.factor(srData$cCover_num)

# Set the response variable and predictors
response_var <- "myIncObs"
numeric_vars <- setdiff(colnames(srData), c(response_var, "Location", "year", "cCover_num"))
categorical_vars <- c("Location", "year", "cCover_num")

# Create a matrix of predictors
X <- model.matrix(~. - 1, data = srData[, c(response_var, numeric_vars, categorical_vars)])

# Create a vector of response variable
Y <- srData[[response_var]]

# Perform L1 regularization with leave-one-out cross-validation
cv.glmnet1 <- cv.glmnet(x = X, y = Y, alpha = 1, type.measure = "mse",
                        nfolds = nrow(X), grouped = FALSE,
                        standardize = TRUE, intercept = FALSE)
# Store the feature importance for L1 regularization
l1_imp <- data.frame(var = colnames(X)[-1], coef = as.data.frame(cv.glmnet1$beta[, which.min(cv.glmnet1$cvm)])[, 1])



# Perform L2 regularization with leave-one-out cross-validation
cv.glmnet2 <- cv.glmnet(x = X, y = Y, alpha = 0, type.measure = "mse",
                        nfolds = nrow(X), grouped = FALSE,
                        standardize = TRUE, intercept = FALSE)

# Store the feature importance for L2 regularization
l2_imp <- data.frame(var = colnames(X)[-1], coef = as.data.frame(cv.glmnet2$beta[, cv.glmnet2$lambda.1se])[,1])

# Perform elastic net with leave-one-out cross-validation
cv.glmnet3 <- cv.glmnet(x = X, y = Y, alpha = 0.5, type.measure = "mse",
                        nfolds = nrow(X), grouped = FALSE,
                        standardize = TRUE, intercept = FALSE)

# Store the feature importance for elastic net
en_imp <- data.frame(var = colnames(X)[-1], coef = as.data.frame(cv.glmnet3$beta[, cv.glmnet3$lambda.1se])[,1])
#To store the model fit indices for each method, you can extract the mean cross-validated error (cv.glmnet$lambda.min) 
#and the R-squared value (cv.glmnet$glmnet.fit$dev.ratio) for each of the three methods, 
#like below:
# Extract model fit indices
l1_error <- cv.glmnet1$lambda.min
l1_rsq <- cv.glmnet1$glmnet.fit$dev.ratio

 
# Store the model fit criteria in a data frame
model_fit <- data.frame(model = c("L1", "L2", "Elastic Net"),
                        mse = c(cv.glmnet1$cvm[cv.glmnet1$lambda == cv.glmnet1$lambda.1se],
                                cv.glmnet2$cvm[cv.glmnet2$lambda == cv.glmnet2$lambda.1se],
                                cv.glmnet3$cvm[cv.glmnet3$lambda == cv.glmnet3$lambda.1se]),
                        lambda = c(cv.glmnet1$lambda.1se, cv.glmnet2$lambda.1se, cv.glmnet3$lambda.1se))

# Print the results
l1_imp
l2_imp
en_imp
model_fit
