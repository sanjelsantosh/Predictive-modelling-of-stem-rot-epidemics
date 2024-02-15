#Table 2 in the paper: Single-variable and multiple-variable model fit summary represented by the decision trees

library(dplyr)
library(readxl)

# load the data
datModelDT <- read_excel("data/data_for_model/srNpredictorsW14_19_5.xlsx") %>%  
  select(myIncObs, nHrRHa90ST25.30, nHrST25.35)

#--------------------------------------
# fit decision tree model with 1 splits
model1 = lm(myIncObs ~0+ I(nHrRHa90ST25.30 < 155),  data = datModelDT) 

summary(model1)

#--------------------------------------
# fit decision tree model with 2 splits
model2 = lm(myIncObs ~0+ I(nHrRHa90ST25.30 < 155) +
              I(nHrRHa90ST25.30 > 155 & nHrST25.35 > 323), data = datModelDT)


summary(model2)
#test of 1 vs 2
bs <- coef(model2)[-3]
V <- vcov(model2)[-3,-3]
tibble::tibble(
  diff_estim = diff(bs),
  diff_SE = sqrt(V[1, 1] + V[2, 2] - 2 * V[1, 2]),
  t_stat = diff_estim / diff_SE,
  df = df.residual(model2),
  p_value = 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
)

#test of 1 vs 3
bs <- coef(model2)[-2]
V <- vcov(model2)[-2,-2]
tibble::tibble(
  diff_estim = diff(bs),
  diff_SE = sqrt(V[1, 1] + V[2, 2] - 2 * V[1, 2]),
  t_stat = diff_estim / diff_SE,
  df = df.residual(model2),
  p_value = 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
)
#test of 2 vs 3
bs <- coef(model2)[-1]
V <- vcov(model2)[-1,-1]
tibble::tibble(
  diff_estim = diff(bs),
  diff_SE = sqrt(V[1, 1] + V[2, 2] - 2 * V[1, 2]),
  t_stat = diff_estim / diff_SE,
  df = df.residual(model2),
  p_value = 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
)

#==============================================================

# ============== add cross validation -> not working currently
# Remove any observations with missing values
datModelDT <- datModelDT[complete.cases(datModelDT),]

# Define the function to calculate RMSE
rmse <- function(predictions, actuals) {
  sqrt(mean((predictions - actuals)^2))
}

# Define the linear model
lm_model <- lm(myIncObs ~0+ I(nHrRHa90ST25.30 < 155),  data = datModelDT) 

library(boot)
# Use cross-validation to estimate RMSE
cv_results <- cv.glm(datModelDT, lm_model, K = 10, cost = rmse)

# Extract the cross-validated RMSE
cv_results

# Print the cross-validated RMSE
cat("Cross-validated RMSE:", cv_rmse, "\n")

