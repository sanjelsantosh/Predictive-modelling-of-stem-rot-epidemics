# plot the decision tree

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

###

# Fit the decision tree with 1 split
mytree1 <- rpart(myIncObs ~ ., data = srData, method = "anova", 
                 control = rpart.control(cp = 0.01, minsplit = 6, maxdepth = 1)) # change for 1 split

# Print the tree
print(mytree1)


# Fit the decision tree with 2-split
mytree2 <- rpart(myIncObs ~ ., data = srData, method = "anova", 
                 control = rpart.control(cp = 0.01, minsplit = 6, maxdepth = 2)) # gives 2 split


# Save figures to print, save from files directly from the R's plot menu
printT1 <- rpart.plot(mytree1, type =5 , extra = 1, tweak = 1.2)
printT2 <- rpart.plot(mytree2, type =5 , extra = 1, tweak = 1.2)


##---------------