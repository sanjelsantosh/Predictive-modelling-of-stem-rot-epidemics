#Testing the distributions

# Load the sample data
library(dplyr)
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

# Assuming your response variable is stored in a vector called "response"
shapiro.test(srData$myIncObs)

# Assuming your response variable is stored in a vector called "response"
library(MASS)
resMod <- 
fit <- fitdistr(srData$myIncObs, "Poisson")
summary(fit)


# Assuming your response variable is stored in a vector called "response"

library(MCMCpack)
library(MASS)
library(coda)
fit <- MCMCbeta(srData$myIncObs)
summary(fit)

# Assuming your response variable is stored in a vector called "response"
library(MASS)
fit <- fitdistr(srData$myIncObs, "negative binomial")
summary(fit)

#=====================
# Load the pscl package
library(pscl)


# Convert percentage to proportion by dividing by 100
srData$myIncObs <- as.integer(srData$myIncObs)

# Fit a zero-inflated Poisson regression model
zip_model <- zeroinfl(myIncObs ~ ., data = srData, dist = "poisson")

# Fit a zero-inflated negative binomial regression model
zinb_model <- zeroinfl(myIncObs ~ ., data = srData, dist = "negbin")

# Fit a zero-inflated beta regression model
zib_model <- zeroinfl(myIncObs ~ ., data = srData, dist = "beta")

# Summarize the model results
summary(zip_model)
summary(zinb_model)
summary(zib_model)
