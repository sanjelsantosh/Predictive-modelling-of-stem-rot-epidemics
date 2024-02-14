# This program calculates summary of specified windows using daily weather data
# Modify the function "summaryOfDaily"  to calculate the avg, max min or sum
# Make sure the variable names in the weather data match to the functions


#use this code to calculate summary of the following variables
# 1. GWET, Precipitation, ET and Solar_Radiation 

# this function is currently set to calculate the average ground wetness (GWET)

library(tidyverse)

# Function to calculate Avg of daily weather between specified start and end dates
# Adjust the needed additional filters such as RH
summaryOfDaily <- function(start_date, end_date, column_name, RH = NULL, data) {
  
  # Parameter Validation
  if (!all(c("TIMESTAMP", column_name) %in% colnames(data))) {
    stop("Data does not contain required columns.")
  }
  
  # Data Trimming
  data <- filter(data, TIMESTAMP > start_date, TIMESTAMP <= end_date)
  
  # RH Threshold Handling
  if (!is.null(RH)) {
    data <- filter(data, RH_15cm >= RH)
  }
  
  # Calculating Average (can be changed to calculate: min, max, sum)
  avg_value <- mean(data[[column_name]], na.rm = TRUE)
  #min_value <- min(data[[column_name]])
  #max_value <- max(data[[column_name]])
  #add_value <- sum(data[[column_name]])
  
  # Output Formatting
  time_period <- paste(start_date, 'to', end_date)
  which_column <- paste(column_name)
  result <- cbind(timePeriod = time_period, whichColumn = which_column, average = avg_value)
  
  return(result)
}

# load weather data; note this data set has both year data in the same file
weather <- read_csv("data/weather_data/daily/nasa_GWET/lo_NASA.csv",
                    col_types = cols(TIMESTAMP = col_date(format = "%m/%d/%Y")))

# Import disease assessment dates
start_to_end_data <- read.csv("data/diseaseAssessment_dates/lo_days_prior_to_loop_input.csv", header = TRUE)
disease_scoring_dates <- as.Date(start_to_end_data$Disease.scoring.dates, format = "%m/%d/%Y")

# Create windows
start_dba <- c(29, 33, 22, 26, 15, 19, 8, 12)
end_dba <- rep(c(1, 5), times = 4)
windo <- data.frame(start_dba, end_dba)

# Function to iterate the previously defined 'summaryOfDaily' function with the given conditions
# Function below counts the number of hours of specified conditions
# summary are based on the specified windows corresponding to the disease scoring dates

calculate_hours <- function(column_name, windo, disease_scoring_dates, data = weather) {
  results <- list()
  j <- 0
  for (k in 1:nrow(windo)) {
    store <- rep(NA, length(disease_scoring_dates))
    for (dd in 1:length(disease_scoring_dates)) {
      x <- as.numeric(windo[k, 'start_dba'])
      y <- as.numeric(windo[k, 'end_dba'])
      start_d <- as.Date(disease_scoring_dates[dd], format = "%Y-%m-%d") - x
      end_d <- as.Date(disease_scoring_dates[dd], format = "%Y-%m-%d") - y
      store[dd] <- list(summaryOfDaily(start_d, end_d, column_name, data = data))
    }
    results[[as.character(j)]] <- store
    j <- j + 1
  }
  return(results)
}

#specify the conditions to calculate the count of the conditions
results_list <- calculate_hours(column_name = "GWET_TOP", 
                                windo = windo, 
                                disease_scoring_dates= disease_scoring_dates)

results_list[[1]]
# Store the results_list in a data.frame for convenience

# Initialize an empty list to store the tally values from each list in results
tally_list <- list()

# Extract the tally values from each list in results and store them in tally_list
for (i in 1:length(results_list)) {
  tally_list[[i]] <- sapply(results_list[[i]], function(x) x[, "average"])
}

# Convert tally_list to a data.frame; use proper naming based on conditions
results_df <- as.data.frame(tally_list)
colnames(results_df) <- paste0("avgGWET_W", c(28, 28, 21, 21, 14, 14, 7, 7), "_", start_dba[1:8], "_", end_dba[1:8])
print(results_df)

# Write the results to a file
#write.csv(results_df, file = "store_predictors/LiveOak/lo_avgGWET_allWin.csv", row.names = FALSE)

#-------------------------------------------------------------------------------
# simple modifications for 

