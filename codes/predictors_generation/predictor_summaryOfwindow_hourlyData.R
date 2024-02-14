# This program calculates summary of specified windows using hourly weather data
# Modify the summaryOfDaily function to calculate average, max, min or sum 
# Specify the variable name in your weather data
# this version is set to calculate average temp

# use this code for the predictors of :
# 1. Soil temperature(ST), Relative humidity(RH) and volumeric water content (VWC)
# All three predictors are calculated using hourly log


library(tidyverse)

# Function to load weather data
load_weather_data <- function(files) {
  weather_data <- lapply(files, function(file) {
    read_csv(file, col_types = cols(TIMESTAMP = col_datetime(format = "%m/%d/%Y %H:%M")))
  })
  bind_rows(weather_data)
}

# Constants
inch_to_mm <- 25.4 # mm conversion from inch

# Function to calculate hourly summary from 15-min log
calculate_hourly_summary <- function(data) {
  data |> 
    mutate(days = floor_date(TIMESTAMP, "day"),
           hours = floor_date(TIMESTAMP, "hour")) |> 
    group_by(days, hours) |> 
    summarize(across(AirT_15cm_C: `Rain(in)`, \(x) mean(x, na.rm = TRUE))) |> 
    mutate(ppt = `Rain(in)` * inch_to_mm) 
}

# Function for calculating Avg of hourly temperature in a window
# This function can be easily changed for avg.daily.min or avg.daily.max
# Possible to add any threshold like RH, change RH = threshold value 

summaryOfDaily <- function(start_date, end_date, start_time, end_time, column_name, RH = NULL, data) {
  
  # Parameter Validation
  if(start_time > 24 || start_time < 0 || end_time > 24 || end_time < 0){
    stop('Please choose time that is from 0-24')
  }
  if (!all(c("days", "hours", column_name) %in% colnames(data))) {
    stop("Data does not contain required columns.")
  }
  
  # Data Trimming
  data <- filter(data, days > start_date, days <= end_date)
  
  # Handling Time Periods Crossing Midnight
  data <- if (start_time > end_time) {
    filter(data, hour(hours) >= start_time | hour(hours) <= end_time)
  } else {
    filter(data, hour(hours) >= start_time, hour(hours) <= end_time)
  }
  
  # RH Threshold Handling
  if (!is.null(RH)) {
    data <- filter(data, RH_15cm >= RH)
  }
  
  # Calculating Average (can be changed to calculate: min, max, sum)
  avg_value <- mean(data[[column_name]])
  #min_value <- min(data[[column_name]])
  #max_value <- max(data[[column_name]])
  #add_value <- sum(data[[column_name]])
  
  # Output Formatting
  time_period <- paste(start_date, 'to', end_date)
  which_column <- paste(column_name)
  result <- cbind(timePeriod = time_period, whichColumn = which_column, average = avg_value)
  
  return(result)
}

# Load weather data- 15-min logs
weather_files <- c("data/weather_data/hourly/lo19_field_sensor.csv", 
                   "data/weather_data/hourly/lo20_field_sensor.csv")
fawn_15 <- load_weather_data(weather_files)

# Calculate hourly summary
fawn <- calculate_hourly_summary(fawn_15)

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

calculate_hours <- function(column_name, windo, disease_scoring_dates, data = fawn) {
  results <- list()
  j <- 0
  for (k in 1:nrow(windo)) {
    store <- rep(NA, length(disease_scoring_dates))
    for (dd in 1:length(disease_scoring_dates)) {
      x <- as.numeric(windo[k, 'start_dba'])
      y <- as.numeric(windo[k, 'end_dba'])
      start_d <- as.Date(disease_scoring_dates[dd], format = "%Y-%m-%d") - x
      end_d <- as.Date(disease_scoring_dates[dd], format = "%Y-%m-%d") - y
      store[dd] <- list(summaryOfDaily(start_d, end_d, 0, 24, column_name, data = data))
    }
    results[[as.character(j)]] <- store
    j <- j + 1
  }
  return(results)
}

#specify the conditions to calculate the count of the conditions
results_list <- calculate_hours(column_name = "SoilT5_C", 
                                windo = windo, 
                                disease_scoring_dates= disease_scoring_dates)


# Store the results_list in a data.frame for convenience

# Initialize an empty list to store the tally values from each list in results
tally_list <- list()

# Extract the tally values from each list in results and store them in tally_list
for (i in 1:length(results_list)) {
  tally_list[[i]] <- sapply(results_list[[i]], function(x) x[, "average"])
}

# Convert tally_list to a data.frame; use proper naming based on conditions
results_df <- as.data.frame(tally_list)
colnames(results_df) <- paste0("avgST_W", c(28, 28, 21, 21, 14, 14, 7, 7), "_", start_dba[1:8], "_", end_dba[1:8])
print(results_df)

# Write the results to a file
#write.csv(results_df, file = "store_predictors/LiveOak/lo_avgST_allWin.csv", row.names = FALSE)


