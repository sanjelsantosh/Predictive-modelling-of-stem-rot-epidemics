# This code calculates number of hours of provided specific conditions (Tmin, Tmax and RH)
# Windows of length 28, 21, 14 and 7 are created with variations of start and end dates
# Start and end dates of windows correspond to the  disease scoring dates 

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

# Function to calculate number of hours of specific conditions
# Input parameters: start_date, end_date, start_time, end_time
# RH: Relative humidity thereshold, only hours with RH>threshold will be considered
# Use Soil temperature vaiable name = SoilT5_C

numberOfHours <- function(start_date, end_date, start_time, end_time, RH, avgTempLow=NULL, avgTempHigh = NULL){
  fawnSubset <- fawn %>%
    subset(days > start_date & days <= end_date & RH_15cm > RH)
  
  
  if(start_time > 24 || start_time < 0 || end_time > 24 || end_time < 0){
    stop('Please choose time that is from 0-24')
  }
  
  if(start_time > end_time){
    fawnTime1 <- subset(fawnSubset, hour(fawnSubset$hours) >= start_time)
    fawnTime2 <- subset(fawnSubset, hour(fawnSubset$hours) <= end_time)
    fawnSubset <- rbind(fawnTime1, fawnTime2)
  } else {
    fawnSubset <- subset(fawnSubset, hour(fawnSubset$hours) >= start_time & hour(fawnSubset$hours) <= end_time)
  }
  
  if(is.null(avgTempLow)==TRUE){
    timePeriod <- paste(start_date,' to ', end_date)
    tally <- dim(fawnSubset)[1]
    answer <- cbind(timePeriod, tally)
  } else {
    fawnSubset <- fawnSubset %>%
      subset(SoilT5_C >= avgTempLow & SoilT5_C <= avgTempHigh)
    
    timePeriod <- paste(start_date,' to ', end_date)
    tally <- dim(fawnSubset)[1]
    answer <- cbind(timePeriod, tally)
  }
  
  return(answer)
}  

# Load weather data- 15-min logs
weather_files <- c("data/weather_data/lo19_field_sensor.csv", 
                   "data/weather_data/lo20_field_sensor.csv")
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

# Function to iterate the previously defined 'numberOfHours' function with the given conditions
# counts are based on the specified windows corresponding to the disease scoring dates

calculate_hours <- function(start_time, endtime, RH, avgTempLow, avgTempHigh, windo, disease_scoring_dates) {
  results <- list()
  j <- 0
  for (k in 1:nrow(windo)) {
    store <- rep(NA, length(disease_scoring_dates))
    for (dd in 1:length(disease_scoring_dates)) {
      x <- as.numeric(windo[k, 'start_dba'])
      y <- as.numeric(windo[k, 'end_dba'])
      start_d <- as.Date(disease_scoring_dates[dd], format = "%Y-%m-%d") - x
      end_d <- as.Date(disease_scoring_dates[dd], format = "%Y-%m-%d") - y
      store[dd] <- list(numberOfHours(start_d, end_d, start_time, endtime, RH, avgTempLow, avgTempHigh))
    }
    results[[as.character(j)]] <- store
    j <- j + 1
  }
  return(results)
}

#specify the conditions to calculate the count of the conditions
results_list <- calculate_hours(start_time =0, 
                                endtime = 24, 
                                RH = 90, 
                                avgTempLow = 25, 
                                avgTempHigh = 35, 
                                windo = windo, 
                                disease_scoring_dates= disease_scoring_dates)

#-------
# Store the values in a data.frame for convenience

# Initialize an empty list to store the tally values from each list in results
tally_list <- list()

# Extract the tally values from each list in results and store them in tally_list
for (i in 1:length(results_list)) {
  tally_list[[i]] <- sapply(results_list[[i]], function(x) x[, "tally"])
}

# Convert tally_list to a data.frame; use proper naming based on conditions
results_df <- as.data.frame(tally_list)
colnames(results_df) <- paste0("nHrRHa90T25-35_W", c(28, 28, 21, 21, 14, 14, 7, 7), "_", start_dba[1:8], "_", end_dba[1:8])

print(results_df)

# Write the results to a file
#write.csv(results_df, file = "store_weather_var/liveOak/lo_RHa90T25-35_allWin.csv", row.names = FALSE)



#-----------------------------------------------------------------------------------------
# Function to extract the individual list of values in each window; for review purpose only
extract_df_from_results <- function(results_list, list_number) {
  # Extracting the specified list from results
  list_data <- results_list[[list_number]]
  
  # Creating an empty data frame
  df <- data.frame(timePeriod = character(), tally = character(), window = character(), stringsAsFactors = FALSE)
  
  # Looping through each element of the specified list and appending to the data frame
  for (i in 1:length(list_data)) {
    
    # Extracting timePeriod and tally from the matrix
    timePeriod <- list_data[[i]][1, "timePeriod"]
    tally <- list_data[[i]][1, "tally"]

    # Extracting start and end dates
    dates <- unlist(strsplit(timePeriod, " to "))
    start_date <- as.Date(dates[1])
    end_date <- as.Date(dates[2])

    # Calculating the difference in days
    window <- as.numeric(end_date - start_date)

    # Appending to the data frame
    df <- rbind(df, data.frame(window, timePeriod, tally, stringsAsFactors = FALSE))

  }

  # Resetting row names
  rownames(df) <- NULL

  return(df)
}

# Example usage: extract data frame from list 1
extract_df_from_results(results_list, 1)


##---------------------------------
