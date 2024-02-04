# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(geosphere)

# Specify the directory where the zip files are located
zip_dir <- paste0(getwd(), "/data")

# Get the list of zip files
zip_files <- list.files(path = zip_dir, pattern = "*.zip")

# Initialize an empty list to store data frames
data_list <- list()

# Loop over the zip files
for (zip_file in zip_files) {

  # Unzip the file
  unzip(file.path(zip_dir, zip_file), exdir = zip_dir)

  # Get the name of the csv file (assuming it's the same as the zip without the .zip)
  csv_file <- sub(".zip", ".csv", zip_file)

  # Read the csv file and store the data frame in the list
  data_list[[csv_file]] <- read_delim(file.path(zip_dir, csv_file))

  # Remove the csv file if you no longer need it
  file.remove(file.path(zip_dir, csv_file))
}

# Combine all data frames into a single one
combined_data <- bind_rows(data_list)

# Add new variables
dataset <- combined_data |>
  mutate(ride_time = as.numeric(difftime(ended_at, started_at, units = "mins")),
         year_started = year(started_at),
         year_month_started = as.Date(as_datetime(
           paste(year_started, month(started_at, label = TRUE), "01",
                 sep = "-"))),
         month_started = month(started_at, label = TRUE),
         wday_started = wday(started_at, label = TRUE),
         day_started = day(started_at),
         hour_started = hour(started_at),
         distance_km = distHaversine(cbind(start_lng, start_lat),
                                     cbind(end_lng, end_lat)) / 1000)

glimpse(dataset)
