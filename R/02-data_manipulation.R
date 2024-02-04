# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(forcats)

# Define cold and warm months ----
cold_months <- c("2022-11-01", "2022-12-01", "2023-01-01", "2023-02-01")
warm_months <- c("2023-06-01", "2022-07-01", "2022-08-01")

# Monthly and daily ranking ----
# Obs.: dataset is the name of the data frame created in the previous script
month_descriptive <- dataset |>
  filter(rideable_type != "docked_bike") |>
  group_by(year_month_started,
           member_casual, rideable_type,
           day_started) |>
  summarise(total_rides = n(),
            total_ride_time = sum(ride_time, na.rm = TRUE),
            avg_ride_time = mean(ride_time, na.rm = TRUE),
            med_ride_time = median(ride_time, na.rm = TRUE),
            sd_ride_time = sd(ride_time, na.rm = TRUE),
            min_ride_time = min(ride_time, na.rm = TRUE),
            max_ride_time = max(ride_time, na.rm = TRUE),
            total_ride_distance_km = sum(distance_km, na.rm = TRUE),
            avg_distance_km = mean(distance_km, na.rm = TRUE),
            med_distance_km = median(distance_km, na.rm = TRUE),
            sd_distance_km = sd(distance_km, na.rm = TRUE),
            min_distance_km = min(distance_km, na.rm = TRUE),
            max_distance_km = max(distance_km, na.rm = TRUE))

month_line_plot_total <- month_descriptive |>
  group_by(year_month_started, member_casual, rideable_type) |>
  summarise(total_rides = sum(total_rides),
            total_ride_time = sum(total_ride_time),
            total_ride_distance_km = sum(total_ride_distance_km))

# Weekday ranking ----
weekday_descriptive <- dataset |>
  filter(rideable_type != "docked_bike") |>
  group_by(year_month_started,
           member_casual, rideable_type,
           wday_started) |>
  summarise(total_rides = n(),
            total_ride_time = sum(ride_time, na.rm = TRUE),
            avg_ride_time = mean(ride_time, na.rm = TRUE),
            med_ride_time = median(ride_time, na.rm = TRUE),
            sd_ride_time = sd(ride_time, na.rm = TRUE),
            min_ride_time = min(ride_time, na.rm = TRUE),
            max_ride_time = max(ride_time, na.rm = TRUE),
            total_ride_distance_km = sum(distance_km, na.rm = TRUE),
            avg_distance_km = mean(distance_km, na.rm = TRUE),
            med_distance_km = median(distance_km, na.rm = TRUE),
            sd_distance_km = sd(distance_km, na.rm = TRUE),
            min_distance_km = min(distance_km, na.rm = TRUE),
            max_distance_km = max(distance_km, na.rm = TRUE))

# Hourly ranking ----
hour_descriptive <-dataset |>
  filter(rideable_type != "docked_bike") |>
  group_by(wday_started,
           member_casual, rideable_type,
           hour_started) |>
  summarise(total_rides = n(),
            total_ride_time = sum(ride_time, na.rm = TRUE),
            avg_ride_time = mean(ride_time, na.rm = TRUE),
            med_ride_time = median(ride_time, na.rm = TRUE),
            sd_ride_time = sd(ride_time, na.rm = TRUE),
            min_ride_time = min(ride_time, na.rm = TRUE),
            max_ride_time = max(ride_time, na.rm = TRUE),
            total_ride_distance_km = sum(distance_km, na.rm = TRUE),
            avg_distance_km = mean(distance_km, na.rm = TRUE),
            med_distance_km = median(distance_km, na.rm = TRUE),
            sd_distance_km = sd(distance_km, na.rm = TRUE),
            min_distance_km = min(distance_km, na.rm = TRUE),
            max_distance_km = max(distance_km, na.rm = TRUE))

weekday_line_plot_total <- weekday_descriptive |>
  group_by(wday_started, member_casual, rideable_type) |>
  summarise(total_rides = sum(total_rides),
            total_ride_time = sum(total_ride_time),
            total_ride_distance_km = sum(total_ride_distance_km))
