
rm( list = ls())
# load libraries
preq = c( "ggpmisc", "gridExtra", "ggfortify", "scales", "jsonlite", "lubridate",
          "readxl", "readr", "stats", "haven", "Matrix", "foreign", "zoo", 
          "prophet", "corrr", "broom", "usethis", "rvest", "forecast", 
          "seasonal", "purrr", "data.table", "fable", "fabletools", "ggplot2", 
          "feasts", "tsibble", "tsibbledata", "tidyverse"
)
#for (y in preq) install.packages(y, dep = TRUE)
sapply(preq, library, character.only=T)

folder <- "C:/Users/musta/Documents/MyFitbitData/MM/Sleep/"
folder

files <- list.files(folder, full.names = TRUE); files
#select all files that start with "sleep" and end with ".json"
sleep_files <- files[grep("^sleep.*\\.json$", basename(files), ignore.case = TRUE)]

week_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
month_order <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", 
                 "December")

# Function to process a single JSON file
process_json_file <- function(file_path) {
  # Read JSON file
  json_data <- jsonlite::fromJSON(file_path, flatten = TRUE)
  
  # Extract the filename without extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Convert to tibble and add filename as a column
  df <- as_tibble(json_data) %>%
    mutate(source_file = file_name)
  
  # Unnest the data if it contains nested lists or arrays
 # df_unnested <- df %>%
  #  unnest(cols = where(is.list), names_sep = "_", keep_empty = TRUE)
  
  return(df)
}

# Process all sleep files
sleep_list <- list()
for (i in 1:length(sleep_files)) {
  print(sleep_files[i])
  cat(readLines(sleep_files[i], n = 5))
  sleep_list[[i]] <- process_json_file(sleep_files[i])
}

# Combine all data frames into a single tibble
sleep_data <- bind_rows(sleep_list)

# Assuming sleep_data is your dataframe
sleep_daily <- sleep_data %>%
  mutate(date = as.Date(dateOfSleep)) %>%    # Ensure dateOfSleep is in Date format
  select(-logId, -startTime, -endTime, -dateOfSleep, -source_file
         ) %>%  # Remove unnecessary columns
  group_by(date) %>%
  summarise(across(where(is.numeric), list(
    sum = ~sum(.x, na.rm = TRUE)
  ))) %>%
  ungroup() %>%
  mutate(week_day = weekdays(date),
         week_day = factor(week_day, levels = week_order),
         month = lubridate::month(date, label = TRUE, abbr = FALSE),
         month = factor(month, levels = month_order),
         sleep_hours = minutesAsleep_sum / 60,
         median_sleep = median(sleep_hours, na.rm = TRUE)
         ) %>%    #calculate weekday
  select(date, week_day, month, sleep_hours, everything())   #make date the first column

View(sleep_daily)


# Create the histogram and add the median line
ggplot(sleep_daily, aes(x = sleep_hours)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_vline(aes(xintercept = median_sleep), color = "red", linetype = "dashed", size = 1) +  # Add median line
  labs(x = "Hours of Sleep", y = "Frequency", title = "Histogram of Daily Sleep Hours with Median Line") +
  #annotate("text", x = median_sleep + 0.5, y = Inf, label = "7h 21min", color = "red", vjust = 2) +  # Add text label
  theme_minimal()


# Assuming sleep_duration is already one of the numeric variables summarized
ggplot(sleep_daily, aes(x = week_day, y = levels.summary.rem.minutes_sum)) +
  geom_jitter(width = 0.2, height = 0) +  # Adding jitter for better visibility of points
  labs(x = "Day of the Week", y = "Total Sleep Duration", 
       title = "Daily Sleep Duration by Week Day") +
  theme_minimal()

ggplot(sleep_daily, aes(x = date, y = minutesAsleep_sum)) +
  geom_point() +  # Adding jitter for better visibility of points
  #add a fitted line
  geom_smooth(method = "lm",
              #make the fitted line more curved
              formula = y ~ poly(x, 2)
              ) +
  labs(x = "Day of the Week", y = "Total Sleep Duration", 
       title = "Daily Sleep Duration by Week Day") +
  theme_minimal()


# View the first few rows and structure of the summary
print(head(sleep_daily))
print(str(sleep_daily))
