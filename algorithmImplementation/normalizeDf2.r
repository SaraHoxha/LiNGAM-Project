# Load necessary library
library(dplyr)

# Read the CSV file
weather_data <- read.csv("src/seattle-weather.csv")

# Remove the "date" column
weather_data <- select(weather_data, -date)

# Get unique values in the "weather" column
unique_weather <- unique(weather_data$weather)

# Create a mapping for the weather conditions to numeric values
weather_mapping <- setNames(seq_along(unique_weather), unique_weather)

# Apply the mapping to the weather column
weather_data$weather <- as.numeric(factor(weather_data$weather, levels = unique_weather, labels = seq_along(unique_weather)))

# Select numeric columns for scaling, excluding the 'weather' column
numeric_cols <- select(weather_data, -weather)

# Apply scaling to the numeric columns
scaled_numeric_cols <- as.data.frame(scale(numeric_cols))

# Combine the scaled numeric columns with the weather column
weather_data_scaled <- cbind(scaled_numeric_cols, weather = weather_data$weather)

# Save the new data frame to a CSV file
write.csv(weather_data_scaled, "src/seattle-weather-Normalized.csv", row.names = FALSE)
