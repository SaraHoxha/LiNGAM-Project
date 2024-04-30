# Load the necessary library
library(data.table)

# Read the CSV file into a data frame
data <- fread("statsDf.csv")

# Select numeric columns to normalize
numeric_cols <- sapply(data, is.numeric)

# Normalize the numeric columns
normalized_data <- scale(data[, ..numeric_cols])

# Convert the normalized data back to a data frame
normalized_data <- as.data.frame(normalized_data)

# Replace the original numeric columns with the normalized ones
data[, (names(data)[numeric_cols]) := normalized_data]

# Print the first few rows of the normalized dataset
print(head(data))

fwrite(data, "statsDfNormalized.csv")