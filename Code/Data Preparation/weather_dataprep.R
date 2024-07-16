# Load necessary libraries
library(ggplot2)
library(reshape2)
library(gplots)
library(GGally)
library(stringr)
library(dplyr)

# Read the dataset
df <- read.csv("../Dataset/Seattle_Weather.csv")

# Check for missing values in each column
missing_values <- colSums(is.na(df))
print(missing_values[missing_values > 0])

# Drop unnecessary columns
df <- df[, !names(df) %in% c('date')] 

# Capitalize names of columns
colnames(df) <- str_to_title(colnames(df))

# Drop rows with missing values
df_cleaned <- na.omit(df)

# Get unique values in the "Weather" column
unique_weather <- unique(df_cleaned$Weather)

# Create a mapping for the weather conditions to numeric values
weather_mapping <- setNames(seq_along(unique_weather), unique_weather)

# Apply the mapping to the weather column
df_cleaned$Weather <- as.numeric(factor(df_cleaned$Weather, levels = unique_weather, labels = seq_along(unique_weather)))

#Obtain only the numeric columns of the dataset
numeric_cols <- select(df_cleaned, -Weather)

# Plot histogram of each vairable to check visually distribution
for (col in names(numeric_cols)) {
  print(
    ggplot(numeric_cols, aes_string(x = col)) + 
      geom_histogram(binwidth = 0.5, fill = 'lightblue', color = 'black') + 
      labs(title = paste('Histogram of', col), x = col, y = 'Frequency') +
      theme_minimal()
  )
}

# Check Non-Gaussian with Shapiro-Wilk test for each variable
shapiro_test <- sapply(numeric_cols, function(x) shapiro.test(x)$p.value)
shapiro_test_result <- data.frame(
  Variable = names(numeric_cols),
  Shapiro_Test_p_value = shapiro_test,
  Normality = ifelse(shapiro_test < 0.05, "Not Normal", "Normal")
)
print(shapiro_test_result)


# Scatter plots for all variables
scatter_plot <- ggpairs(df_cleaned,  
                        lower = list(continuous = wrap("points", color = "black")),
                        diag = list(continuous = wrap("densityDiag", color = "lightblue")),
                        upper = list(continuous = wrap("cor", color = "black")))

# Save the scatter plot
ggsave("../Figures/Weather_Scatter_Plot.png", scatter_plot, width = 12, height = 10, units = "in")

# Normalize the numeric columns
scaled_numeric_cols <- as.data.frame(scale(numeric_cols))

# Combine the scaled numeric columns with the Weather column
df_normalized <- cbind(scaled_numeric_cols, Weather = df_cleaned$Weather)

# Save the normalized dataset to a CSV file
write.csv(df_normalized, "../Dataset/Seattle_Weather_Normalized.csv", row.names = FALSE)

# Compute the correlation matrix
correlation_matrix <- cor(df_normalized, method = "pearson")

# Define the path to save the heatmap
output_path <- "../Figures/Weather_Correlation_Heatmap.png"
png(output_path, width = 800, height = 800)

# Plot the correlation matrix as a heatmap using heatmap.2 for better customization
heatmap.2(correlation_matrix,
          Rowv = FALSE, Colv = FALSE,
          col = colorRampPalette(c("navy", "white", "firebrick3"))(100),
          trace = "none", 
          cellnote = round(correlation_matrix, 2),  
          notecol = "black", 
          margins = c(10, 15),
          main = "Weather Pearson Correlation Heatmap",
          symm = TRUE,
          density.info = "none",
          key = TRUE, 
          keysize = 1.5,  
          key.title = "Correlation",
          key.xlab = "Pearson Coefficient")

dev.off()
