# Load  libraries
library(ggplot2)
library(reshape2)
library(gplots)
library(GGally)
library(dplyr)

# Read the dataset
df <- read.csv("Dataset/TEH_World_Happiness_2019_Imputed.csv")

# Check for missing values in each column
missing_values <- colSums(is.na(df))
print(missing_values[missing_values > 0])

# Drop unnecessary columns
df <- df[, !names(df) %in% c('Country', 'Year', 'Happiness.rank', 'Corruption', 'Generosity')] 

# Drop rows with missing values
df_cleaned <- na.omit(df)

# Plot histogram of each vairable to check visually distribution
for (col in names(df_cleaned)) {
  print(
    ggplot(df_cleaned, aes_string(x = col)) + 
      geom_histogram(binwidth = 0.5, fill = 'lightblue', color = 'black') + 
      labs(title = paste('Histogram of', col), x = col, y = 'Frequency') +
      theme_minimal()
  )
}

# Check Non-Gaussian with Shapiro-Wilk test for each variable
shapiro_test <- sapply(df_cleaned, function(x) shapiro.test(x)$p.value)
shapiro_test_result <- data.frame(
  Variable = names(df_cleaned),
  Shapiro_Test_p_value = shapiro_test,
  Normality = ifelse(shapiro_test < 0.05, "Not Normal", "Normal")
)
print(shapiro_test_result)

# Scatter plots for all variables
scatter_plot <- ggpairs(df_cleaned,  
                        lower = list(continuous = wrap("points", color = "black")),
                        diag = list(continuous = wrap("densityDiag", color = "lightblue")),
                        upper = list(continuous = wrap("cor", color = "black")))

# Save the plot to a file
ggsave("Figures/Happiness_Scatter_Plot.png", scatter_plot, width = 12, height = 10, units = "in")

# Normalize the dataframe
df_normalized <- as.data.frame(scale(df_cleaned))

# Save the normalized dataset to a CSV file
write.csv(df_normalized, "Dataset/TEH_World_Happiness_2019_Imputed_Normalized.csv", row.names = FALSE)

# Compute the correlation matrix
correlation_matrix <- cor(df_normalized, method = "pearson")

# Plot and save the heatmap
output_path <- "../Figures/Happiness_Correlation_Heatmap.png"
png(output_path, width = 800, height = 800)

# Plot the correlation matrix as a heatmap based on Pearson Coefficient
heatmap.2(correlation_matrix,
          Rowv = FALSE, Colv = FALSE,
          col = colorRampPalette(c("navy", "white", "firebrick3"))(100),
          trace = "none", 
          cellnote = round(correlation_matrix, 2),
          notecol = "black",  
          margins = c(10, 15),
          main = "Happiness Pearson Correlation Heatmap",
          symm = TRUE,
          density.info = "none",  
          key = TRUE,  
          keysize = 1.5,
          key.title = "Correlation",
          key.xlab = "Pearson Coefficient")

dev.off()
