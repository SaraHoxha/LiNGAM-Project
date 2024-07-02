# Load necessary libraries
library(ggplot2)
library(reshape2)
library(gplots)

# Read the dataset
df <- read.csv("~/Desktop/Master/UniPi/SFDS/Statistics/Dataset/TEH_World_Happiness_2019_Imputed.csv")

# Check for missing values in each column
missing_values <- colSums(is.na(df))
print(missing_values[missing_values > 0])

# Drop unnecessary columns
df <- df[, !names(df) %in% c('Country', 'Year', 'Happiness.rank', 'Corruption')] #'Corruption', 'Generosity') add

# Drop rows with missing values
df_cleaned <- na.omit(df)

# Normalize the dataframe
df_normalized <- as.data.frame(scale(df_cleaned))

# Display the normalized dataframe
print("After normalization:")
print(df_normalized)

# Save the normalized dataset to a CSV file
write.csv(df_normalized, "~/Desktop/Master/UniPi/SFDS/Statistics/Dataset/TEH_World_Happiness_2019_Imputed_Normalized_Milica_v2.csv", row.names = FALSE)

# Compute the correlation matrix
correlation_matrix <- cor(df_normalized, method = "pearson")


# Define the path to save the heatmap
output_path <- "~/Desktop/Master/UniPi/SFDS/Statistics/Figures/correlation_heatmap_Milica.png"

# Open a PNG device to save the heatmap
png(output_path, width = 800, height = 800)

# Plot the correlation matrix as a heatmap using heatmap.2 for better customization
heatmap.2(correlation_matrix,
          Rowv = NA, Colv = NA,
          col = colorRampPalette(c("navy", "white", "firebrick3"))(100),
          trace = "none", 
          cellnote = round(correlation_matrix, 2),  # Add correlation values
          notecol = "black",  # Set the color of the text
          margins = c(10, 15),
          main = "Pearson Correlation Heatmap",
          symm = TRUE,
          density.info = "none",  # Remove density info
          key = TRUE,  # Add a legend
          keysize = 1.5,  # Size of the legend
          key.title = "Correlation",
          key.xlab = "Pearson Coefficient")

dev.off()
