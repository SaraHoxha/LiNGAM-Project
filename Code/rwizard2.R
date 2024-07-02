library(fastICA)
library(igraph)

# Load the dataset
data <- read.csv("~/Desktop/Master/UniPi/SFDS/Statistics/Dataset/TEH_World_Happiness_2019_Imputed_Normalized.csv", header = TRUE)
data <- as.matrix(data)

# Step 1: Subtract the mean from each column of the data
data_centered <- scale(data, center = TRUE, scale = FALSE)

# Step 2: Apply ICA
ica_result <- fastICA(data_centered, n.comp = ncol(data_centered))
A <- ica_result$A
S <- ica_result$S

# Compute W as the inverse of A
W <- solve(A)

# Normalize rows to have ones on the diagonal
W_normalized <- W
for (i in 1:nrow(W_normalized)) {
  if (W_normalized[i, i] != 0) {
    W_normalized[i, ] <- W_normalized[i, ] / W_normalized[i, i]
  } else {
    stop(paste("Diagonal element at row", i, "is zero. Cannot normalize."))
  }
}

# Compute B as I - W_normalized
B <- diag(nrow(W_normalized)) - W_normalized

# Ensure B is strictly lower triangular
B[upper.tri(B)] <- 0

# Step 5: Prune B to obtain B_pruned
threshold <- 0.35  # Set a threshold value for pruning
B_pruned <- B
B_pruned[abs(B) < threshold] <- 0

# Generate the Adjacency Matrix for B_pruned
adjacency_matrix <- ifelse(B != 0, 1, 0)
adjacency_matrix_pruned <- ifelse(B_pruned != 0, 1, 0)

# Function to plot a DAG
plot_dag <- function(adj_matrix, main_title) {
  colnames(adj_matrix) <- colnames(data)
  rownames(adj_matrix) <- colnames(data)
  
  graph <- graph.adjacency(adj_matrix, mode = "directed")
  plot(graph, vertex.label = colnames(data), main = main_title)
}

# Set up the layout for the plots
par(mfrow = c(1, 2))  # Arrange plots in 1 row, 2 columns

# Plot DAG from B
plot_dag(adjacency_matrix, "DAG from B")

# Plot DAG from B_pruned
plot_dag(adjacency_matrix_pruned, "DAG from B_pruned")

# Save the matrices to CSV files
write.csv(B, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_Estimated_B_matrix.csv", row.names = FALSE)
write.csv(B_pruned, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_B_pruned_matrix.csv", row.names = FALSE)
write.csv(adjacency_matrix_pruned, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_Adjacency_matrix_pruned.csv", row.names = FALSE)


# Function to check if a graph is a DAG
is_dag <- function(adjacency_matrix) {
  graph <- graph.adjacency(adjacency_matrix, mode = "directed")
  return(is.dag(graph))
}

# Check if the graph is a DAG
result <- is_dag(adjacency_matrix)
if (result) {
  print("The graph is a DAG.")
} else {
  print("The graph is not a DAG.")
}

# Print the matrices
print("B matrix:")
print(B)
print("B pruned matrix:")
print(B_pruned)
print("Adjacency matrix:")
print(adjacency_matrix_pruned)

# Save the matrices to CSV files
write.csv(B, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_Estimated_B_matrix.csv", row.names = FALSE)
write.csv(B_pruned, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_B_pruned_matrix.csv", row.names = FALSE)
write.csv(adjacency_matrix, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_Adjacency_matrix.csv", row.names = FALSE)

# Wald Test Function for Pruning Edges
wald_test <- function(B, W, data, significance_level = 0.05) {
  n <- nrow(B)
  p <- ncol(data)
  
  # Calculate the asymptotic variances of the elements of W
  avar <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        avar[i, j] <- sum((W[i, ] * W[j, ])^2) / p
      }
    }
  }
  
  # Calculate the Wald statistics
  wald_stat <- B^2 / avar
  
  # Critical value for the chi-square distribution with 1 degree of freedom
  critical_value <- qchisq(1 - significance_level, df = 1)
  
  # Prune the B matrix based on the Wald test
  B_pruned <- ifelse(wald_stat >= critical_value, B, 0)
  
  return(B_pruned)
}

# Example usage
# Assuming B, W, and data_centered are already defined from the previous steps

# Calculate pruned B matrix using the Wald test
B_pruned_after <- wald_test(B, W, data_centered)

# Print the pruned B matrix
print("B pruned matrix AFTER:")
print(B_pruned)


