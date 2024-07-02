# Install and load necessary packages
library(fastICA)
library(igraph)

# Load the dataset
data <- read.csv("~/Desktop/Master/UniPi/SFDS/Statistics/Dataset/TEH_World_Happiness_2019_Imputed_Normalized.csv")

#Step 1: Subtract the mean from each column of the data
data_centered <- scale(data, center = TRUE, scale = FALSE)

# Step 2: Apply ICA
ica_result <- fastICA(data_centered, n.comp = ncol(data_centered))
A <- ica_result$A
S <- ica_result$S

# Compute W as the inverse of A
W <- solve(A)

# Step 3: Normalize rows to have ones on the diagonal
W_normalized <- W
for (i in 1:nrow(W_normalized)) {
  W_normalized[i, ] <- W_normalized[i, ] / W_normalized[i, i]
}

# Step 4: Compute B as I - W_normalized
B <- diag(nrow(W_normalized)) - W_normalized

# Ensure B is strictly lower triangular
B[upper.tri(B)] <- 0

# Step 5: Prune B to obtain B_pruned
threshold <- 0.1 # Set a threshold value for pruning
B_pruned <- B
B_pruned[abs(B) < threshold] <- 0

# Step 6: Generate the Adjacency Matrix
adjacency_matrix <- ifelse(B_pruned != 0, 1, 0)

# Print the matrices
print("B matrix:")
print(B)
print("B pruned matrix:")
print(B_pruned)
print("Adjacency matrix:")
print(adjacency_matrix)

# Step 7: Plot the DAG using igraph
colnames(adjacency_matrix) <- colnames(data)
rownames(adjacency_matrix) <- colnames(data)
graph <- graph.adjacency(adjacency_matrix, mode = "directed")
plot(graph, vertex.label = colnames(data), main = "DAG from LiNGAM")

is_dag <- function(adjacency_matrix) {
  graph <- graph.adjacency(adjacency_matrix, mode = "directed")
  return(is.dag(graph))
}

print(is_dag(adjacency_matrix))

# Save the matrices to CSV files
write.csv(B, "estimated_B_matrix.csv", row.names = FALSE)
write.csv(B_pruned, "B_pruned_matrix.csv", row.names = FALSE)
write.csv(adjacency_matrix, "adjacency_matrix.csv", row.names = FALSE)