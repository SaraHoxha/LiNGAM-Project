# Install and load necessary packages
if (!require(fastICA)) install.packages("fastICA")
if (!require(igraph)) install.packages("igraph")
if (!require(gtools)) install.packages("gtools")

library(fastICA)
library(igraph)
library(gtools)

# Load the dataset
data <- read.csv("C:/Users/milit/OneDrive/Desktop/stats_project/Statistics-for-data-science/Dataset/TEH_World_Happiness_2019_Imputed_Normalized.csv")

# Step 1: Subtract the mean from each column of the data
data_centered <- scale(data, center = TRUE, scale = FALSE)

# Step 2: Apply ICA
ica_result <- fastICA(data_centered, n.comp = ncol(data_centered))
A <- ica_result$A
S <- ica_result$S

cat("Mixing Matrix (A):\n")
print(A)
cat("Source Matrix (S):\n")
print(S)

# Compute W as the inverse of A
W <- solve(A)

# Step 3: Normalize rows to have ones on the diagonal
W_normalized <- W
for (i in 1:nrow(W_normalized)) {
  W_normalized[i, ] <- W_normalized[i, ] / W_normalized[i, i]
}

cat("Normalized Unmixing Matrix (W):\n")
print(W_normalized)

# Step 4: Compute B as I - W_normalized
#this matrix represents the estimated direct influences between variables.
B <- diag(nrow(W_normalized)) - W_normalized

# Step 5: Find the permutation matrix P that makes B as close as possible to strictly lower triangular
# Define a cost function for measuring how close B is to lower triangular
lower_tri_cost <- function(B) {
  sum(B[upper.tri(B)]^2)
}

# Find the best permutation
perms <- permutations(nrow(B), nrow(B))
min_cost <- Inf
best_perm <- NULL
for (perm in seq_len(nrow(perms))) {
  perm_matrix <- B[perms[perm, ], perms[perm, ]]
  cost <- lower_tri_cost(perm_matrix)
  if (cost < min_cost) {
    min_cost <- cost
    best_perm <- perms[perm, ]
  }
}

P <- diag(nrow(B))[best_perm, ]
B_permuted <- P %*% B %*% t(P)

# It appears that ensuring 
#B is strictly lower triangular after permutation is necessary to maintain the DAG structure. 
B_permuted[upper.tri(B_permuted)] <- 0

# Step 6: Prune B to obtain B_pruned
threshold <- 0.1 # Set a threshold value for pruning
B_pruned <- B_permuted
B_pruned[abs(B_pruned) < threshold] <- 0

# Step 7: Generate the Adjacency Matrix
adjacency_matrix <- ifelse(B_pruned != 0, 1, 0)

# Print the matrices
cat("B matrix:\n")
print(B)
cat("B permuted matrix:\n")
print(B_permuted)
cat("B pruned matrix:\n")
print(B_pruned)
cat("Adjacency matrix:\n")
print(adjacency_matrix)

# Step 8: Plot the DAG using igraph
colnames(adjacency_matrix) <- colnames(data)
rownames(adjacency_matrix) <- colnames(data)
graph <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed", diag = FALSE)

# Open a new graphics device to avoid invalid graphics state
#dev.off()  # Close any open graphics devices
dev.new()
plot(graph, vertex.label = colnames(data), main = "DAG from LiNGAM")

is_dag <- function(adjacency_matrix) {
  graph <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed", diag = FALSE)
  return(is.dag(graph))
}

cat("Is the graph a DAG? ", is_dag(adjacency_matrix), "\n")