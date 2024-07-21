# Install and load necessary packages
if (!require(fastICA)) install.packages("fastICA")
if (!require(igraph)) install.packages("igraph")
if (!require(gtools)) install.packages("gtools")
if (!require(lmtest)) install.packages("lmtest")

library(fastICA)
library(igraph)
library(gtools)
library(lmtest)

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
B <- diag(nrow(W_normalized)) - W_normalized

# Step 5: Find the permutation matrix P that makes B as close as possible to strictly lower triangular
lower_tri_cost <- function(B) {
  sum(B[upper.tri(B)]^2)
}

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

# Step 8: Implement the Wald test
wald_test_results <- matrix(NA, nrow = nrow(B_pruned), ncol = ncol(B_pruned))
rownames(wald_test_results) <- colnames(data)
colnames(wald_test_results) <- colnames(data)

for (i in 1:nrow(B_pruned)) {
  for (j in 1:ncol(B_pruned)) {
    if (B_pruned[i, j] != 0) {
      model <- lm(data_centered[, i] ~ data_centered[, j])
      wald_test <- waldtest(model, . ~ . - data_centered[, j])
      p_value <- wald_test$'Pr(>F)'[2]
      wald_test_results[i, j] <- p_value
    }
  }
}

cat("Wald test p-values:\n")
print(wald_test_results)

# Print significant causal relationships based on the Wald test
significance_threshold <- 0.05
significant_edges <- which(wald_test_results < significance_threshold, arr.ind = TRUE)

cat("Significant causal relationships (p < 0.05):\n")
for (edge in 1:nrow(significant_edges)) {
  cat(colnames(data)[significant_edges[edge, 1]], "->", colnames(data)[significant_edges[edge, 2]], "\n")
}

# Step 9: Plot the DAG using igraph with significant edges highlighted
colnames(adjacency_matrix) <- colnames(data)
rownames(adjacency_matrix) <- colnames(data)
graph <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed", diag = FALSE)

# Edge colors: Red for significant, Black for non-significant
edge_colors <- rep("black", ecount(graph))
for (i in 1:length(edge_colors)) {
  from <- ends(graph, i)[1]
  to <- ends(graph, i)[2]
  if (wald_test_results[which(rownames(wald_test_results) == from), which(colnames(wald_test_results) == to)] < significance_threshold) {
    edge_colors[i] <- "red"
  }
}

# Open a new graphics device to avoid invalid graphics state
if (dev.cur() > 1) dev.off()
dev.new()
plot(graph, vertex.label = colnames(data), edge.color = edge_colors, main = "DAG from LiNGAM with Significant Edges Highlighted")

# Check if the graph is a DAG
is_dag <- function(adjacency_matrix) {
  graph <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed", diag = FALSE)
  return(is.dag(graph))
}

cat("Is the graph a DAG? ", is_dag(adjacency_matrix), "\n")
