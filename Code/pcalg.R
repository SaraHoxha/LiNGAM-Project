# Install and load necessary packages
library(pcalg)
library(igraph)

# Load utility functions if any
 #source("~/Desktop/Master/UniPi/SFDS/Statistics/Code/utils.R") # Ensure this path is correct

# Load the dataset
data <- read.csv("~/Desktop/Master/UniPi/SFDS/Statistics/Dataset/TEH_World_Happiness_2019_Imputed_Normalized.csv", header = TRUE)
data <- as.matrix(data)

# Apply LiNGAM using the pcalg package
lingam_result <- lingam(data)
print(str(lingam_result))

# Extract Bpruned matrix
Bpruned <- lingam_result$Bpruned

# Determine the causal ordering from Bpruned
causal_order <- order(rowSums(Bpruned != 0))
print("Causal Ordering:")
print(causal_order)

# Construct the adjacency matrix
adj_matrix <- Bpruned != 0
print("Adjacency Matrix:")
print(adj_matrix)

# Print the Estimated B Matrix
print("Estimated B Matrix:")
print(Bpruned)

# Save the results to files
write.csv(causal_order, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_Causal_Order_Milica.csv", row.names = FALSE)
write.csv(adj_matrix, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_Adj_Matrix_Milica.csv", row.names = FALSE)
write.csv(Bpruned, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_Estimated_B_matrix_Milica.csv", row.names = FALSE)

# Function to plot the causality graph
plot_causality_graph <- function(adjacency_matrix, node_names = NULL) {
  graph <- graph.adjacency(adjacency_matrix, mode = "directed")
  if (!is.null(node_names)) {
    V(graph)$name <- node_names
  }
  plot(graph, vertex.label = node_names, main = "DAG from LiNGAM")
}

# Plot the DAG
plot_causality_graph(adj_matrix, node_names = colnames(data))

# Function to list and print edges
print_edges <- function(lingam_result, data) {
  Bpruned <- lingam_result$Bpruned
  edges <- which(Bpruned != 0, arr.ind = TRUE)
  edge_list <- data.frame(from = colnames(data)[edges[, 2]], to = colnames(data)[edges[, 1]])
  print(edge_list)
  return(edge_list)
}

# Print the edges
edges <- print_edges(lingam_result, data)

# Function to check if a graph is a DAG
is_dag <- function(adjacency_matrix) {
  graph <- graph.adjacency(adjacency_matrix, mode = "directed")
  return(is.dag(graph))
}

print(is_dag(adj_matrix))