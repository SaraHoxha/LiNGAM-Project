# Combine into a data frame
library("pcalg")
library("igraph")
library("here")

source("utils.R")

data <- read.csv("~/Desktop/Master/UniPi/SFDS/Statistics/Dataset/TEH_World_Happiness_2019_Imputed_Normalized.csv", header = TRUE)
data <- as.matrix(data)

# Apply LINGAM
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
write.csv(causal_order, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_Causal_Order.csv", row.names = FALSE)
write.csv(adj_matrix, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_Adj_Matrix.csv", row.names = FALSE)
write.csv(Bpruned, "~/Desktop/Master/UniPi/SFDS/Statistics/Results/World_Happiness_Estimated_B_matrix.csv", row.names = FALSE)


# Use make_dot function to plot the graph
plot_causality_graph(adj_matrix, node_names = colnames(data))

# Use getAndPrintEdges function to list and print edges
edges <- print_edges(lingam_result, data)