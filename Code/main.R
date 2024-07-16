# Install and load necessary packages
library(igraph)
source("utils.R")

set.seed(123)

# Load the dataset
happiness_data <- read_dataframe("../Dataset/TEH_World_Happiness_2019_Imputed_Normalized.csv")

happiness_result <- lingam_algorithm(happiness_data)

#BEFORE PRUNING
print("B matrix BEFORE:")
print(happiness_result$B)
print("Causal Order")
print(happiness_result$causal_order)
print("Adjacency matrix BEFORE:")
adjacency_matrix <- happiness_result$adjacency_matrix
print(happiness_result$adjacency_matrix)
#plot_causality_graph(happiness_result$adjacency_matrix,colnames(happiness_data), "Happiness BEFORE PRUNING")
#print(is_dag(adjacency_matrix))

is_dag <- function(adjacency_matrix) {
  graph <- graph.adjacency(adjacency_matrix, mode = "directed")
  return(is.dag(graph))
}

#AFTER PRUNING
pruning_result = wald_test(as.matrix(happiness_data),happiness_result$W_prime)

print("B matrix AFTER:")
print(happiness_result$B_pruned)

print("Adjacency matrix AFTER:")
pruning_result$adjacency_matrix_pruned

plot_causality_graph(pruning_result$adjacency_matrix_pruned,colnames(happiness_data), "Happiness AFTER PRUNING")
print(is_dag(pruning_result$adjacency_matrix_pruned))


