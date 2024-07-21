# Install and load necessary packages
library(igraph)
source("utils.R")

set.seed(123)
# Load the dataset
happiness_data <- read_dataframe("C:/Users/milit/OneDrive/Desktop/stats_project/Statistics-for-data-science/Dataset/generated_data.csv")

happiness_result <- lingam_algorithm(happiness_data)

#BEFORE PRUNING
print("B matrix :")
print(happiness_result$B)
print("Causal Order")
print(happiness_result$causal_order)
print("Adjacency matrix :")
adjacency_matrix <- happiness_result$adjacency_matrix
print(happiness_result$adjacency_matrix)
plot_causality_graph(happiness_result$adjacency_matrix,colnames(happiness_data), happiness_result$causal_order, "Happiness")
print(is_dag(adjacency_matrix))
print("Wald test result")
print(happiness_result$wald_test_results)

is_dag <- function(adjacency_matrix) {
  graph <- graph.adjacency(adjacency_matrix, mode = "directed")
  return(is.dag(graph))
}


