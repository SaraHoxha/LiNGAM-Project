# Install and load necessary packages
library(igraph)
source("utils.R")

#Set seed for reproducibiilty
set.seed(123)

# Load the dataset
happiness_data <- read_dataframe("../Dataset/TEH_World_Happiness_2019_Imputed_Normalized.csv")

#Apply LINGAM
happiness_result <- lingam_algorithm(happiness_data)

#Print results
print("B matrix :")
print(happiness_result$B)
print("Causal Order")
print(happiness_result$causal_order)
print("Adjacency matrix :")
print(happiness_result$adjacency_matrix)
print("Wald test result Signficant")
print(happiness_result$wald_test_results)

#Plot causality graph
plot_causality_graph(happiness_result$adjacency_matrix, "../Figures/CasualGraph.png")

#Check if graph is a DAG
print("Is the causal graph a DAG?")
print(is_dag(adjacency_matrix))

