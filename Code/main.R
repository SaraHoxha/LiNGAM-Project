# Install and load necessary packages
library(igraph)
source("utils.R")

# Load the dataset
data <- read_dataframe("../Dataset/seattle-weather-Normalized.csv")

result <- lingam_algorithm(data)


print("B matrix:")
print(result$B)
print("Causal Order")
print(result$causal_order)
print("Adjacency matrix:")
adjacency_matrix <- result$adjacency_matrix
print(result$adjacency_matrix)

colnames(adjacency_matrix) <- colnames(data)
rownames(adjacency_matrix) <- colnames(data)
graph <- graph.adjacency(adjacency_matrix, mode = "directed")
plot(graph, 
     vertex.label = colnames(data),  # Use column names as vertex labels
     main = "DAG from LiNGAM",
     vertex.size = 30,               # Set the size of vertices (nodes)
     vertex.color = "lightblue",     # Set the color of vertices (nodes)
     vertex.frame.color = "black",   # Set the color of the border around vertices
     vertex.label.color = "black",   # Set the color of vertex labels
     vertex.label.cex = 0.8          # Set the size of vertex labels
)

is_dag <- function(adjacency_matrix) {
  graph <- graph.adjacency(adjacency_matrix, mode = "directed")
  return(is.dag(graph))
}

print(is_dag(adjacency_matrix))

