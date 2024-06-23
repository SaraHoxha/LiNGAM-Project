#IMPORT LIBRARIES AND FILES
library("pcalg")
library("here")
library("igraph")

#FUNCTION TO PLOT THE ADJACENCY MATRIX
make_dot <- function(adjacency_matrix, node_names = NULL) {
  graph <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed", weighted = TRUE)
  
  if (!is.null(node_names)) {
    V(graph)$name <- node_names
  }
  
  E(graph)$color <- ifelse(E(graph)$weight < 0, "red", "black")
  
  plot(graph,
       layout = layout_nicely,
       vertex.label.color = "black",
       vertex.size = 30,
       edge.arrow.size = 0.5,
       edge.curved = 0.2,
       main = "Causal Graph")
}

getAndPrintEdges <- function (res, data){
  # LIST EDGES
  edges <- list()
  for (i in 1:nrow(res$Bpruned)) {
    for (j in 1:ncol(res$Bpruned)) {
      if (res$Bpruned[i, j] != 0) {  # If the coefficient is non-zero, it indicates an edge
        edges[[length(edges) + 1]] <- list(from = colnames(data)[j], to = colnames(data)[i], value = res$Bpruned[i, j])
      }
    }
  }

  for (edge in edges) {
    cat("Edge from", edge$from, "to", edge$to, "with value:", edge$value, "\n")
  }

  cat ("number of edges", length(edges), "\n")

  return (edges)
}

# Function to perform topological sorting
topological_sort <- function(graph) {
  indegree <- colSums(graph)
  queue <- which(indegree == 0)
  sorted <- character()
  
  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    sorted <- c(sorted, node)
    
    neighbors <- which(graph[node, ] == 1)
    for (neighbor in neighbors) {
      indegree[neighbor] <- indegree[neighbor] - 1
      if (indegree[neighbor] == 0) {
        queue <- c(queue, neighbor)
      }
    }
  }
  
  if (length(sorted) == ncol(graph)) {
    return(sorted)
  } else {
    return(NULL) # Not a DAG
  }
}

#LOAD DATASET & CONVERT TO MATRIX
data <- read.csv("algorithmImplementation/seattle-weather-Normalized No weather column.csv", header = TRUE)
matrix_data <- as.matrix(data)

# RUN LINGAM ALGORITHM
#lingam() returns:
# -Bpruned a p × p matrix B of linear coefficients, where Bi,j corresponds to a directed
# -edge from j to i.
# -stde a vector of length p with the standard deviations of the estimated residuals
# -ci a vector of length p with the intercepts of each equation
res <- lingam(matrix_data, verbose = FALSE)

edges <- getAndPrintEdges(res, data)

#PLOT CAUSALITY GRAPH
make_dot(res$Bpruned, colnames(data))

# Perform topological sorting
sorted_nodes <- topological_sort(graph = adj_matrix)

# Check if the graph is a DAG
if (is.null(sorted_nodes)) {
  cat("The graph is not a Directed Acyclic Graph (DAG).\n")
} else {
  cat("The graph is a Directed Acyclic Graph (DAG).\n")
}