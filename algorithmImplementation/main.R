
#IMPORT LIBRARIES AND FILES
library("pcalg")
library("here")
source("algorithmImplementation/util.R")

#LOAD DATASET & CONVERT TO MATRIX
relative_path <- here("geneNoStringColsNormalized.csv")
data <- read.csv(relative_path, header = TRUE)
matrix_data <- as.matrix(data)

# RUN LINGAM ALGORITHM
#lingam() returns:
# -Bpruned a p × p matrix B of linear coefficients, where Bi,j corresponds to a directed
# -edge from j to i.
# -stde a vector of length p with the standard deviations of the estimated residuals
# -ci a vector of length p with the intercepts of each equation
res <- lingam(matrix_data, verbose = FALSE)


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

cat (length(edges))

#PLOT CAUSALITY GRAPH
make_dot(res$Bpruned, colnames(data))


# Function to perform topological sorting
topological_sort <- function(graph) {
  indegree <- numeric(length = length(graph))
  for (i in seq_along(graph)) {
    indegree[i] <- sum(graph[[i]] == TRUE)
  }
  
  queue <- which(indegree == 0)
  sorted <- vector("character", length = length(graph))
  index <- 1
  
  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    sorted[index] <- node
    index <- index + 1
    
    neighbors <- which(graph[[node]] == TRUE)
    for (neighbor in neighbors) {
      indegree[neighbor] <- indegree[neighbor] - 1
      if (indegree[neighbor] == 0) {
        queue <- c(queue, neighbor)
      }
    }
  }
  
  if (sum(indegree > 0) > 0) {
    return(NULL) # Not a DAG
  } else {
    return(sorted)
  }
}

# Construct adjacency matrix
num_nodes <- ncol(matrix_data)
adj_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)
for (edge in edges) {
  from_index <- which(colnames(data) == edge$from)
  to_index <- which(colnames(data) == edge$to)
  adj_matrix[to_index, from_index] <- 1
}

# Perform topological sorting
sorted_nodes <- topological_sort(graph = adj_matrix)

# Check if the graph is a DAG
if (is.null(sorted_nodes)) {
  cat("The graph is not a Directed Acyclic Graph (DAG).\n")
} else {
  cat("The graph is a Directed Acyclic Graph (DAG).\n")
}


