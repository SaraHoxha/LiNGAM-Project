#IMPORT LIBRARIES AND FILES
library("pcalg")
library("here")
source("algorithmImplementation/util.R")

#LOAD DATASET & CONVERT TO MATRIX
relative_path <- here("statsDf.csv")
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


#PLOT CAUSALITY GRAPH
make_dot(res$Bpruned, colnames(data))
