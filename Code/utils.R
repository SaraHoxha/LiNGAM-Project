plot_causality_graph <- function(adjacency_matrix, node_names = NULL) {
  # Convert adjacency matrix to graph object with directed edges
  graph <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed", weighted = TRUE)
  
  # Set vertex names if provided
  if (!is.null(node_names)) {
    V(graph)$name <- node_names
  }
  
  # Set edge colors based on weight (for visualization purposes)
  E(graph)$color <- ifelse(E(graph)$weight < 0, "red", "black")
  
  # Plot the graph
  plot(graph,
       layout = layout_nicely,
       vertex.label.color = "black",
       vertex.size = 30,
       edge.arrow.size = 0.5,
       edge.curved = 0.2,
       main = "Causal Graph")
}

print_edges <- function(res, data) {
  # Initialize an empty list to store edges
  edges <- list()
  
  # Iterate over the Bpruned matrix to find non-zero entries
  for (i in 1:nrow(res$Bpruned)) {
    for (j in 1:ncol(res$Bpruned)) {
      if (res$Bpruned[i, j] != 0) {
        # If the coefficient is non-zero, add the edge to the list
        edges[[length(edges) + 1]] <- list(from = colnames(data)[j], to = colnames(data)[i], value = res$Bpruned[i, j])
      }
    }
  }
  
  # Print the edges
  for (edge in edges) {
    cat("Edge from", edge$from, "to", edge$to, "with value:", edge$value, "\n")
  }
  
  # Print the total number of edges
  cat("Number of edges:", length(edges), "\n")
  
  # Return the list of edges
  return(edges)
}