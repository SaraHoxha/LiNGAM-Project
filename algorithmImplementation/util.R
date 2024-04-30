#UTILITY FILE
library(igraph)

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
