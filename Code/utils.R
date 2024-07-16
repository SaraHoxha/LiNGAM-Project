library(fastICA)
library(combinat)
library(gtools)

# Function to read the CSV file into a data frame
read_dataframe <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File does not exist.")
  }
  df <- read.csv(file_path)
  return(df)
}

# Function to calculate Adjacency Matrix from matrix B
calculate_adjacency_matrix(B) {
  adjacency_matrix <- ifelse(B != 0, 1, 0)
  return(adjacency_matrix)
}

# Function implementing LINGAM based on algorithm steps of study paper
lingam_algorithm <- function(data) {
  # Step 1: Subtract the mean from each column of the data
  data_centered <- scale(data, center = TRUE, scale = FALSE)
  
  # Step 1: Apply ICA
  ica_result <- fastICA(data_centered, n.comp = ncol(data_centered))
  A <- ica_result$A
  S <- ica_result$S
  
  cat("Mixing Matrix (A):\n")
  print(A)
  cat("Source Matrix (S):\n")
  print(S)
  
  # Step 2: Compute W as the inverse of A
  W <- solve(A)
  
  # Step 2: Permute rows of W to minimize sum(1/|Wii|)
  n <- ncol(W)
  perms <- permn(1:n)
  min_sum <- Inf
  best_perm <- NULL
  
  for (perm in perms) {
    W_perm <- W[perm, ]
    diag_elements <- diag(W_perm)
    current_sum <- sum(1 / abs(diag_elements))
    if (current_sum < min_sum) {
      min_sum <- current_sum
      best_perm <- perm
    }
  }
  
  W_permuted <- W[best_perm, ]
  
  # Step 3: Normalize rows by dividing by its corresponding diagonal element to have ones on the diagonal
  W_prime <- W_permuted
  for (i in 1:nrow(W_prime)) {
    W_prime[i, ] <- W_prime[i, ] / W_prime[i, i]
  }
  
  cat("Normalized Unmixing Matrix (W):\n")
  print(W_prime)
  
  # Step 4: Compute B as I - W_prime
  I <- diag(nrow(W_prime))
  B <- I - W_prime
  
  #Step 5: Find the permutation matrix P (applied equally to both rows and columns) of B which yields a matrix B = PBPT which is as close as possible to strictly lower triangular. This can be measured for instance using ∑i≤ j B2i j.
  lower_tri_cost <- function(B) {
    sum(B[upper.tri(B)]^2)
  }
  
  # Find the best permutation
  perms <- permutations(nrow(B), nrow(B))
  min_cost <- Inf
  best_perm <- NULL
  for (perm in seq_len(nrow(perms))) {
    perm_matrix <- B[perms[perm, ], perms[perm, ]]
    cost <- lower_tri_cost(perm_matrix)
    if (cost < min_cost) {
      min_cost <- cost
      best_perm <- perms[perm, ]
    }
  }
  
  P <- diag(nrow(B))[best_perm, ]
  B_permuted <- P %*% B %*% t(P)
  
  B_permuted[upper.tri(B_permuted)] <- 0
  
  
  # Step 6: Generate the Adjacency Matrix
  adjacency_matrix <- calculate_adjacency_matrix(B_permuted)
  
  return(list(B=B_permuted, adjacency_matrix =adjacency_matrix))
}

# Wald Test Function for Pruning Edges
  wald_test <- function(B, significance_level = 0.05) {
    n <- nrow(B)
    
    # Calculate the asymptotic variances of the elements of B
    avar <- matrix(0, n, n)
    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          avar[i, j] <- B[i, j]^2  # Assuming B[i, j] is Bij
        }
      }
    }
    
    # Calculate the Wald statistics
    wald_stat <- B^2 / avar
    
    # Calculate the critical value for the chi-square distribution with 1 degree of freedom
    critical_value <- qchisq(1 - significance_level, df = 1)
    
    # Prune the B matrix based on the Wald test
    B_pruned <- ifelse(wald_stat >= critical_value, B, 0)
    
    # Generate the Adjacency Matrix
    adjacency_matrix_pruned <- calculate_adjacency_matrix(B_pruned)
    
    return(list(B_pruned, adja))
  }
  
 plot_causality_graph(adjacency_matrix, colnames, rownames, datasetName) {
   colnames(adjacency_matrix) <- colnames
   rownames(adjacency_matrix) <- rownames
   title = datasetName + " DAG"
   graph <- graph.adjacency(adjacency_matrix, mode = "directed")
   plot(graph, 
        vertex.label = colnames(adjacency_matrix),  # Use column names as vertex labels
        main = title,
        vertex.size = 30,               # Set the size of vertices (nodes)
        vertex.color = "lightblue",     # Set the color of vertices (nodes)
        vertex.frame.color = "black",   # Set the color of the border around vertices
        vertex.label.color = "black",   # Set the color of vertex labels
        vertex.label.cex = 0.8          # Set the size of vertex labels
   )
 }

