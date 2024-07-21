#Import necessary libraries
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
calculate_adjacency_matrix <- function(B) {
  adjacency_matrix <- ifelse(B != 0, 1, 0)
  return(adjacency_matrix)
}

# Function to implement LINGAM based on algorithm steps of study paper
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
  
  cat("Normalized W_Prime:\n")
  print(W_prime)
  
  # Step 4: Compute B as I - W_prime
  I <- diag(nrow(W_prime))
  B <- I - W_prime
  
  #Step 5: Find the permutation matrix P (applied equally to both rows and columns) of B which yields a matrix B = PBPT which is as close as possible to strictly lower triangular. This can be measured for instance using ∑i≤ j B2i j.
  lower_tri_cost <- function(B) {
    sum(B[upper.tri(B)]^2)
  }
  
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
  
  # Step 6: Generate the Adjacency Matrix and causal order
  adjacency_matrix <- calculate_adjacency_matrix(B_permuted)
  causal_order <- colnames(data)[best_perm]
  
  # Step 7: Perform Wald Test
  wald_test_results <- perform_wald_test(data_centered, B_permuted)$significant
  return(list(causal_order=causal_order,B=B_permuted, adjacency_matrix =adjacency_matrix, wald_test_results= wald_test_results))
}

# Function to implement Wald Test to prune edges based on algorithm steps of study paper
perform_wald_test <- function(data_centered, B) {
  
  wald_test_results <- matrix(NA, nrow = nrow(B), ncol = ncol(B))
  rownames(wald_test_results) <- colnames(data_centered)
  colnames(wald_test_results) <- colnames(data_centered)
  
  # Define the significance level and critical value for chi-squared test
  alpha <- 0.05
  critical_value <- qchisq(1 - alpha, df = 1)
  
  for (i in 1:nrow(B)) {
    for (j in 1:ncol(B)) {
      if (B[i, j] != 0) {
        # Fit linear model for each coefficient
        model <- lm(data_centered[, i] ~ data_centered[, j])
        
        # Extract coefficient and standard error
        coef_estimate <- coef(model)[2]  # coefficient for data_centered[, j]
        std_error <- summary(model)$coefficients[2, 2]  # standard error of the coefficient
        
        # Compute Wald statistic as the formula from the paper
        wald_statistic <- (coef_estimate^2) / (std_error^2)
        
        # Calculate p-value from chi-squared distribution
        p_value <- 1 - pchisq(wald_statistic, df = 1)
        
        # Store p-value in results matrix
        wald_test_results[i, j] <- p_value
      }
    }
  }
  
  # Return results matrix and indicate which coefficients are significant
  list(
    p_values = wald_test_results,
    significant = wald_test_results < alpha
  )
}


# Plot causality graph based on adjacency_matrix
plot_causality_graph <- function(adjacency_matrix, pathName = NULL) {
  # Create an igraph object from the adjacency matrix
  g_estimated <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  
  png(filename = pathName, width = 800, height = 600)
  # Plot the graph
  plot(g_estimated, 
       vertex.label = V(graph)$name,  # Use vertex names as labels
       vertex.size = 70, 
       vertex.label.cex = 1.2, 
       edge.width = E(g_estimated)$weight * 2, 
       edge.color = "black", 
       vertex.color = "lightblue",    # Set the color of vertices (nodes)
       vertex.frame.color = "black",  # Set the color of the border around vertices
       vertex.label.color = "black",  # Set the color of vertex labels
       main = "Happiness Causal Graph")
  dev.off()
}
 
 is_dag <- function(adjacency_matrix) {
   graph <- graph.adjacency(adjacency_matrix, mode = "directed")
   return(is.dag(graph))
 }
 

