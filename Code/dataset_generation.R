# Set seed for reproducibility
set.seed(123)

# Generate non-Gaussian linear data
n <- 1000  # number of samples
p <- 5     # number of variables

# Create a random linear model with non-Gaussian noise
B <- matrix(c(
  0, 0, 0, 0, 0,
  0.5, 0, 0, 0, 0,
  0.5, 0, 0, 0, 0,
  0, 0.5, 0, 0, 0,
  0, 0, 0.5, 0.5, 0
), byrow = TRUE, ncol = p)

# Generate non-Gaussian noise (e.g., uniform distribution)
noise <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)

# Initialize data matrix
X <- matrix(0, nrow = n, ncol = p)

# Generate data according to the linear model with non-Gaussian noise
for (i in 1:p) {
  X[, i] <- noise[, i]
  if (i > 1) {
    for (j in 1:(i-1)) {
      X[, i] <- X[, i] + B[i, j] * X[, j]
    }
  }
}


df <- as.data.frame(X)
colnames(df) <- paste0("V", 1:p)
# Save dataset to file
write.csv(df, "../Dataset/generated_data.csv", row.names = FALSE)
