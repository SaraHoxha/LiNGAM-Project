# Load required libraries
library(MASS)     # for generating multivariate normal-distributed data

# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 1000

# Mean vector for the variables
mean_vec <- c(0, 0, 0, 0, 0)

# Define a lower triangular matrix L
L <- matrix(c(
  1.0, 0.0, 0.0, 0.0, 0.0,
  0.6, 0.8, 0.0, 0.0, 0.0,
  0.7, 0.3, 0.7, 0.0, 0.0,
  0.0, 0.5, 0.4, 0.6, 0.0,
  0.0, 0.0, 0.0, 0.3, 0.7
), nrow = 5, byrow = TRUE)

# Covariance matrix derived from L
cov_mat <- L %*% t(L)

# Check if covariance matrix is positive definite
eigen(cov_mat)$values  # All values should be positive

# Generate multivariate normal-distributed data
data_norm <- mvrnorm(n, mu = mean_vec, Sigma = cov_mat)

# Convert to multivariate t-distribution
df <- 3  # Degrees of freedom for t-distribution
data_t <- data_norm / sqrt(rchisq(n, df) / df)

# Combine the datasets into a dataframe
dataframe <- data.frame(A = data_t[,1],  # Variable A from t-distribution
                        B = data_t[,2],  # Variable B from t-distribution
                        C = data_t[,3],  # Variable C from t-distribution
                        D = data_t[,4],  # Variable D from t-distribution
                        E = data_t[,5])  # Variable E from t-distribution

# Print the first few rows of the dataset
write.csv(dataframe, "../Dataset/generated_data.csv", row.names = FALSE)
