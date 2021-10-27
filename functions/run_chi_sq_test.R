run_chi_sq_test <- function(standards_data, test_vector) {
  
  cholesky_matrix <- t(chol(var(standards_data) / 12))
  
  standards_means <- apply(standards_data, 2, mean)
  
  1.0 - pchisq(sum((solve(cholesky_matrix) %*% (test_vector-standards_means))^2), length(test_vector))
}
