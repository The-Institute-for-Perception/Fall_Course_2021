# Calculate standard bounds #

calculate_standard_bounds <- function(standards_data) {
  
  standard_means <- map_dbl(standards_data, mean)
  std_dev <- map_dbl(standards_data, sd) / sqrt(12)
  enframe(standard_means, name = "attribute", value = "mean") %>% 
    mutate(lower_bound = standard_means - 1.96 * std_dev,
           upper_bound = standard_means + 1.96 * std_dev)
}
