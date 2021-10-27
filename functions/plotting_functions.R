### Functions for creating various charts ###

# make_parallel_coord_chart
#
# Creates a parallel line chart for comparing multivariate
#   descriptive data.
#
# Input: 
#        descriptive_data: A data frame with the following columns:
#           attribute: the name of each attribute
#           value: the value of that attribute
#           item: the name of the item corresponding to each value
#
#        scale_bounds: the lower and upper limit on the scale
#        normalize: if TRUE, values will be normalized (scaled to between 0 and 1)
#
make_parallel_coord_chart <- function(descriptive_data, scale_bounds = NULL, normalize = FALSE, show_legend = TRUE) {
  
  if(!is.null(scale_bounds)) {
    min_bound <- scale_bounds[1]
    max_bound <- scale_bounds[2]
  } else {
    min_bound <- min(descriptive_data$value)
    max_bound <- max(descriptive_data$value)
  }
  
  if(normalize) {
    
    descriptive_data <- descriptive_data %>% 
      mutate(value = scales::rescale(value, from = c(min_bound, max_bound)))
    
    min_bound <- 0
    max_bound <- 1
  }
  
  the_plot <- ggplot(descriptive_data %>%
                       mutate(attribute = factor(attribute, levels = unique(attribute), ordered = TRUE)) %>% 
                       mutate(color = ifelse(grepl("bound", item), "red", ifelse(grepl("standard", item), "green", "blue")),
                              linetype = ifelse(grepl("bound", item), "dashed", "solid")), 
                     aes(x = attribute, y = value, group = item, color = color, linetype = linetype)) +
    geom_point() +
    geom_line() +
    ylim(c(min_bound, max_bound)) +
    xlab("") +
    ylab("Score") +
    guides(linetype = "none") +
    scale_color_manual(values = c("red", "blue"),
                       breaks = c("red", "blue"),
                       labels = c("Bounds", "Test"),
                       name = "Source") + 
    scale_linetype_manual(values = c("solid", "dashed"),
                          breaks = c("solid", "dashed")) +
    theme_bw(base_size = 14)
  
  if(!show_legend) {
    the_plot <- the_plot +
      guides(color = "none")
  }
  
  return(the_plot)
}


# make_qc_chart
#
# Creates a quality control chart
#
# Input: 
#        qc_data: A data frame with the following columns:
#           run_number: the number associated with an evaluation
#           test_value: the statistical test value associated with an evaluation
#           
#        upper_limit: the upper limit on the test value for acceptability
#
make_qc_chart <- function(qc_data, upper_limit) {
  
  ggplot(qc_data, aes(x = run_number, y = test_value)) +
    geom_point() +
    geom_hline(aes(yintercept = upper_limit), color = "red", linetype = "dashed") +
    xlab("Run") +
    ylab("Test Value") +
    theme_bw(base_size = 18)
}


# make_spider_plot
#
# Creates a spider/radar chart for comparing multivariate
#   descriptive data.
#
# Input: 
#        descriptive_data: A data frame with the following columns:
#           attribute: the name of each attribute
#           value: the value of that attribute
#           item: the name of the item corresponding to each value
#
#        scale_bounds: the lower and upper limit on the scale
#        normalize: if TRUE, values will be normalized (scaled to between 0 and 1)
#
make_spider_plot <- function(descriptive_data, scale_bounds = NULL, normalize = FALSE) {
  
  if(!("item" %in% names(descriptive_data))) {
    descriptive_data <- descriptive_data %>% 
      mutate(item = "Item")
  }
  
  if(!is.null(scale_bounds)) {
    min_bound <- scale_bounds[1]
    max_bound <- scale_bounds[2]
  } else {
    min_bound <- min(descriptive_data$value)
    max_bound <- max(descriptive_data$value)
  }
  
  if(normalize) {
    
    descriptive_data <- descriptive_data %>% 
      mutate(value = scales::rescale(value, from = c(min_bound, max_bound)))
    
    min_bound <- 0
    max_bound <- 1

  }
  
  mid_val <- min_bound + 0.5 * (max_bound - min_bound)
  
  descriptive_data %>%
    select(attribute, item, value) %>% 
    mutate(attribute = factor(attribute, levels = unique(attribute), ordered = TRUE)) %>% 
    pivot_wider(names_from = "attribute", id_cols = "item") %>% 
    ggradar::ggradar(group.line.width = 1,
                     group.point.size = 3,
                     group.colours = c("red", "blue", "red"),
                     centre.y	= min_bound,
                     grid.min = min_bound,
                     grid.mid = mid_val,
                     grid.max = max_bound,
                     values.radar	= c(min_bound, mid_val, max_bound),
                     label.gridline.mid	= FALSE,
                     label.gridline.min = FALSE,
                     label.gridline.max = FALSE,
                     legend.position = "right",
                     background.circle.transparency = 0,
                     )
}


# make_descriptive_chart
#
# Creates a parallel line chart for displaying descriptive data
#
# Input: 
#        descriptive_data: A data frame with the following columns:
#           attribute: the name of each attribute
#           value: the value of that attribute
#           product: the name of the product with that value
#
#        scale_bounds: the lower and upper limit on the scale
#        normalize: if TRUE, values will be normalized (scaled to between 0 and 1)
#
make_descriptive_chart <- function(descriptive_data, scale_bounds = NULL, normalize = FALSE, show_legend = TRUE) {
  
  if(!("product" %in% names(descriptive_data))) {
    descriptive_data <- descriptive_data %>% 
      mutate(product = "Product")
  } else {
    descriptive_data <- descriptive_data %>% 
      rename(Product = product)
  }
  
  if(!is.null(scale_bounds)) {
    min_bound <- scale_bounds[1]
    max_bound <- scale_bounds[2]
  } else {
    min_bound <- min(descriptive_data$value)
    max_bound <- max(descriptive_data$value)
  }
  
  if(normalize) {
    
    descriptive_data <- descriptive_data %>% 
      mutate(value = scales::rescale(value, from = c(min_bound, max_bound)))
    
    min_bound <- 0
    max_bound <- 1
  }
  
  the_plot <- ggplot(descriptive_data %>%
                       mutate(attribute = factor(attribute, levels = unique(attribute), ordered = TRUE)), 
                       aes(x = attribute, y = value, color = product, group = product)) +
    geom_point() +
    geom_line() +
    ylim(c(min_bound, max_bound)) +
    xlab("") +
    ylab("Score") +
    theme_bw(base_size = 14)
  
  if(!show_legend) {
    the_plot <- the_plot +
      guides(color = "none")
  }
  
  return(the_plot)
}
