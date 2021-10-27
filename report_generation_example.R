### PowerPoint Report Generation Example
###
### This script demonstrates how to plot descriptive data and then export the plots to PowerPoint
###

# load libraries
library(tidyverse)

# load local functions
source("functions/calculate_standard_bounds.R")
source("functions/run_chi_sq_test.R")
source("functions/plotting_functions.R")
source("functions/report_generation_functions.R")


# load data
sample_1 <- readxl::read_xlsx(path = "data/Sample 1.xlsx")
sample_2 <- readxl::read_xlsx(path = "data/Sample 2.xlsx")
standards_data <- readxl::read_xlsx("data/Standards.xlsx")
qc_data <- readxl::read_xlsx(path = "data/Quality Control Tests.xlsx")


# run the multivariate tests
sample_1_p_value <- run_chi_sq_test(standards_data, sample_1$`Sample 1`)
sample_2_p_value <- run_chi_sq_test(standards_data, sample_2$`Sample 2`)


# calculate the standard bounds for a z-test
standard_bounds <- calculate_standard_bounds(standards_data)


# create the plots

sample_1_chart <- standard_bounds %>% 
  left_join(sample_1) %>% 
  select(-mean) %>% 
  rename(test = "Sample 1") %>% 
  pivot_longer(-attribute, names_to = "item", values_to = "value") %>% 
  make_parallel_coord_chart()

sample_2_chart <- standard_bounds %>% 
  left_join(sample_2) %>% 
  select(-mean) %>% 
  rename(test = "Sample 2") %>% 
  pivot_longer(-attribute, names_to = "item", values_to = "value") %>% 
  make_parallel_coord_chart()

upper_limit <- qchisq(0.95, 15)
qc_chart <- qc_data %>% 
  make_qc_chart(upper_limit)

# create the PowerPoint presentation

presentation <- officer::read_pptx()

add_title_slide(presentation, "Quality Control Presentation")

add_graphical_slide(presentation, 
                    title = paste0("Uni. Fails, Multi. Passes (p = ", round(sample_1_p_value, 2), ")"),
                    plot = sample_1_chart)

add_graphical_slide(presentation, 
                    title = paste0("Uni. Passes, Multi. fails (p = ", round(sample_2_p_value, 2), ")"),
                    plot = sample_2_chart)

add_graphical_slide(presentation,
                    title = "Multivariate Quality Control Chart",
                    qc_chart)


# output/save the presentation
print(presentation, target = "output/Quality Control Presentation.pptx")
