### PowerPoint Report Generation Example - Descriptive Spider Plots
###
### This script demonstrates how to plot descriptive data and then export the plots to PowerPoint
###

# load libraries
library(tidyverse)

# load local functions
source("functions/plotting_functions.R")
source("functions/report_generation_functions.R")

# load data
protein_bar_descriptive_data <- readxl::read_xlsx("data/protein_bar_descriptive_data.xlsx")

# create the plots
protein_bar_descriptive_plots <- protein_bar_descriptive_data %>% 
  gather(key = "attribute", value = "value", -Product) %>% 
  group_by(Product) %>% 
  nest() %>% 
  mutate(plot = map(data, make_spider_plot, scale_bounds = c(1, 15))) %>% 
  select(Product, plot) %>% 
  deframe()

# create the PowerPoint presentation
presentation <- officer::read_pptx()

add_title_slide(presentation, "Protein Bar Descriptive Plots")

iwalk(protein_bar_descriptive_plots, function(plot, name) {
  presentation <- presentation %>% 
    add_graphical_slide(name, plot)
})

# output/save the presentation
print(presentation, target = "output/Protein Bar Descriptive Plots.pptx")
