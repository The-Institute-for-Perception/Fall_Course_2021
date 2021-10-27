### Report Generation Functions ###

add_graphical_slide <- function(presentation, title, plot) {
  
  presentation <- officer::add_slide(presentation, layout = "Title and Content", master = "Office Theme") %>% 
    officer::ph_with(rvg::dml(ggobj = plot), location = officer::ph_location_type(type = "body")) %>% 
    officer::ph_with(title, location = officer::ph_location_type(type="title"))
}

add_title_slide <- function(presentation, title) {
  
  presentation <- officer::add_slide(presentation, layout = "Title Slide", master = "Office Theme") %>% 
    officer::ph_with(title, location = officer::ph_location_type(type="ctrTitle"))
}

add_comparison_slide <- function(presentation, title, text1, plot1, text2, plot2) {
  
  presentation <- officer::add_slide(presentation, layout = "Comparison", master = "Office Theme") %>% 
    officer::ph_with(title, location = officer::ph_location_label("Title 1")) %>% 
    officer::ph_with(text1, location = officer::ph_location_label("Text Placeholder 2")) %>% 
    officer::ph_with(rvg::dml(ggobj = plot1), location = officer::ph_location_label("Content Placeholder 3")) %>% 
    officer::ph_with(text1, location = officer::ph_location_label("Text Placeholder 4")) %>% 
    officer::ph_with(rvg::dml(ggobj = plot2), location = officer::ph_location_label("Content Placeholder 5"))
}
