process_data_types <- function(data) {
  data <- data %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.integer, as.double)
  }