plot_correlation_funnel_EO <- function(data,  interactive = FALSE, limits = c(-0.35, 0.35), alpha = 1) {
  UseMethod("plot_correlation_funnel", data)
}

#' @export
plot_correlation_funnel.default <- function(data,  interactive = FALSE, limits = c(-0.35, 0.35), alpha = 1) {
  stop("plot_correlation_funnel(): Object is not of class `data.frame`.", call. = FALSE)
}

#' @export
plot_correlation_funnel.data.frame <- function(data,  interactive = FALSE, limits = c(-0.35, 0.35), alpha = 1) {
  
  # Checks
  check_column_names(
    data,
    acceptable_column_names = c("feature", "bin", "correlation"),
    .fun_name = "plot_correlation_funnel")
  
  if (interactive) {
    
    data <- data %>%
      dplyr::mutate(label_text = stringr::str_glue("{feature}
                                         Bin: {bin}
                                         Correlation: {round(correlation, 3)}"))
    
    g <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = correlation, y = feature, text = label_text)) +
      
      # Geometries
      ggplot2::geom_vline(xintercept = 0, linetype = 1, color = "red") +
      ggplot2::geom_point(aes(color = "#000000",
                              alpha = alpha),
                          color = "black") +
      # ggrepel::geom_text_repel(ggplot2::aes(label = bin), size = 3, color = "#000000") +
      
      # Formatting
      ggplot2::scale_x_continuous(limits = limits) +
      
      theme_tq() +
      
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size     = 0.10, 
                                              linetype = "solid",
                                              colour   = "gray95"),
            axis.text.x = element_text(color="#000000"),
            axis.text.y = element_text(color="#000000"),
            axis.text = element_text(size=8),
	    axis.title = element_text(size = 9)
            ) +
      
      ggplot2::labs(title = "", x = "Supports Staying |----------------------------------| Supports Leaving", y = "")
    
    p <- plotly::ggplotly(g, tooltip = "text")
    
    return(p)
    
  } else {
    g <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = correlation, y = feature, text = bin)) +
      
      # Geometries
      ggplot2::geom_vline(xintercept = 0, linetype = 1, color = "red") +
      ggplot2::geom_point(aes(color = "#000000",
                              alpha = alpha),
                          color = "black") +
      
      ggrepel::geom_text_repel(ggplot2::aes(label = bin), size = 3, color = "#000000") +
      
      # Formatting
      ggplot2::scale_x_continuous(limits = limits) +
      
      theme_tq() +
      
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size     = 0.10, 
                                              linetype = "solid",
                                              colour   = "gray95"),
            axis.text.x = element_text(color="#000000"),
            axis.text.y = element_text(color="#000000"),
            axis.text = element_text(size=8),
	    axis.title = element_text(size = 9)
            ) +
      
      ggplot2::labs(title = "", x = "Supports Staying |----------------------------------| Supports Leaving", y = "")
    
    return(g)
  }
  
}

# Check column names of data
check_column_names <- function(data, acceptable_column_names, .fun_name) {
  
  if (any(!(names(data) %in% acceptable_column_names))) {
    
    msg1 <- paste0(.fun_name, "(): ")
    msg2 <- paste0("[Unnacceptable Data] Acceptable data is generated from the output of correlate().")
    
    msg  <- paste0(msg1, msg2)
    
    stop(msg, call. = FALSE)
  }
}


