plot_LIME_Shapley <- function(data) {
  ggplot(data = data, aes(x = reorder(feature_desc, Importance), 
                          y = Importance, 
                          fill = Importance), 
         interactive = FALSE) +
    geom_bar(stat = "identity",
             show.legend = TRUE) +
    
    scale_fill_gradient2(low="#202020", mid="#D3D3D3", high="#e60000") +
    
    coord_flip() +
    xlab("") +
    ylab("") +
    
    theme_minimal() +
    
    labs(title = "", 
         x = "", 
         y = "Supports Staying (Gray) |---------------------------| Supports Leaving (Red)") +
    
    theme(axis.text.x = element_text(color="#ffffff"),
          axis.text.y = element_text(color="#000000"),
          
          axis.line = element_line(colour = "gray75"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
	  axis.text = element_text(size=9),
	  axis.title = element_text(size = 9),

	  legend.text = element_text(size=7),
          legend.title = element_text(size=8),

          )
}