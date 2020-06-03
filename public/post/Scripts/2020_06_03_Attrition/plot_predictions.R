plot_predictions <- function(data) {
  
  ggplot(data = data, aes(x = Employee, y = Probability), interactive = FALSE) +
    
    geom_bar(stat = "identity", fill = "#D3D3D3") +
    
    geom_text(aes(label = Probability_Label), 
              color="black", size=3.0, nudge_y = -0.05) +
    
    coord_flip() +
    xlab("") +
    ylab("") +
    theme_minimal() +
    labs(title = "", 
         x = "",
         y = "Individual Employee Probability of Leaving") +
    
    theme(axis.text.x = element_text(color="#ffffff"),
          axis.text.y = element_text(color="#000000"),
          
          axis.line = element_line(colour = "gray75"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
	  axis.text = element_text(size=9),
	  axis.title = element_text(size = 9)) +

    geom_hline(yintercept=0.089, linetype="solid", color = "red", size = 1, show.legend = FALSE) +
	  geom_text(aes(0.089, 0, label = "Median = 8.9%", vjust = -2, hjust = -0.85), 
		    size = 3)
	  

}