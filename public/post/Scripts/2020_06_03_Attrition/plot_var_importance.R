plot_var_importance <- function(data) {

data <- ggplot(data = data, aes(x = variable, y = scaled_importance), interactive = FALSE) +
  geom_bar(stat = "identity", fill = "#D3D3D3") +
  
  geom_text(aes(label=(round(scaled_importance, digits = 2)*100)), color="black", size=3.0, nudge_y = -0.05) +
  
  coord_flip() +
  xlab("") +
  ylab("") +
  
  theme_minimal() +
  
  labs(title = "", 
       x = "", 
       y = "H2O's Variable Importance (max = 100)") +
  
  theme(axis.text.x = element_text(color="#ffffff"),
        axis.text.y = element_text(color="#000000"),
        
        axis.line = element_line(colour = "gray75"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
	axis.text = element_text(size=9),
	axis.title = element_text(size = 9))

}
