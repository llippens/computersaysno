if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, ggeffects, hrbrthemes, viridis, forcats, install = TRUE, update = FALSE)

# Define a function for creating a ggplot of model predictions
ggplot_prediction_lnr <- function(model,                   # Model object
                                  rev = FALSE,             # Plot x or fct_rev(x) on x-axis
                                  term,                    # Term to predict
                                  vcov_type = "HC1",       # Variance-covariance matrix type
                                  vcov_args = list(cluster = ~vac_uid), # Arguments for the vcov function
                                  colours) { 
  
  # Generate predictions using the ggpredict function
  predictions <- ggpredict(model, terms = term, vcov.type = vcov_type, vcov.args = vcov_args)
  
  if(rev == TRUE){
    predictions$x <- fct_rev(predictions$x)
  }
  
  # Create a ggplot object with the predictions
  p <- ggplot(predictions,
              aes(x,
                  predicted, 
                  ymin = conf.low, ymax = conf.high,
                  colour = x)) +
    geom_errorbar(width = 0,
                  linewidth = 1) +                               # Add error bars
    geom_point(size = 3,
               colour = "white") +                               # Add points (white background)
    geom_point(size = 2) +                                       # Add points (actual color)
    scale_y_continuous(breaks = seq(0,100,1),
                       minor_breaks = NULL) +                    # Scale y-axis as a percentage
    scale_colour_manual(values = colours) +                      # Apply viridis color scale
    coord_flip(ylim = c(floor(min(predictions$conf.low))-.05,
                        ceiling(max(predictions$conf.high))+.05)) +
    # Flip coordinates and set y-axis limits
    theme_minimal() +                                            # Apply minimal theme
    theme(title = element_text(family = family),
          text = element_text(family = family, size = 11),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(
            colour = "gray90", linewidth = 0.25),
          axis.ticks.x = element_line(
            colour = "gray90", linewidth = .3),
          axis.ticks.y = element_blank(),
          axis.text = element_text(family = family, size = 11),
          axis.title.y = element_text(
            margin = margin(t = 0, r = 10, b = 0, l = 0),
            angle = 90,
            colour = "gray20",
            size = 12,
            vjust = .5,
            hjust = .5),
          axis.title.x = element_text(
            angle = 0,
            margin = margin(t = 10, r = 0, b = 0, l =0),
            colour = "gray20",
            size = 12),
          strip.text.y.right = element_text(
            angle = 0, hjust = 0.5),
          plot.title.position = "plot",
          panel.spacing = unit(10, "points"),
          legend.position = "none"
    ) +
    labs(x = "",
         y = "Mean GPT-3.5 Interview Invitation Score")
  
  # Return the ggplot object
  return(p)
}