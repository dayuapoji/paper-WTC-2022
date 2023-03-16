plot_scaled_df <- function(df) {
  
  # scaling data to mean = 1, sd = 1
  scaled_df <- cbind(df['Chainage'], scale(df[, 2:ncol(df)]))  
  # melt scaled df
  melted_df <- reshape2::melt(data.table(scaled_df), id = "Chainage")
  # plot
  fig <- ggplot(melted_df) +
    geom_line(aes(x = Chainage, y = value, color = variable),
              size = 0.1) +
    ggtitle("(a)") +
    scale_x_continuous(limit = c(20500, 28800),
                       breaks = seq(20500, 28800, 500)) +
    labs(color = "Features") +
    xlab("Chainage") + ylab("Normalized Data") +
    theme_bw(base_size = text_size) +
    theme(legend.key.size = unit(0.3, 'cm'),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(), 
          # panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(fig)
}
