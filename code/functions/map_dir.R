# PLOT DIRECTION MAP

map_dir <- function(link_dir) {#, link_str , 
                    # thres_str = 0.5, 
                    # thres_dir = 0.5) {#, 
                    # filename){
  
  # plot heatmap direction
  # select_dir <- link_dir %>%
  #   # remove links with average strength < 0.5
  #   subset(., rowMeans(link_str[ , 500:(ncol(link_str)-1)]) >= thres_str) %>%
  #   # select controlling direction
  #   subset(., rowMeans(.[ , 500:(ncol(.)-1)]) >= thres_dir)
  
  # long format
  link_dir_long <- melt(link_dir, id = "Links")
  link_dir_long$variable <- as.numeric(as.character(link_dir_long$variable))
  
  # plot
  fig <- ggplot(link_dir_long) +
    geom_tile(aes(x = variable, y = Links, fill = value, width = 10)) +
    scale_fill_gradient2(name = "Link Direction",
                         midpoint = 0.5,
                         low = "red",
                         mid = "white",
                         high = "blue") +
    scale_x_continuous(limit = c(20500, 28800),
                       breaks = seq(20500, 28800, 500)) +
    ggtitle("(c)") +
    xlab("Chainage (ft)") + ylab("") +
    theme_bw(base_size = text_size) +
    theme(legend.key.size = unit(0.3, 'cm'),
          # axis.title.x = element_blank(),
          # axis.text.x = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(fig)
}

# save as pdf
# pdf(file = paste0("../figs/",filename,".pdf"))
# plot(fig)
# dev.off()
