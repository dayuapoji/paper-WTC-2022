# PLOT DIRECTION MAP

map_str <- function(link_str) {#, link_dir,  
                    # thres_str = 0.3, 
                    # thres_dir = 0.3) {#, 
                    # filename){
  
  # plot heatmap direction
  # select_str <- link_str %>%
  #   # select controlling direction
  #   subset(., rowMeans(link_dir[ , 500:(ncol(link_dir)-1)]) >= thres_dir) %>%
  #   # remove links with average strength < 0.5
  #   subset(., rowMeans(.[ , 500:(ncol(.)-1)]) >= thres_str) #%>%
    
  # long format
  link_str_long <- melt(link_str, id = "Links")
  link_str_long$variable <- as.numeric(as.character(link_str_long$variable))
  
    # plot
  fig <- ggplot(link_str_long) +
    geom_tile(aes(x = variable, y = Links, fill = value, width = 10)) +
    scale_fill_gradient2(name = "Link Strength",
                         midpoint = 0.5,
                         low = "white",
                         mid = "grey",
                         high = "black") +
    scale_x_continuous(limit = c(20500, 28800),
                       breaks = seq(20500, 28800, 500)) +
    ggtitle("(b)") +
    xlab("Chainage") + ylab("") +
    theme_bw(base_size = text_size) +
    theme(legend.key.size = unit(0.3, 'cm'),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(fig)
}

# save as pdf
# pdf(file = paste0("../figs/",filename,".pdf"))
# plot(fig)
# dev.off()
