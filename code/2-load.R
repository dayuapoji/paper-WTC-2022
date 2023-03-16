# ==============================================================================
# LOAD DATA
# ==============================================================================

# load tbm data
df <- read_csv('../data/df.csv')

# load geologic map
geomap <- readPNG('../../../../2-data/seattle/geomap.png', 
                  native = TRUE,
                  info = TRUE)

# Setup ------------------------------------------------------------------------

colors <- c('CCS'="blue",
            'CCS_CSG'="dodgerblue",
            'CSGCSF_CCS'= "sandybrown",
            'CSGCSF_TLTD'= 'pink2',
            'TLTD_CSGCSF_CCS'= "purple",
            'Unlabeled'='gray',
            'CCS_TLTD'='yellow')

# set font
text_size <- 6
# fig_size <- 6

# pdf weight
w <- 7

# Plots ------------------------------------------------------------------------

# plot geologic map
fig_geomap <- ggplot() +
  annotation_raster(geomap,
                    xmin = 20000, xmax = 28800,
                    ymin = 0, ymax = 1) +
  # xlim(20000, 28800) +
  ylim(0, 1) +
  scale_x_continuous(limit = c(20500, 28800),
                     breaks = seq(20500, 28800, 500)) +
  # labels = geo_label$BH) +
  ggtitle("(a)") +
  theme_classic(base_size = text_size) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# plot boreholes
fig_bh <- ggplot() + 
  geom_segment(data = df,
               mapping =  aes(x = Chainage, xend = Chainage, 
                              y = 0, yend = 0.75,
                              color = SoilLabel),
               size=0.3, alpha=0.5) +
  scale_color_manual(values = colors) +
  scale_x_continuous(limit = c(20500, 28800),
                     breaks = seq(20500, 28800, 500)) +
  ggtitle("(b)") +
  ylim(0, 1) +
  ylab('Soil Label') +
  xlab('Chainage (ft)') +
  # labs(color = 'Soil Label') +
  guides(color = guide_legend(nrow = 1)) +
  theme_classic(base_size = text_size) + 
  theme(legend.title = element_blank(), #element_text(size = 5),
        legend.text = element_text(size = text_size),
        legend.position = c(.5, 0.9),
        legend.key.size = unit(0.3, 'cm'),
        # legend.position = "top",
        # axis.title = element_text(size = text_size),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

fig5 <- ggarrange(fig_geomap, fig_bh, ncol = 1, heights = c(1, 0.5))

# save figure ------------------------------------------------------------------

pdf(file = "../figs/fig-5.pdf",
    width= w, height = w/2.5,
    useDingbats = F)
fig5
dev.off()




