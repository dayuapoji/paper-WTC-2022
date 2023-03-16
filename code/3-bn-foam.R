# ==============================================================================
# GROUND CONDITIONING VOLUME
# ==============================================================================

set.seed(1)

# ------------------------------------------------------------------------------
# Data preparation
# ------------------------------------------------------------------------------
# load data
df$SoilLabel <- as.factor(df$SoilLabel)

# create df foam volumes
df_vol <- df %>%
  select(contains('chainage') |
           contains("foamvol") |
           contains("airvol") |
           contains("foamliquidvol") |
           contains("foamagentvol"))

# ------------------------------------------------------------------------------
# BN graph
# ------------------------------------------------------------------------------

# boot strength bnsl
str_vol <- boot.strength(df_vol[, 2:ncol(df_vol)], 
                         R = 200, algorithm = "tabu", 
                         algorithm.args = list(score = "bic-g"))

# select threshold
avg_vol <- averaged.network(str_vol, threshold = 0.9)

# plot
fig1 <- graphviz.plot(avg_vol, layout = "dot", shape = "rectangle", render = F)
nodeRenderInfo(fig1)$fontsize = 14
edgeRenderInfo(fig1)$col = "red"


# save figure
w <- 2.8
pdf(file = "../figs/fig-1.pdf",
    width = w, height = w/1.5, 
    useDingbats = F) 
renderGraph(fig1)
dev.off() 

# ------------------------------------------------------------------------------
# BN strength 
# ------------------------------------------------------------------------------
# set cluster
cluster <- 12
obs <- c(1:nrow(df_vol))
# obs <- c(200, 400, 600, 800, 1000, 1244)

# bnsl tabu
cl <- makeCluster(cluster)
system.time(
  links_vol_tabu <- get_links(df_vol, obs, "tabu", cl)
)

# create df strength and direction 
link_str <- links_vol_tabu[[1]]
link_dir <- links_vol_tabu[[2]]

# plot data
fig_data <- plot_scaled_df(df_vol)

# plot strength
fig_str <- map_str(link_str[(link_str$Links == "AirVol-FoamVol") |
                              (link_str$Links == "FoamLiquidVol-FoamVol") |
                              (link_str$Links == "FoamAgentVol-FoamLiquidVol"), ])

# plot direction
fig_dir <- map_dir(link_dir[(link_dir$Links == "AirVol-FoamVol") |
                              (link_dir$Links == "FoamLiquidVol-FoamVol") |
                              (link_str$Links == "FoamAgentVol-FoamLiquidVol"), ])
# combine figures
fig2 <- ggarrange(fig_data, fig_str, fig_dir, ncol = 1)

# save figs
w <- 7
pdf(file = "../figs/fig-2.pdf",
    width= w, height = w/2.5, useDingbats = F) 
fig2
dev.off() 

# ==============================================================================

