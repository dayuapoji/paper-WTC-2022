# ==============================================================================
# EXCAVATION FEATURES
# ==============================================================================

set.seed(5)

# ------------------------------------------------------------------------------
# Data preparation
# ------------------------------------------------------------------------------
# load data
df$SoilLabel <- as.factor(df$SoilLabel)

# create df excavation features
df_excav <- df %>%
  select(contains('chainage') |
           contains('cutter') |
           contains('thrust')|
           contains('advance') |
           contains('penetration') |
           contains('foamvol') |
           # contains('airvol') |
           # contains('foamliquidvol') |
           # contains('foamagentvol') |
           # contains('polymeragentvol') |
           contains('ChamberSoilPres') |
           contains('screwrotspeed')) %>%
  select(!contains('rotspeed1') &
           !contains('rotspeed2') &
           !contains('presup')&
           !contains('preslow'))

# shorten long colnames
colnames(df_excav)[3] <- "CutterRotSp"
colnames(df_excav)[4] <- "CutterForce"
colnames(df_excav)[7] <- "AdvanceSp"
colnames(df_excav)[8] <- "PenetRate"
colnames(df_excav)[10] <- "ChamberPres"
colnames(df_excav)[11] <- "ScrewRotSp"

# ------------------------------------------------------------------------------
# BN graph at chainage 23000
# ------------------------------------------------------------------------------

# boot strength bnsl
str_excav <- boot.strength(df_excav[1:373, 2:ncol(df_excav)], 
                           R = 200, algorithm = "tabu", 
                           algorithm.args = list(score = "bic-g"))

# select threshold
avg_excav <- averaged.network(str_excav, threshold = 0.5)

# plot
gR <- graphviz.plot(avg_excav, layout = "dot", shape = "rectangle", sub = "(a)",
                    render = F)

# clustering nodes
sg1 <- list(graph = subGraph(c("ThrustForce",
                               "CutterTorque",
                               "CutterForce"), gR), cluster = TRUE)
sg2 <- list(graph = subGraph(c("AdvanceSp",
                               "PenetRate",
                               "CutterRotSp"), gR), cluster = TRUE)
sg3 <- list(graph = subGraph(c("ChamberPres",
                               "ScrewRotSp"), gR), cluster = TRUE)
gR <- layoutGraph(gR, subGList = list(sg1, sg2, sg3),
                  attrs = list(graph = list(rankdir = "LR")))

# set font size
nodeRenderInfo(gR)$fontsize = 16

# set nodel colors
nodeRenderInfo(gR)$fill[c("FoamVol")] = "grey"
nodeRenderInfo(gR)$fill[c("AdvanceSp",
                          "CutterRotSp",
                          "ScrewRotSp")] = "skyblue"
nodeRenderInfo(gR)$fill[c("ThrustForce",
                          "CutterTorque",
                          "CutterForce")] = "tomato"
nodeRenderInfo(gR)$fill[c("ChamberPres")] = "yellow"
nodeRenderInfo(gR)$fill[c("PenetRate")] = "lightgreen"

# set link colors
edgeRenderInfo(gR)$col[c("AdvanceSp~PenetRate",
                      "CutterRotSp~PenetRate")] = "red"
edgeRenderInfo(gR)$col[c("PenetRate~FoamVol",
                     "AdvanceSp~FoamVol",
                     "AdvanceSp~ChamberPres",
                     "ChamberPres~ScrewRotSp",
                     "AdvanceSp~ScrewRotSp")] = "blue"

# save figure
w <- 7
pdf(file = "../figs/fig-3a.pdf",
    width = w, height = w/1.5, 
    useDingbats = F) 
renderGraph(gR)
dev.off() 
  
# ------------------------------------------------------------------------------
# BN graph at chainage 28000
# ------------------------------------------------------------------------------

# boot strength bnsl
str_excav <- boot.strength(df_excav[1:1133, 2:ncol(df_excav)],  
                           R = 200, algorithm = "tabu", 
                           algorithm.args = list(score = "bic-g"))

# select threshold
avg_excav <- averaged.network(str_excav, threshold = 0.5)

# plot
gR <- graphviz.plot(avg_excav, layout = "dot", shape = "rectangle", sub = "(b)",
                    render = F)

# clustering nodes
sg1 <- list(graph = subGraph(c("ThrustForce",
                               "CutterTorque",
                               "CutterForce"), gR), cluster = TRUE)
sg2 <- list(graph = subGraph(c("AdvanceSp",
                               "PenetRate",
                               "CutterRotSp"), gR), cluster = TRUE)
sg3 <- list(graph = subGraph(c("ChamberPres",
                               "ScrewRotSp"), gR), cluster = TRUE)
gR <- layoutGraph(gR, subGList = list(sg1, sg2, sg3),
                  attrs = list(graph = list(rankdir = "LR")))

# set font size
nodeRenderInfo(gR)$fontsize = 16

# set nodel colors
nodeRenderInfo(gR)$fill[c("FoamVol")] = "grey"
nodeRenderInfo(gR)$fill[c("AdvanceSp",
                          "CutterRotSp",
                          "ScrewRotSp")] = "skyblue"
nodeRenderInfo(gR)$fill[c("ThrustForce",
                          "CutterTorque",
                          "CutterForce")] = "tomato"
nodeRenderInfo(gR)$fill[c("ChamberPres")] = "yellow"
nodeRenderInfo(gR)$fill[c("PenetRate")] = "lightgreen"

# set link colors
edgeRenderInfo(gR)$col[c("AdvanceSp~PenetRate",
                         "CutterRotSp~PenetRate")] = "red"
edgeRenderInfo(gR)$col[c("PenetRate~FoamVol",
                         "AdvanceSp~FoamVol",
                         "AdvanceSp~ChamberPres",
                         "ChamberPres~ScrewRotSp",
                         "AdvanceSp~ScrewRotSp")] = "blue"

# save figure
w <- 7
pdf(file = "../figs/fig-3b.pdf",
    width = w, height = w/1.5, 
    useDingbats = F) 
renderGraph(gR)
dev.off() 


# ------------------------------------------------------------------------------
# BN strength 
# ------------------------------------------------------------------------------

# set cluster
cluster <- 12
obs <- c(1:nrow(df_excav))
# obs <- c(200, 400, 600, 800, 1000, 1244)

# tabu
cl <- makeCluster(cluster)
system.time(
  links_excav_tabu <- get_links(df_excav, obs, "tabu", cl)
)
# create df strength and direction 
link_str <- links_excav_tabu[[1]]
link_dir <- links_excav_tabu[[2]]

# ------------------------------------------------------------------------------
# Longitudinal plot - penetration 
# ------------------------------------------------------------------------------

# select links
selected_links <- c("AdvanceSp-PenetRate",
                    "CutterRotSp-PenetRate",
                    "PenetRate-FoamVol")
selected_feats <- c("Chainage", "FoamVol", 
                    "AdvanceSp", "CutterRotSp", "PenetRate")
# plot data
fig_data <- plot_scaled_df(df_excav[, selected_feats])
# plot strength
fig_str <- map_str(link_str %>% filter(., Links %in% selected_links))
# plot direction
fig_dir <- map_dir(link_dir %>% filter(., Links %in% selected_links))

# combine figures
fig6 <- ggarrange(fig_data, fig_str, fig_dir, ncol = 1)

# save figure
pdf(file = "../figs/fig-6.pdf",
    width = w, height = w/2.5, 
    useDingbats = F) 
fig6
dev.off() 

# ------------------------------------------------------------------------------
# Longitudinal plot - chamber pressure
# ------------------------------------------------------------------------------

# select links
selected_links <- c("ScrewRotSp-ChamberPres",
                    "AdvanceSp-ChamberPres")
selected_feats <- c("Chainage", 
                    "AdvanceSp", "ScrewRotSp", "ChamberPres")

# plot data
fig_data <- plot_scaled_df(df_excav[, selected_feats])
# plot strength
fig_str <- map_str(link_str %>% filter(., Links %in% selected_links))
# plot direction
fig_dir <- map_dir(link_dir %>% filter(., Links %in% selected_links))
# combine figures
fig7 <- ggarrange(fig_data, fig_str, fig_dir, ncol = 1)

# save figure
pdf(file = "../figs/fig-7.pdf",
    width = w, height = w/2.5,
    useDingbats = F) 
fig7
dev.off()

# ==============================================================================
