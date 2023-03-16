# ==============================================================================
# EXCAVATION FEATURES WITH SOIL LABELS
# ==============================================================================

set.seed(7)

# ------------------------------------------------------------------------------
# Data preparation
# ------------------------------------------------------------------------------
# load data
df <- read_csv('../data/df.csv')
df$SoilLabel <- as.factor(df$SoilLabel)

# create df excavation features
df_excav_soil <- df %>%
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
           contains('screwrotspeed') |
           contains('SoilLabel')) %>%
  select(!contains('rotspeed1') &
           !contains('rotspeed2') &
           !contains('presup')&
           !contains('preslow')) %>%
  # select only obs with label
  .[(.$SoilLabel != 'Unlabeled'), ]

# shorten long colnames
colnames(df_excav_soil)[3] <- "CutterRotSp"
colnames(df_excav_soil)[4] <- "CutterForce"
colnames(df_excav_soil)[7] <- "AdvanceSp"
colnames(df_excav_soil)[8] <- "PenetRate"
colnames(df_excav_soil)[10] <- "ChamberPres"
colnames(df_excav_soil)[11] <- "ScrewRotSp"

# ------------------------------------------------------------------------------
# BN graph at chainage 23000
# ------------------------------------------------------------------------------

# boot strength bnsl
str_excav_soil <- boot.strength(df_excav_soil[1:151, 2:ncol(df_excav_soil)], 
                                R = 200, algorithm = "tabu")#,
                                # algorithm.args = list(score = "bic-g"))

# select threshold
avg_excav_soil <- averaged.network(str_excav_soil, threshold = 0.5)

# plot
gR <- graphviz.plot(avg_excav_soil, layout = "dot", shape = "rectangle", 
                    sub = "(a)",
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
nodeRenderInfo(gR)$fill[c("SoilLabel")] = "orange"
edgeRenderInfo(gR)$col[c("AdvanceSp~PenetRate",
                         "CutterRotSp~PenetRate")] = "red"
edgeRenderInfo(gR)$col[c("PenetRate~FoamVol",
                         "AdvanceSp~FoamVol",
                         "AdvanceSp~ChamberPres",
                         "ChamberPres~AdvanceSp",
                         "ChamberPres~ScrewRotSp",
                         "AdvanceSp~ScrewRotSp")] = "blue"

# save figure
w <- 7
pdf(file = "../figs/fig-4a.pdf",
    width = w, height = w/1.5, 
    useDingbats = F) 
renderGraph(gR)
dev.off()

# ------------------------------------------------------------------------------
# BN graph at chainage 28000
# ------------------------------------------------------------------------------

# boot strength bnsl
str_excav_soil <- boot.strength(df_excav_soil[1:408, 2:ncol(df_excav_soil)], 
                                R = 200, algorithm = "tabu")

# select threshold
avg_excav_soil <- averaged.network(str_excav_soil, threshold = 0.5)

# plot
gR <- graphviz.plot(avg_excav_soil, layout = "dot", shape = "rectangle", 
                    sub = "(b)",
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
nodeRenderInfo(gR)$fill[c("SoilLabel")] = "orange"
edgeRenderInfo(gR)$col[c("AdvanceSp~PenetRate",
                         "CutterRotSp~PenetRate")] = "red"
edgeRenderInfo(gR)$col[c("PenetRate~FoamVol",
                         "AdvanceSp~FoamVol",
                         "AdvanceSp~ChamberPres",
                         "ChamberPres~AdvanceSp",
                         "ChamberPres~ScrewRotSp",
                         "AdvanceSp~ScrewRotSp")] = "blue"

# save figure
w <- 7
pdf(file = "../figs/fig-4b.pdf",
    width = w, height = w/1.5, 
    useDingbats = F) 
renderGraph(gR)
dev.off()

# ==============================================================================
