#### LIBRARIES ####

# library(readr)
# library(dplyr)
# library(ggplot2)
# library(magick)
# library(viridis)
# library(brms)
# 
# library(tidyr)
# library(purrr)
# library(vegan)
# library(bipartite)
# library(patchwork)
# library(rlist)
# library(network)
# library(sna)
# library(ergm)
# library(DiagrammeR)
# library(rsvg)
# library(cowplot)
# library(FactoMineR)
# library(Factoshiny)
# 
# library(rworldmap)
# library(ggrepel)
# 
# library(ggfortify)
# # library(ggConvexHull)
# library(ggpubr)
# library(ggcorrplot)
# library(corrplot)
# library(ggpmisc)
# library(GGally)
# library(ggbreak)
# library(scales)
# 
# library(DiagrammeRsvg)
# library(htmltools)
# library(svglite)
# 
# library(grid)
# 
# library(khroma)
# library(rstan)


#### MAKE FIG 1 ####

makeGraph <- function(list){
  # biP edge dataframe
  # n number of consumers
  # m number of resources
  
  biP <- list[[1]]
  n <- list[[2]]
  m <- list[[3]]
  S <- n + m
  
  colnames(biP) <- c("from_id", "to_id")
  biP$from_id <- as.character(biP$from_id)
  biP$to_id <- as.character(biP$to_id)
  
  node <- rbind(biP %>% select(from_id) %>% rename(label = from_id), 
                biP %>% select(to_id) %>% rename(label = to_id)) %>% 
    group_by(label) %>% summarise() %>% 
    mutate(id = seq(1:S)) %>%
    select(id, label)
  
  edge <- biP %>% 
    select(from_id, to_id) %>% 
    rename(from = from_id, to = to_id)
  edge <- left_join(edge, node, by = c("from" = "label"))
  edge <- edge %>% rename(from_label = from, from = id)
  edge <- left_join(edge, node, by = c("to" = "label"))
  edge <- edge %>% rename(to_label = to, to = id)
  
  graph <- create_graph(
    directed = T) %>%
    
    add_nodes_from_table(
      table = node,
      label_col = label) %>%
    
    set_node_attrs(
      node_attr = fontcolor,
      values = "black") %>% 
    
    set_node_attrs(
      node_attr = fillcolor,
      values = "#FEE08B",
      nodes = c(1:n)) %>%
    set_node_attrs(
      node_attr = color,
      values = "#FEE08B",
      nodes = c(1:n)) %>% 
    
    set_node_attrs(
      node_attr = fillcolor,
      values = "#66C2A5",
      nodes = c(n+1:m)) %>%
    set_node_attrs(
      node_attr = color,
      values = "#66C2A5",
      nodes = c(n+1:m)) %>%
    
    add_edges_from_table(
      table = edge,
      from_col = from,
      to_col = to,
      from_to_map = id_external) %>% 
    set_edge_attrs(
      edge_attr = color,
      value =  "grey50") %>% 
    set_edge_attrs(
      edge_attr = fillcolor,
      value =  "grey50")
  
  graph <- graph %>% 
    add_global_graph_attrs(attr = "width", value = 0.4, attr_type = "node") %>% 
    add_global_graph_attrs(attr = "fontsize", value = 12, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"), 
                           attr_type = c("graph", "graph", "graph"))
  
  plot <- render_graph(graph,
                       output = NULL,
                       as_svg = FALSE)
  
  return (plot)
}

# makeFig1_data <- function(){
#   
#   Fig1_data <- list(
#     graphA = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", "4", "4", # high S  and low C
#                                                "5", "6", "6", "7", "7", "8", "8", "9"),
#                                              ncol = 2)),
#                   n = 4,
#                   m = 5),
#     
#     graphB = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", # low S and high C 
#                                                "4", "5", "5", "4", "5", "4"),
#                                              ncol = 2)),
#                   n = 3,
#                   m = 2),
#     
#     graphC = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "2", "3", "3", "3", "4", "4", #regular 
#                                                "5", "6", "5", "6", "7", "6", "7", "8", "7", "8"),
#                                              ncol = 2)),
#                   n = 4,
#                   m = 4),
#     
#     graphD = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", "4", "4", # modular
#                                                "5", "6", "5", "6", "7", "8", "7", "8"),
#                                              ncol = 2)),
#                   n <- 4,
#                   m <- 4),
#     
#     graphE = list(biP = as.data.frame(matrix(c("1", "1", "1", "1", "2", "2", "2", "3", "3", "4", # nested
#                                                "5", "6", "7", "8", "6", "7", "8", "7", "8", "8"),
#                                              ncol = 2)),
#                   n = 4,
#                   m = 4))
#   
#   return(Fig1_data)
# }
# 
# #Fig1 <- makeFig1()

#### MAKE FIG 2 ####
# tar_load(bga)

make_zFig2 <- function(bga){
  
  bga <- bga %>%
    rename(N = NODF2, 
           Od = G, 
           Id = V,
           Region = Realm) %>%    
    mutate(Region_short = case_when(Region == "Tropical Eastern Pacific" ~ "TEP",
                                    Region == "Tropical Atlantic" ~ "TA",
                                    Region == "Eastern Indo-Pacific" ~ "WIP",
                                    Region == "Central Indo-Pacific" ~ "CIP",
                                    Region == "Western Indo-Pacific" ~ "EIP")) %>% 
    mutate(Region = base::factor(Region, levels = c("Tropical Eastern Pacific",
                                                    "Tropical Atlantic", 
                                                    "Eastern Indo-Pacific", 
                                                    "Central Indo-Pacific", 
                                                    "Western Indo-Pacific"))) %>% 
    mutate(Region_short = base::factor(Region_short, levels = c("TEP",
                                                                "TA", 
                                                                "WIP", 
                                                                "CIP", 
                                                                "EIP"))) %>% 
    ungroup() %>% 
    select(SiteCode, Region, Region_short, S, C, zN, zQn) %>% 
    rename(N = zN, Qn = zQn)
  
  archi <- bga %>% 
    # select(SiteCode, Region, Region_short, S, L, C, Bc, Od, Id, zN, zQn) %>% 
    mutate(S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
           # L = (log(L+1) - mean(log(L+1))) / sd(log(L+1)),
           C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
           # Bc = (log(Bc+1) - mean(log(Bc+1))) / sd(log(Bc+1)),
           # Od = (log(Od+1) - mean(log(Od+1))) / sd(log(Od+1)),
           # Id = (log(Id+1) - mean(log(Id+1))) / sd(log(Id+1)),
           # S = (S - mean(S)) / sd(S),
           # C = (C - mean(C)) / sd(C),
           N = (N - mean(N)) / sd(N),
           Qn = (Qn - mean(Qn)) / sd(Qn))
  
  #### pca ####
  draw_order <- c("Central Indo-Pacific", "Tropical Eastern Pacific", "Eastern Indo-Pacific", 
                  "Tropical Atlantic", "Western Indo-Pacific")
  archi_ordered <- archi %>%
    arrange(factor(Region, levels = draw_order))
  
  pca_res <- prcomp(archi_ordered[4:7], scale. = TRUE) 
  (pca <- autoplot(pca_res, data = archi_ordered, size = 2, colour = 'Region', fill = 'Region', alpha = 0.6,
                   loadings = TRUE, loadings.colour = 'grey15',
                   loadings.label = TRUE, loadings.label.colour = "black", loadings.label.size = 4, loadings.label.repel = T) +
      theme_minimal() +
      theme(
        title = element_text(size = 8),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        legend.position = "right") +
      scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")))
  
  #### boxplots ####
  # node number
  boxS <-  ggplot() +
    geom_boxplot(data = bga, aes(x = Region_short, y = S, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      axis.text = element_text(size = 7),
      legend.position = "none")
  
  # connectance
  boxC <- ggplot() +
    geom_boxplot(data = bga, aes(x = Region_short, y = C, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      axis.text = element_text(size = 7),
      legend.position = "none")
  
  # modularity
  boxQn <- ggplot() +
    geom_boxplot(data = bga, aes(x = Region_short, y = Qn, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      axis.text = element_text(size = 7),
      legend.position = "none")
  
  # nestedness
  boxN <- ggplot() +
    geom_boxplot(data = bga, aes(x = Region_short, y = N, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      axis.text = element_text(size = 7),
      legend.position = "none")
  
  box <- ggarrange(boxS, boxC, boxQn, boxN, nrow = 1, ncol = 4, labels = c("B", "C", "D", "E"),
                   font.label = list(size = 9, color = "black", face = "bold", family = 'Helvetica')
  )
  
  #### all #####
  (Fig2 <- ggarrange(pca, box, nrow = 2, ncol = 1, heights = c(2,1), labels = c("A", ""),
                     font.label = list(size = 9, color = "black", face = "bold", family = 'Helvetica')))
  
  ggsave(file = "output_figures/Fig2_PCA_Topo.pdf", dpi = 300, unit = "in", width = 7.25, height = 3.55*2)  #width=10, height=8
  
  return(Fig2)
}

# makeFig2_data <- function(){
#   Fig2_data <- list(
#     graphDiverse = list(biP <- as.data.frame(matrix(c("1", "2", "2", "3", "3", "3", "4", "4", "5", "5", "6", # diverse
#                                                       "7", "7", "8", "7", "8", "i", "8", "j", "i", "j", "j"),
#                                                     ncol = 2)),
#                         n <- 6,
#                         m <- 4),
#     
#     graphConnected = list(biP <- as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", "3", "3", # connected
#                                                         "4", "5", "5", "6", "4", "5", "6", "7"),
#                                                       ncol = 2)),
#                           n <- 3,
#                           m <- 4),
#     
#     graphModular = list(biP <- as.data.frame(matrix(c("1", "2", "2", "3", # modular
#                                                       "4", "5", "6", "6"),
#                                                     ncol = 2)),
#                         n <- 3,
#                         m <- 3))
#   return(Fig2_data)
# }

#### MAKE FIG 3 ####

# tar_load(bga)
# tar_load(flux_per_prey_Ic)

makeFig3 <- function(bga, flux_per_prey_Ic){
  
  bga <- bga %>%
    rename(N = NODF2, 
           Od = G, 
           Id = V,
           Region = Realm) %>%    
    mutate(Region_short = case_when(Region == "Tropical Eastern Pacific" ~ "TEP",
                                    Region == "Tropical Atlantic" ~ "TA", 
                                    Region == "Eastern Indo-Pacific" ~ "EIP",
                                    Region == "Central Indo-Pacific" ~ "CIP",
                                    Region == "Western Indo-Pacific" ~ "WIP")) %>% 
    mutate(Region = base::factor(Region, levels = c("Tropical Eastern Pacific",
                                                    "Tropical Atlantic", 
                                                    "Eastern Indo-Pacific", 
                                                    "Central Indo-Pacific", 
                                                    "Western Indo-Pacific"))) %>% 
    mutate(Region_short = base::factor(Region_short, levels = c("TEP",
                                                                "TA", 
                                                                "EIP", 
                                                                "CIP", 
                                                                "WIP"))) %>% 
    select(SiteCode, Region, Region_short, SiteLongitude, SiteLatitude, S, L, C, Bc, Od, Id, N, Qn) %>% 
    ungroup()
  
  
  flux <- flux_per_prey_Ic %>%
    select(site, bAut_prop:zooP_prop) %>%
    rename("bAut" = "bAut_prop",
           "det" = "det_prop",
           "fish" = "fish_prop",
           "mInv" = "mInv_prop",
           "sInv" = "sInv_prop",
           "zooP" = "zooP_prop",
           "SiteCode" = "site") 
  
  flux <- right_join(bga %>% select(SiteCode, Region, Region_short, SiteLongitude, SiteLatitude), 
                     flux, by = "SiteCode") %>% 
    filter(is.na(Region) == F)
  
  #### pca ####
  
  draw_order <- c("Central Indo-Pacific", "Tropical Eastern Pacific", "Eastern Indo-Pacific", 
                  "Tropical Atlantic", "Western Indo-Pacific")
  flux_ordered <- flux %>%
    arrange(factor(Region, levels = draw_order))
  
  pca_res <- prcomp(flux_ordered[6:11], scale. = TRUE)
  
  (pca <- autoplot(pca_res, data = flux_ordered, size = 2, colour = 'Region', fill = 'Region', alpha = 0.6,
                   loadings = TRUE, loadings.colour = 'grey15',
                   loadings.label = TRUE, loadings.label.colour = "black", loadings.label.size = 4, loadings.label.repel = T) +
      theme_minimal() +
      theme(
        title = element_text(size = 8),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        legend.position = "right") +
      scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")))
  
  #### boxplots flux #####
  # fish
  boxFish <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = fish, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "fish") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      axis.text = element_text(size = 7),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.8))
  
  # bAut
  boxbAut <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = bAut, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "bAut") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      axis.text = element_text(size = 7),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.8))
  
  # mInv
  boxmInv <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = mInv, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "mInv") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      axis.text = element_text(size = 7),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.8))
  
  # zooP
  boxzooP <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = zooP, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "zooP") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      axis.text = element_text(size = 7),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.3))
  
  # sInv
  boxsInv <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = sInv, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "sInv") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      axis.text = element_text(size = 7),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.3))
  
  # det
  boxdet <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = det, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "det") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      axis.text = element_text(size = 7),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.3))
  
  box <- ggarrange(boxbAut, boxmInv, boxFish, boxdet, boxzooP, boxsInv, nrow = 2, ncol = 3, 
                   labels = c("B", "C", "D", "E", "F", "G"),
                   font.label = list(size = 9, color = "black", face = "bold", family = 'Helvetica'))
  
  box <- annotate_figure(box, left = text_grob("Proportion of carbon flowing through major pathways", 
                                               color = "black", size = 9, rot = 90))
  
  #### all #####
  (all <- ggarrange(pca, box, nrow = 2, ncol = 1, heights = c(1,1), labels = c("A", ""),
                    font.label = list(size = 9, color = "black", face = "bold", family = 'Helvetica')))
  
  ggsave(file = "output_figures/Fig3_PCA_Flux.pdf", dpi = 300, unit = "in", width = 7.25, height = 7.8)  #width=10, height=8
  
  return(all)  
}

#### MAKE FIG 4 ####

# tar_load(fit_z_sem_5000)
# fit_sem <- fit_z_sem_5000

make_zFig4_SEM <- function(fit_sem){
  
  ##### Fe ####
  
  fe <- brms::fixef(fit_sem) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("name") %>%
    tidyr::separate(name, into = c("dep", "var"), sep = "_") %>%
    dplyr::filter(var != "Intercept") %>% 
    dplyr::mutate(dep = case_when(
      dep == "coral" ~ "Coral",
      dep == "algae" ~ "Algae",
      dep == "turf" ~ "Turf",
      dep == "S" ~ "Node number",
      dep == "C" ~ "Connectance",
      dep == "zQn" ~ "Modularity",
      dep == "zN" ~ "Nestedness",
      dep == "mInv" ~ "Mobile invertebrates",
      dep == "sInv" ~ "Sessile invertebrates",
      dep == "bAut" ~ "Benthic autotrophs",
      dep == "det" ~ "Detritus",
      dep == "fish" ~ "Fish",
      dep == "zooP" ~ "Zooplankton",
      TRUE ~ dep)) %>% 
    dplyr::mutate(var = case_when(
      var == "coral" ~ "Coral",
      var == "algae" ~ "Algae",
      var == "turf" ~ "Turf",
      var == "gravity" ~ "Gravity",
      var == "sst" ~ "Sea Surface Temperature",
      var == "npp" ~ "Net Primary Production",
      var == "dhw" ~ "Degree Heating Weeks",
      var == "S" ~ "Node number",
      var == "C" ~ "Connectance",
      var == "zQn" ~ "Modularity",
      var == "zN" ~ "Nestedness",
      TRUE ~ var)) 

  
  fe$dep <- factor(fe$dep, levels = c("Node number", "Connectance",  "Nestedness", "Modularity", 
                                      "Benthic autotrophs", "Detritus", "Fish", "Mobile invertebrates", "Sessile invertebrates", "Zooplankton",
                                      "Coral", "Algae", "Turf")) 
  fe$var <- factor(fe$var, levels = c("Node number", "Connectance", "Nestedness", "Modularity", 
                                      "Gravity", "Net Primary Production", "Sea Surface Temperature", "Degree Heating Weeks",
                                      "Turf", "Algae", "Coral"))
  
  ##### Nodes ####
  
  node <- rbind(fe %>% select(var) %>% rename(label = var), 
                fe %>% select(dep) %>% rename(label = dep)) %>% 
    group_by(label) %>% summarise() %>%
    mutate(type = case_when(
      label == "Degree Heating Weeks" | label == "Sea Surface Temperature" | label == "Net Primary Production" | label == "Gravity" ~ 1,
      label == "Coral" | label == "Algae" | label == "Turf" ~ 2,
      label == "Node number" | label == "Connectance" | label == "Nestedness" | label == "Modularity" ~ 3,
      label == "Benthic autotrophs" | label == "Detritus" | label == "Fish" | 
        label == "Mobile invertebrates" | label == "Sessile invertebrates" | label == "Zooplankton" ~ 4)) %>%
    arrange(type) %>% 
    mutate(id = seq(1:17)) %>%
    select(id, label)
  
  #Prepare label sizes
  node_labels <- as.vector(node$label)
  node_widths <- nchar(node_labels) * 0.1
  node_heights <- rep(0.5, length(node_labels)) 
  
  ##### Edges ####
  
  edge <- fe %>%
    mutate(influence = case_when(
      Q2.5 > 0 & Q97.5 > 0 ~ TRUE,
      Q2.5 < 0 & Q97.5 < 0 ~ TRUE,
      TRUE ~ FALSE)) %>% 
    filter(influence == TRUE) %>% 
    select(var, dep, Estimate) %>% 
    rename(from = var, to = dep, weight = Estimate)
  
  edge <- left_join(edge, node, by = c("from" = "label"))
  edge <- edge %>% rename(from_label = from, from = id)
  edge <- left_join(edge, node, by = c("to" = "label"))
  edge <- edge %>% rename(to_label = to, to = id)
  
  ##### Graph ####
  graphSEM <- create_graph() %>%
    
    add_nodes_from_table(
      table = node,
      label_col = label) %>%
    
    set_node_attrs(
      node_attr = fontcolor,
      values = "white") %>% 
    set_node_attrs(
      node_attr = width,
      values = node_widths) %>% 
    set_node_attrs(
      node_attr = shape,
      values = "rectangle") %>% 
    
    # node color
    # environmental + human pressures
    set_node_attrs(
      node_attr = fillcolor,
      values = "#CC4411",
      nodes = c(1:4)) %>%
    set_node_attrs(
      node_attr = color,
      values = "#CC4411",
      nodes = c(1:4)) %>%
    set_node_attrs(
      node_attr = rank,
      values = 1,
      nodes = c(2:4)) %>%
    
    # benthos
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#EEBB44",
      nodes = c(5:7)) %>%
    set_node_attrs(
      node_attr = color,
      values =  "#EEBB44",
      nodes = c(5:7)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  2,
      nodes =  c(5:7)) %>%
    
    # architecture
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#74BBCD",
      nodes = c(8:12)) %>%
    set_node_attrs(
      node_attr = color,
      values =  "#74BBCD",
      nodes = c(8:12)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  3,
      nodes =  c(8:12)) %>%
    
    # flux
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#E9695F",
      nodes = c(12:17)) %>% 
    set_node_attrs(
      node_attr = color,
      values =  "#E9695F",
      nodes = c(12:17)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  4,
      nodes =  c(12:17))
  
  # edges color
  graphSEM <- graphSEM %>%
    add_edges_from_table(
      table = edge,
      from_col = from,
      to_col = to,
      from_to_map = id_external) %>%
    
    select_edges(conditions = weight > 0) %>%
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#7CAE00") %>% 
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#7CAE00") %>%
    clear_selection() %>%
    
    select_edges(conditions = weight < 0) %>%
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#E68650") %>% 
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#E68650") %>%
    clear_selection()
  
  # edges width
  graphSEM <- graphSEM %>% 
    set_edge_attrs(
      edge_attr = width,
      value = abs(graphSEM[["edges_df"]]$weight)*8) %>% 
    copy_edge_attrs(
      edge_attr_from = width,
      edge_attr_to = penwidth)
  
  # global attributes
  graphSEM <- graphSEM %>% 
    add_global_graph_attrs(attr = "bgcolor", value = "transparent", attr_type = "graph") %>% 
    add_global_graph_attrs(attr = "width", value = 0.4, attr_type = "node") %>% 
    add_global_graph_attrs(attr = "fontname", value = "Helvetica", attr_type = "node") %>%
    add_global_graph_attrs(attr = "fontsize", value = 10, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"),
                           attr_type = c("graph", "graph", "graph"))
  
  final_graphSEM <- render_graph(graphSEM,
                                 output = NULL,
                                 as_svg = FALSE
  )
  
  export_graph(graphSEM, file_name = "output_figures/Fig4_SEM.pdf", file_type = "pdf",
               width = 515, height = 175) # width in pixel, 696 pi = 7.25 inches
  
  return(final_graphSEM)  
}

# tar_load(fit_z_sem_5000)
# fit_sem <- fit_z_sem_5000

make_zFig4_CE_data <- function(fit_sem){
  
  Fig4_CE_data <- list(  
    cef_grav = conditional_effects(fit_sem, effects = c("gravity")),
    cef_npp = conditional_effects(fit_sem, effects = c("npp")),
    cef_coral = conditional_effects(fit_sem, effects = c("coral")),
    cef_algae = conditional_effects(fit_sem, effects = c("algae")),
    cef_modularity = conditional_effects(fit_sem, effects = c("zQn")),
    cef_nestedness = conditional_effects(fit_sem, effects = c("zN")))

  # cef_connectance = conditional_effects(fit_sem, effects = c("C"))
  
  return(Fig4_CE_data)
}

# tar_load(Fig4_z_ce_data_5000)
# Fig4_CE_data <- Fig4_z_ce_data_5000

make_zFig4_CE <- function(Fig4_CE_data){
  
  theme_custom <- theme_minimal() +
    theme(
      title = element_text(size = 8, family = "Helvetica"),
      axis.title = element_text(size = 7, family = "Helvetica"),
      legend.title = element_text(size = 7, family = "Helvetica"),
      legend.text = element_text(size = 7, family = "Helvetica"),
      axis.text = element_text(size = 7, family = "Helvetica")
    )
  
  ce_fish_grav <- Fig4_CE_data[[1]][["fish.fish_gravity"]]
  (plot_fish_grav <- ggplot(ce_fish_grav, aes(x = gravity, y = estimate__)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey80", alpha = 0.5) +
      geom_line(color = "#E9695F", linewidth = 1) +
      labs(x = "Gravity (log)", y = "Fish (proportion)") +
      theme_custom)
  
  
  ce_S_algae <- Fig4_CE_data[[4]][["S.S_algae"]]
  (plot_S_algae <- ggplot(ce_S_algae, aes(x = algae, y = estimate__)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey80", alpha = 0.5) +
      geom_line(color = "#74BBCD", linewidth = 1) +
      labs(x = "Algae cover (proportion)", y = "Node number (log)") +
      theme_custom)
  
  ce_sInv_algae <- Fig4_CE_data[[4]][["sInv.sInv_algae"]]
  (plot_sInv_algae <- ggplot(ce_S_algae, aes(x = algae, y = estimate__)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey80", alpha = 0.5) +
      geom_line(color = "#E9695F", linewidth = 1) +
      labs(x = "Algae cover (proportion)", y = "Sessile invertebrates (proportion)") +
      theme_custom)
  
  
  ce_mInv_coral <- Fig4_CE_data[[3]][["mInv.mInv_coral"]]
  (plot_mInv_coral <- ggplot(ce_mInv_coral, aes(x = coral, y = estimate__)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey80", alpha = 0.5) +
      geom_line(color = "#E9695F", linewidth = 1) +
      labs(x = "Coral cover (proportion)", y = "Mobile invertebrates (proportion)") +
      theme_custom)
  
  
  ce_C_coral <- Fig4_CE_data[[3]][["C.C_coral"]]
  (plot_C_coral <- ggplot(ce_C_coral, aes(x = coral, y = estimate__)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey80", alpha = 0.5) +
      geom_line(color = "#74BBCD", linewidth = 1) +
      labs(x = "Coral cover (proportion)", y = "Connectance (log)") +
      theme_custom)
  
  
  ce_C_npp <- Fig4_CE_data[[2]][["C.C_npp"]]
  (plot_C_npp <- ggplot(ce_C_npp, aes(x = npp, y = estimate__)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey80", alpha = 0.5) +
      geom_line(color = "#74BBCD", linewidth = 1) +
      labs(x = "Net Primary Production (log)", y = "Connectance (log)") +
      theme_custom)
  
  (CE <- ggarrange(
    plot_S_algae, plot_C_npp, plot_C_coral, plot_fish_grav, plot_mInv_coral, plot_sInv_algae,
    ncol = 2, nrow = 3,
    #labels = c("B", "C", "D", "E", "F", "G"),  # Custom labels
    font.label = list(size = 9, color = "black", face = "bold", family = 'Helvetica')
  ))
  
  ggsave('output_figures/Fig4_CE.pdf', plot = CE,
         unit = "in", width = 7.20, height = 5, dpi = 300)
  
  
  # Return the combined grob object
  return(CE)
  
}

#### Permanova ####

# make_permanova <- function(bga){
#   tar_load(bga)
#   
#   bga <- bga %>%
#     rename(N = NODF2, 
#            Od = G, 
#            Id = V,
#            Region = Realm) %>%    
#     mutate(Region_short = case_when(Region == "Tropical Eastern Pacific" ~ "TEP",
#                                     Region == "Tropical Atlantic" ~ "TA", 
#                                     Region == "Eastern Indo-Pacific" ~ "EIP",
#                                     Region == "Central Indo-Pacific" ~ "CIP",
#                                     Region == "Western Indo-Pacific" ~ "WIP")) %>% 
#     mutate(Region = base::factor(Region, levels = c("Tropical Eastern Pacific",
#                                                     "Tropical Atlantic", 
#                                                     "Eastern Indo-Pacific", 
#                                                     "Central Indo-Pacific", 
#                                                     "Western Indo-Pacific"))) %>% 
#     mutate(Region_short = base::factor(Region_short, levels = c("TEP",
#                                                                 "TA", 
#                                                                 "EIP", 
#                                                                 "CIP", 
#                                                                 "WIP"))) %>% 
#     ungroup()
#   
#   archi <- bga %>% 
#     select(SiteCode, Region, Region_short, S, L, C, Bc, Od, Id, zN, zQn) %>% 
#     mutate(S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
#            L = (log(L+1) - mean(log(L+1))) / sd(log(L+1)),
#            C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
#            Bc = (log(Bc+1) - mean(log(Bc+1))) / sd(log(Bc+1)),
#            Od = (log(Od+1) - mean(log(Od+1))) / sd(log(Od+1)),
#            Id = (log(Id+1) - mean(log(Id+1))) / sd(log(Id+1)),
#            
#            # S = (S - mean(S)) / sd(S),
#            # C = (C - mean(C)) / sd(C),
#            
#            zN = (zN - mean(zN)) / sd(zN),
#            zQn = (zQn - mean(zQn)) / sd(zQn)) %>% 
#     # select(SiteCode, Region, Region_short, S, L, C, Bc, Od, Id, zN, zQn)
#     select(SiteCode, Region, Region_short, S, C, zN, zQn)
#   
#   # Distance Matrix #
#   # perm_dist <- vegdist(archi[,4:11], method = 'gower')
#   perm_dist <- vegdist(archi[,4:7], method = 'gower')
#   
#   # perm_dist <- vegdist(top[,4:10], method = 'bray')
#   
#   # Assumptions #
#   dispersion <- betadisper(perm_dist, group = archi$Region, type = "centroid")
#   
#   # # Extract site scores (one row per sample)
#   # scores_df <- as.data.frame(scores(dispersion, display = "sites"))
#   # 
#   # # Add Region info for plotting
#   # scores_df$Region <- archi$Region
#   # 
#   # # Plot with ggplot2
#   # ggplot(scores_df, aes(x = PCoA1, y = PCoA2, color = Region)) +
#   #   geom_point(size = 3) +
#   #   geom_text_repel(aes(label = Region), size = 3) +
#   #   theme_minimal() +
#   #   labs(title = "Multivariate Dispersion (betadisper)",
#   #        x = "PCoA Axis 1",
#   #        y = "PCoA Axis 2")
#   # 
#   
#   
#   
#   # plot(dispersion, text = F)# labels = TRUE, cex = 0.2)
#   
#   # To assess the dispersion
#   # The pvalue isn't significant => the dispersion is  the same between groups
#   # Donc on peut faire la permanova (c'est une des conditions pour le test)
#   anova(dispersion)
#   
#   # Test #
#   adonis2(perm_dist ~ as.factor(archi$Region), data = perm_dist, permutations=9999)
#   
# }


