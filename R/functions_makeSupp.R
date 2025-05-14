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
# 
# library(rworldmap)
# library(maps)
# 
# library(DiagrammeRsvg)
# library(htmltools)
# library(svglite)
# library(grid)
# library(khroma)


#### Map RLS ####
# tar_load(bga)

makeMap <- function(bga){
  
  world_map <- map_data("world") %>% 
    filter(lat > -50 & lat < 50)
  
  .archi_bga <- bga %>% 
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
                                                                "WIP")))
  (ggplot() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey90") +
      geom_point(data = .archi_bga, aes(x = SiteLongitude, y = SiteLatitude, color = Region_short, alpha = 0.4), size = 2, alpha = 0.8) +
      scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
      scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
      theme(panel.background = element_rect(fill = "lightblue"),
            title = element_text(size = 8, family = "Helvetica"),
            axis.title = element_text(size = 7, family = "Helvetica"),
            legend.title = element_text(size = 7, family = "Helvetica"),
            legend.text = element_text(size = 7, family = "Helvetica"),
            axis.text = element_text(size = 7, family = "Helvetica"),
            legend.position = "top") +
      coord_fixed() +
      labs(x = "Longitude", y = "Latitude", color = "Regions"))
  
    ggsave(file = "output_figures/Supp2_map.pdf", dpi = 300, unit = "in", width = 7.25, height = 3)  #width=10, height=8
  
}


#### Theory Topology ####

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
    add_global_graph_attrs(attr = "bgcolor", value = "transparent", attr_type = "graph") %>% 
    add_global_graph_attrs(attr = "width", value = 0.4, attr_type = "node") %>% 
    add_global_graph_attrs(attr = "fontname", value = "Helvetica", attr_type = "node") %>%
    add_global_graph_attrs(attr = "fontsize", value = 8, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"),
                           attr_type = c("graph", "graph", "graph"))
  
  plot <- render_graph(graph,
                       output = NULL,
                       as_svg = FALSE)
  
  return (graph)
}

makeSuppTN <- function(){
  
  graphFig1 <- list(
    graphA = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", "4", "4", # high S  and low C
                                               "5", "6", "6", "7", "7", "8", "8", "9"),
                                             ncol = 2)),
                  n = 4,
                  m = 5),
    
    graphB = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", # low S and high C 
                                               "4", "5", "5", "4", "5", "4"),
                                             ncol = 2)),
                  n = 3,
                  m = 2),
    
    graphC = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "2", "3", "3", "3", "4", "4", #regular 
                                               "5", "6", "5", "6", "7", "6", "7", "8", "7", "8"),
                                             ncol = 2)),
                  n = 4,
                  m = 4),
    
    graphD = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", "4", "4", # modular
                                               "5", "6", "5", "6", "7", "8", "7", "8"),
                                             ncol = 2)),
                  n <- 4,
                  m <- 4),
    
    graphE = list(biP = as.data.frame(matrix(c("1", "1", "1", "1", "2", "2", "2", "3", "3", "4", # nested
                                               "5", "6", "7", "8", "6", "7", "8", "7", "8", "8"),
                                             ncol = 2)),
                  n = 4,
                  m = 4))
  
  graphA <- makeGraph(graphFig1[[1]])
  export_graph(graphA, height = 75, file_name = "output_figures/Supp3/graphA.pdf", file_type = "pdf")
  
  graphB <- makeGraph(graphFig1[[2]])
  export_graph(graphB, height = 75, file_name = "output_figures/Supp3/graphB.pdf", file_type = "pdf")
  
  graphC <- makeGraph(graphFig1[[3]])
  export_graph(graphC, height = 75, file_name = "output_figures/Supp3/graphC.pdf", file_type = "pdf")
  
  graphD <- makeGraph(graphFig1[[4]])
  export_graph(graphD, height = 75, file_name = "output_figures/Supp3/graphD.pdf", file_type = "pdf")
  
  graphE <- makeGraph(graphFig1[[5]])
  export_graph(graphE, height = 75, file_name = "output_figures/Supp3/graphE.pdf", file_type = "pdf")
  
  return()
}

#### Dags ####
# See function_makeSupp_DAGS.R

#### ppChecks & scattAvg ####

# tar_load(fit_z_sem_5000)
# fit_z_sem <- fit_z_sem_5000

plot_ppchecks_z <- function(fit_z_sem){
  
  resp <- c("coral", "turf", "algae",
            "S", "C", "zN", "zQn",
            "bAut", "det", "zooP", "sInv", "mInv", "fish" )
  legend <- c("Coral", "Turf", "Algae",
              "Node number", "Connectance", "Nestedness", "Modularity",
              "Benthic autotrophs", "Detritus", "Zooplankton", "Sessile invertebrates", "Mobile invertebrates", "Fish")
  
  # dens_overlay
  i <- 1
  
  for (i in 1:length(resp)){
    (ppCheck_plot <- pp_check(fit_z_sem, resp = as.character(resp[i]), type = "dens_overlay") +
       labs(x = as.character(legend[i]), y = "Density", title = paste("Posterior predictive distribution of", as.character(legend[i]))) +
       theme(
         text = element_text(family = "Helvetica", size = 6),
         axis.title = element_text(size = 6),
         axis.text = element_text(size = 6),
         plot.title = element_text(size = 6),
         legend.title = element_text(size = 6),
         legend.text = element_text(size = 6),
         legend.key.size = unit(0.4, "cm")
       )
    )
    
    ggsave(ppCheck_plot, filename = paste("output_figures/Supp5_ppChecks/ppCheck_DensOverlay_", as.character(resp[i]),".pdf", sep=""),
           dpi = 300, unit = "in", width = 5, height = 4)
    
  }
  
  # scatter average
  for (i in 1:length(resp)){
    (ppCheck_plot <- pp_check(fit_z_sem, resp = as.character(resp[i]), type = "scatter_avg") +
       labs(title = paste("Scatterplot between", as.character(legend[i]), "data and the average value of \n the posterior predictive distribution of each data point")) +
       geom_point(size = 0.5) +
       theme(
         text = element_text(family = "Helvetica", size = 6),
         axis.title = element_text(size = 6),
         axis.text = element_text(size = 6),
         plot.title = element_text(size = 6),
         legend.title = element_text(size = 6),
         legend.text = element_text(size = 6),
         legend.key.size = unit(0.4, "cm")
       ) 
    )
    
    ggsave(ppCheck_plot, filename = paste("output_figures/Supp6_scattAvg/Supp6_scattAvg_", as.character(resp[i]),".pdf", sep=""),
           dpi = 300, unit = "in", width = 5, height = 4)
  }
}

#### PCA 10% cat ####
# tar_load(bga)

makePCA_CAT <- function(bga){
  
  archi_bga <- bga %>% 
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
    ungroup()
  
  data <- archi_bga %>% 
    mutate(sum_benthos = coral + algae + turf) %>%
    filter(sum_benthos <= 100) %>% 
    mutate(coral_prop = (coral/sum_benthos)*100,
           algae_prop = (algae/sum_benthos)*100,
           turf_prop = (turf/sum_benthos)*100) %>%
    mutate(coral_max = sum_benthos - coral,
           algae_max = sum_benthos - algae,
           turf_max = sum_benthos - turf) %>%
    select(SiteCode, Region,
           coral:turf, 
           coral_prop:turf_prop,
           sum_benthos,
           coral_max:turf_max,
           S, C, zN, zQn) %>% 
    rename(N = zN, Qn = zQn)
  
  .top_coral <- data %>%
    arrange(coral_prop) %>%
    top_n(28, coral_prop) %>%
    mutate(tiptop = "coral")
  .top_algae <- data %>%
    arrange(algae_prop) %>%
    top_n(28, algae_prop) %>%
    mutate(tiptop = "algae")
  .top_turf <- data %>%
    arrange(turf_prop) %>%
    top_n(28, turf_prop) %>%
    mutate(tiptop = "turf")
  
  top <- rbind(.top_coral, .top_algae, .top_turf) %>%
    select(SiteCode, Region, tiptop, S, C, N, Qn) %>% 
    mutate(S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
           C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
           N = (N - mean(N)) / sd(N),
           Qn = (Qn - mean(Qn)) / sd(Qn)) %>% 
    select(SiteCode, Region, tiptop, S, C, N, Qn)
  
  pca_res <- prcomp(top[4:7], scale. = TRUE)
  
  (p <- autoplot(pca_res, data = top, colour = 'tiptop', fill = 'tiptop',
                 loadings = TRUE, loadings.colour = 'grey15',
                 loadings.label = TRUE, loadings.label.colour = "black", loadings.label.size = 4, loadings.label.repel = T) +
      scale_color_manual(values = c("forestgreen", "coral2", "cadetblue3")) +
      scale_fill_manual(values = c("forestgreen", "coral2", "cadetblue3")) +
      guides(fill = guide_legend(title = "Dominant cover"), colour = guide_legend(title = "Dominant cover")) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = "white"),
            title = element_text(size = 8, family = "Helvetica"),
            axis.title = element_text(size = 7, family = "Helvetica"),
            legend.title = element_text(size = 7, family = "Helvetica"),
            legend.text = element_text(size = 7, family = "Helvetica"),
            axis.text = element_text(size = 7, family = "Helvetica"),
            legend.position = "right"))
  
  ggsave(file = "output_figures/Supp7_PCA_Cat.pdf", dpi = 300, unit = "in", width = 7.25, height = 6)  #width=10, height=8
  
  #### PERMANOVA ###
  
  # # Distance Matrix #
  # top <- data %>% select(SiteCode, Region, S, C, N, Qn) %>% 
  #   mutate(S = ((S-42)/(161-42))*100,
  #          C = ((C-0.2018)/(0.6184-0.2018))*100,
  #          N = ((N+7.3829)/(2.0799+7.3829))*100,
  #          Qn = ((Qn+1.1995)/(18.5417+1.1995))*100) %>% 
  #   mutate(across(N:Qn, ~ case_when(. > 100 ~ 100,
  #                                   . < 0 ~ 0,
  #                                   T ~ .)))
  # 
  # 
  # perm_dist <- vegdist(top[,3:6], method = 'bray')
  # # perm_dist <- vegdist(top[,4:10], method = 'bray')
  # 
  # # Assumptions #
  # dispersion <- betadisper(perm_dist, group = top$Region, type = "centroid")
  # plot(dispersion)
  # 
  # # To assess the dispersion
  # # The pvalue isn't significant => the dispersion is  the same between groups
  # # Donc on peut faire la permanova (c'est une des conditions pour le test)
  # anova(dispersion)
  # 
  # # Test #
  # adonis2(perm_dist ~ as.factor(top$Region), data = perm_dist, permutations=9999)
  
  return(p)
}

#### KernellPlot ####

# tar_load(flux_per_prey_Ic)

makeKernell <- function(flux_per_prey_Ic){
  
  flux <- flux_per_prey_Ic %>%
    select(site, bAut_prop:zooP_prop) %>%
    rename("bAut" = "bAut_prop",
           "det" = "det_prop",
           "fish" = "fish_prop",
           "mInv" = "mInv_prop",
           "sInv" = "sInv_prop",
           "zooP" = "zooP_prop",
           "SiteCode" = "site")
  
  longer_flux <- as.data.frame(flux %>% 
                                 pivot_longer(cols = bAut:zooP, names_to = "flux_cat", values_to = "flux_prop") %>% 
                                 mutate(flux_cat = as.factor(flux_cat)) %>% 
                                 select(flux_cat, flux_prop))
  
  ker <- ggplot(longer_flux) + 
    geom_density(aes(x = flux_prop, fill = flux_cat, color = flux_cat), alpha = 0.4) +
    scale_color_manual(values = c("#00BA38", "#B38683", "#619CFF","#00BFC4", "#F8766D", "#F564E3")) +
    scale_fill_manual(values = c("#00BA38", "#B38683", "#619CFF","#00BFC4", "#F8766D", "#F564E3")) +
    labs(title = "", x = "Proportion of Carbon flowing through major pathways", y = "Density", color = "Carbon pathways", fill = "Carbon pathways") +
    theme_minimal() +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, 'cm'),
          title = element_text(size = 8, family = "Helvetica"),
          plot.title = element_text(size=7, family = "Helvetica"),
          axis.title = element_text(size = 7, family = "Helvetica"),
          legend.title = element_text(size = 7, family = "Helvetica"),
          legend.text = element_text(size = 7, family = "Helvetica"),
          axis.text = element_text(size = 7, family = "Helvetica"),
          legend.position = "right")
  
  ggsave(file = "output_figures/Supp8_kerDensity_z.pdf", dpi = 300, unit = "in", width = 7.25, height = 6)
}

#### PCA All ####

# tar_load(bga)

makePCA_All <- function(bga){
  
  .archi_bga <- bga %>% 
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
    ungroup()
  
  .archi <- .archi_bga %>% 
    select(SiteCode, Region, Region_short, S, L, C, Bc, Od, Id, zN, zQn) %>% 
    mutate(S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
           L = (log(L+1) - mean(log(L+1))) / sd(log(L+1)),
           C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
           Bc = (log(Bc+1) - mean(log(Bc+1))) / sd(log(Bc+1)),
           Od = (log(Od+1) - mean(log(Od+1))) / sd(log(Od+1)),
           Id = (log(Id+1) - mean(log(Id+1))) / sd(log(Id+1)),
           # N = (log(N+1) - mean(log(N))) / sd(log(N+1)),
           # Qn = (log(Qn+1) - mean(log(Qn+1))) / sd(log(Qn+1)),
           zN = (zN - mean(zN)) / sd(zN),
           zQn = (zQn - mean(zQn)) / sd(zQn)) %>% 
    rename(N = zN, Qn = zQn)
  
  pca_res <- prcomp(.archi[4:11], scale. = TRUE) 
  (pca_all_z <- autoplot(pca_res, data = .archi, size = 2, colour = 'Region', fill = 'Region', alpha = 0.7, 
                         loadings = TRUE, loadings.colour = 'grey15', 
                         loadings.label = TRUE, loadings.label.colour = "black", loadings.label.size = 4, loadings.label.repel = T) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = "white"),
            title = element_text(size = 8, family = "Helvetica"),
            axis.title = element_text(size = 7, family = "Helvetica"),
            legend.title = element_text(size = 7, family = "Helvetica"),
            legend.text = element_text(size = 7, family = "Helvetica"),
            axis.text = element_text(size = 7, family = "Helvetica"),
            legend.position = "right") +
      scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")))
  
  
  ggsave(file = "output_figures/Supp9_PCA_All.pdf", dpi = 300, unit = "in", width = 7.25, height = 6)  #width=10, height=8
  
  return(pca_all_z)
}


#### CorrPlot ####

# tar_load(bga)

makeCorrplot <- function(bga){
  
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
    ungroup()
  
  archi <- bga %>% 
    select(SiteCode, Region, Region_short, S, L, C, Bc, Od, Id, zN, zQn) %>% 
    mutate(S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
           # L = (log(L+1) - mean(log(L+1))) / sd(log(L+1)),
           C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
           # Bc = (log(Bc+1) - mean(log(Bc+1))) / sd(log(Bc+1)),
           # Od = (log(Od+1) - mean(log(Od+1))) / sd(log(Od+1)),
           # Id = (log(Id+1) - mean(log(Id+1))) / sd(log(Id+1)),
           zN = (zN - mean(zN)) / sd(zN),
           zQn = (zQn - mean(zQn)) / sd(zQn)) %>% 
    rename(N = zN, Qn = zQn)
  
  mcor_archi = cor(archi[4:11])
  p.mat_archi <- cor_pmat(mcor_archi)
  
  archi_corrplot <- ggcorrplot(mcor_archi, 
                               hc.order = F,
                               outline.col = "white", 
                               type = "lower",
                               method = "circle",
                               lab = T, lab_size = 3,
                               p.mat = p.mat_archi,
                               insig = "blank",
                               ggtheme = ggplot2::theme_minimal,
                               colors = c("#619CFF", "white", "#F8766D"),
                               legend.title = "Correlation") +
    theme(plot.background = element_rect(fill = "white", color = "white"),
          title = element_text(size = 8, family = "Helvetica"),
          axis.title = element_text(size = 7, family = "Helvetica"),
          legend.title = element_text(size = 7, family = "Helvetica"),
          legend.text = element_text(size = 7, family = "Helvetica"),
          axis.text = element_text(size = 7, family = "Helvetica"),
          legend.position = "right")
  
  ggsave(file = "output_figures/Supp10_Corr.pdf", dpi = 1200, unit = "in", width = 7.25, height = 6)  #width=10, height=8
  
  return(archi_corrplot)
}
