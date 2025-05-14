# library(brms)
# library(dplyr)

# tar_load(data_z_sem)
# data_sem <- data_z_sem

make_zSEM_full <- function(data_sem){
  #### SEM ####
  
  ##### benthos | layer 2 ####
  
  coral <- brms::bf(coral ~ 1 + npp + gravity + dhw + sst + (1|Realm), family = Beta())
  algae <- brms::bf(algae ~ 1 + npp + gravity + dhw + sst + (1|Realm), family = Beta()) 
  turf <- brms::bf(turf ~ 1 + npp + gravity + dhw + sst + (1|Realm), family = Beta())
  
  ##### topo | layer 3 ####
  
  s <- brms::bf(S ~ 1 + coral + algae + turf + npp + gravity + dhw + sst + (1|Realm))
  c <- brms::bf(C ~ 1 + coral + algae + turf + npp + gravity + dhw + sst + (1|Realm))
  zn <- brms::bf(zN ~ 1 + coral + algae + turf + npp + gravity + dhw + sst + (1|Realm))
  zq <- brms::bf(zQn ~ 1 + coral + algae + turf + npp + gravity + dhw + sst + (1|Realm))
  
  ##### fluxes | layer 4 ####
  
  bAut <- brms::bf(bAut ~ 1 + coral + algae + turf + npp + gravity + dhw + sst + S + C + zN + zQn + (1|Realm), family = Beta())
  det <- brms::bf(det ~ 1 + coral + algae + turf + npp + gravity + dhw + sst + S + C + zN + zQn + (1|Realm), family = Beta())
  fish <- brms::bf(fish ~ 1 + coral + algae + turf + npp + gravity + dhw + sst + S + C + zN + zQn + (1|Realm), family = Beta())
  mInv <- brms::bf(mInv ~ 1 + coral + algae + turf + npp + gravity + dhw + sst + S + C  + zN + zQn + (1|Realm), family = Beta())
  sInv <- brms::bf(sInv ~ 1 + coral + algae + turf + npp + gravity + dhw + sst + S + C + zN + zQn + (1|Realm), family = Beta())
  zooP <- brms::bf(zooP ~ 1 + coral + algae + turf + npp + gravity + dhw + sst + S + C + zN + zQn + (1|Realm), family = Beta())
  
  semPriors <- get_prior(data = data_sem,
                         bAut + det + fish + mInv + sInv + zooP +
                           s + c + zn + zq +
                           coral + algae + turf) %>%
    mutate(prior = case_when(class == as.character("b") & coef != "" ~ "normal(0, 1)",
                             TRUE ~ prior))
  
  fit_z_sem <- brm(
    bAut + det + fish + mInv + sInv + zooP +
      s + c + zn + zq +
      coral + algae + turf,
    data = data_sem,
    prior = semPriors,
    chains = 4, cores = 4, iter = 2000, warmup = 1000,
    backend = "cmdstanr",
    threads = 12)
  
  return(fit_z_sem)
}


plot_fullSEM <- function(fit_sem){
  
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
    add_global_graph_attrs(attr = "fontsize", value = 9, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"),
                           attr_type = c("graph", "graph", "graph"))
  
  final_graphSEM <- render_graph(graphSEM,
                                 output = NULL,
                                 as_svg = FALSE
  )
  
  
  export_graph(graphSEM, file_name = "output_figures/Supp11_fullSEM.pdf", file_type = "pdf",
               width = 515) # width in pixel, 696 pi = 7.25 inches
  
  return(graphSEM)  
}