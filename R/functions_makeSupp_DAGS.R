make_DAGS <- function(){
  #### benthos_layer ####
  
  ##### node ####
  
  node <- as.data.frame(
    rbind("Coral", "Algae", "Turf",
          "Gravity", "Net Primary Production", "Sea Surface Temperature", "Degree Heating Weeks")) %>% 
    rename(label = V1)
  
  node <- node %>% 
    mutate(label = factor(node$label, 
                          levels = c("Coral", "Algae", "Turf",
                                     "Gravity", "Net Primary Production", "Sea Surface Temperature", "Degree Heating Weeks"))) %>% 
    mutate(type = case_when(
      label == "Degree Heating Weeks" | label == "Sea Surface Temperature" | label == "Net Primary Production" | label == "Gravity" ~ 1,
      label == "Coral" | label == "Algae" | label == "Turf" ~ 2)) %>%
    arrange(type) %>% 
    mutate(id = seq(1:7)) %>%
    select(id, label)
  
  ##### edge ####
  
  .benthos <- cbind(
    c(rep("Coral", 3), rep("Algae", 3), rep("Turf", 3)),
    c(rep(c("Net Primary Production", "Gravity", "Degree Heating Weeks"), 3)),
    rep(1, 9))
  
  edge <- as.data.frame(rbind(.benthos)) %>% 
    rename(var = V1, dep = V2, weight = V3)
  
  edge <- edge %>% 
    mutate(var = factor(edge$var, 
                        levels = c("Coral", "Algae", "Turf"))) %>% 
    mutate(dep = factor(edge$dep, 
                        levels = c("Gravity", "Net Primary Production", "Sea Surface Temperature", "Degree Heating Weeks"))) %>% 
    mutate(weight = as.numeric(weight)) %>% 
    rename(from = dep, to = var)
  
  edge <- left_join(edge, node, by = c("from" = "label"))
  edge <- edge %>% rename(from_label = from, from = id)
  edge <- left_join(edge, node, by = c("to" = "label"))
  edge <- edge %>% rename(to_label = to, to = id)
  
  edge <- edge %>% 
    mutate(to_benthos = case_when(
      from_label != "Sea Surface Temperature" & (
        to_label == "Coral" | to_label == "Algae" | to_label == "Turf") ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(magouille = case_when(
      from_label == "Sea Surface Temperature" & (
        to_label == "Coral" | to_label == "Algae" | to_label == "Turf") ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(from_benthos_bga = case_when(
      from_label == "Coral" | from_label == "Algae" | from_label == "Turf" | 
        from_label == "Sea Surface Temperature"| from_label == "Net Primary Production"| from_label == "Gravity"| from_label == "Degree Heating Weeks" ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(from_benthos = case_when(
      from_label == "Coral" | from_label == "Algae" | from_label == "Turf" ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(from_bga = case_when(
      from_label == "Degree Heating Weeks" | from_label == "Sea Surface Temperature"| from_label == "Net Primary Production"| from_label == "Gravity" ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(to_archi = case_when(
      to_label == "Node number" | to_label == "Connectance"| to_label == "Nestedness"| to_label == "Modularity" ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(from_archi = case_when(
      from_label == "Node number" | from_label == "Connectance"| from_label == "Nestedness"| from_label == "Modularity" ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(to_flux = case_when(
      to_label == "Benthic autotrophs" | to_label == "Fish" | to_label == "Detritus" | to_label == "Zooplankton" |
        to_label == "Sessile invertebrates" | to_label == "Mobile invertebrates" ~ TRUE,
      TRUE ~ FALSE))
  
  #Prepare label sizes
  node_labels <- as.vector(node$label)
  node_widths <- nchar(node_labels) * 0.1
  node_heights <- rep(0.5, length(node_labels)) 
  
  ##### graph ####
  
  graph1 <- create_graph() %>%
    
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
    
    # ENV + HP
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
      nodes = c(1:4)) %>%
    
    # BENTHOS
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
      nodes =  c(5:7))
  
  graph2 <- graph1 %>%
    add_edges_from_table(
      table = edge,
      from_col = from,
      to_col = to,
      from_to_map = id_external) %>%
    
    select_edges(conditions = from_bga == T) %>% 
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#CC4411") %>%
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#CC4411") %>%
    clear_selection()
  
  graph3 <- graph2 %>% 
    set_edge_attrs( 
      edge_attr = width,
      value = abs(graph2[["edges_df"]]$weight)*1) %>% 
    copy_edge_attrs(
      edge_attr_from = width,
      edge_attr_to = penwidth)
  
  graphDAG <- graph3 %>% 
    add_global_graph_attrs(attr = "bgcolor", value = "transparent", attr_type = "graph") %>% 
    add_global_graph_attrs(attr = "width", value = 0.5, attr_type = "node") %>% 
    add_global_graph_attrs(attr = "fontname", value = "Helvetica", attr_type = "node") %>%
    add_global_graph_attrs(attr = "fontsize", value = 8, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"), 
                           attr_type = c("graph", "graph", "graph"))
  
  render_graph(graphDAG)
  
  final_graphDAG <- render_graph(graphDAG,
                                 output = NULL,
                                 as_svg = FALSE
  )
  
  # export_graph(graphDAG, height = 1000, file_name = "PAPER_FIGS/script_output_figs/Supp/Supp5_graphDAG_benthos.png", file_type = "png") # ou svg
  export_graph(graphDAG, file_name = "output_figures/Supp4/Supp4_DAG1.pdf", file_type = "pdf",
               width = 250) # width in pixel, 696 pi = 7.25 inches
  
  #### Topo_layer ####
  
  
  ##### node ####
  
  node <- as.data.frame(
    rbind("Node number", "Connectance", "Nestedness", "Modularity", 
          "Coral", "Algae", "Turf",
          "Gravity", "Net Primary Production", "Sea Surface Temperature", "Degree Heating Weeks")) %>% 
    rename(label = V1)
  
  node <- node %>% 
    mutate(label = factor(node$label, 
                          levels = c("Node number", "Connectance", "Nestedness", "Modularity", 
                                     "Coral", "Algae", "Turf",
                                     "Gravity", "Net Primary Production", "Sea Surface Temperature", "Degree Heating Weeks"))) %>% 
    mutate(type = case_when(
      label == "Degree Heating Weeks" | label == "Sea Surface Temperature" | label == "Net Primary Production" | label == "Gravity" ~ 1,
      label == "Coral" | label == "Algae" | label == "Turf" ~ 2,
      label == "Node number" | label == "Connectance" | label == "Nestedness" | label == "Modularity" ~ 3)) %>% 
    arrange(type) %>% 
    mutate(id = seq(1:11)) %>%
    select(id, label)
  
  #Prepare label sizes
  node_labels <- as.vector(node$label)
  node_widths <- nchar(node_labels) * 0.1
  node_heights <- rep(0.5, length(node_labels)) 
  
  
  ##### edge ####
  
  .benthos <- cbind(
    c(rep("Coral", 3), rep("Algae", 3), rep("Turf", 3)),
    c(rep(c("Net Primary Production", "Gravity", "Degree Heating Weeks"), 3)),
    rep(1, 9))
  
  .archi <- cbind(
    c(rep("Node number", 7), rep("Connectance", 7), rep("Nestedness", 7), rep("Modularity", 7)),
    c(rep(c("Net Primary Production", "Gravity", "Sea Surface Temperature", "Degree Heating Weeks", "Coral", "Algae", "Turf"), 4)),
    rep(1, 28))
  
  edge <- as.data.frame(rbind(.benthos, .archi)) %>% 
    rename(var = V1, dep = V2, weight = V3)
  
  edge <- edge %>% 
    mutate(var = factor(edge$var, 
                        levels = c("Node number", "Connectance", "Nestedness", "Modularity", 
                                   "Coral", "Algae", "Turf"))) %>% 
    mutate(dep = factor(edge$dep, 
                        levels = c("Coral", "Algae", "Turf",
                                   "Gravity", "Net Primary Production", "Sea Surface Temperature", "Degree Heating Weeks"))) %>% 
    mutate(weight = as.numeric(weight)) %>% 
    rename(from = dep, to = var)
  
  
  edge <- left_join(edge, node, by = c("from" = "label"))
  edge <- edge %>% rename(from_label = from, from = id)
  edge <- left_join(edge, node, by = c("to" = "label"))
  edge <- edge %>% rename(to_label = to, to = id)
  
  {
    edge <- edge %>% 
      mutate(to_benthos = case_when(
        from_label != "Sea Surface Temperature" & (
          to_label == "Coral" | to_label == "Algae" | to_label == "Turf") ~ TRUE,
        TRUE ~ FALSE))
    
    edge <- edge %>% 
      mutate(from_benthos = case_when(
        from_label == "Coral" | from_label == "Algae" | from_label == "Turf" ~ TRUE,
        TRUE ~ FALSE))
    
    edge <- edge %>% 
      mutate(from_bga_to_archi = case_when(
        (to_label == "Node number" | to_label == "Connectance"| to_label == "Nestedness"| to_label == "Modularity") 
        & 
          (from_label == "Degree Heating Weeks" | from_label == "Sea Surface Temperature"| from_label == "Net Primary Production"| from_label == "Gravity") ~ TRUE,
        TRUE ~ FALSE))
  }
  
  ##### graph ####
  
  graph1 <- create_graph() %>%
    
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
    
    # ENV + HP
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
      nodes = c(1:4)) %>%
    
    # BENTHOS
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
    
    # ARCHI S C N Qn
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#74BBCD",
      nodes = c(8:11)) %>%
    set_node_attrs(
      node_attr = color,
      values =  "#74BBCD",
      nodes = c(8:11)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  3,
      nodes =  c(8:11))
  
  graph2 <- graph1 %>%
    add_edges_from_table(
      table = edge,
      from_col = from,
      to_col = to,
      from_to_map = id_external) %>%
    
    select_edges(conditions = from_bga_to_archi == T) %>% 
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#CC4411") %>%
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#CC4411") %>%
    clear_selection() %>% 
    
    select_edges(conditions = to_benthos == T) %>% 
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#CC441100") %>%
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#CC441100") %>%
    clear_selection() %>% 
    
    select_edges(conditions = from_benthos == T) %>%
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#EEBB44") %>% 
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#EEBB44") %>%
    clear_selection()
  
  graph3 <- graph2 %>% 
    set_edge_attrs( 
      edge_attr = width,
      value = abs(graph2[["edges_df"]]$weight)*1) %>% 
    copy_edge_attrs(
      edge_attr_from = width,
      edge_attr_to = penwidth)
  
  graphDAG <- graph3 %>% 
    add_global_graph_attrs(attr = "bgcolor", value = "transparent", attr_type = "graph") %>% 
    add_global_graph_attrs(attr = "width", value = 0.5, attr_type = "node") %>% 
    add_global_graph_attrs(attr = "fontname", value = "Helvetica", attr_type = "node") %>%
    add_global_graph_attrs(attr = "fontsize", value = 8, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"), 
                           attr_type = c("graph", "graph", "graph"))
  
  render_graph(graphDAG)
  
  final_graphDAG <- render_graph(graphDAG,
                                 output = NULL,
                                 as_svg = FALSE
  )
  
  
  # export_graph(graphDAG, height = 1000, file_name = "PAPER_FIGS/script_output_figs/Supp/Supp5_graphDAG_topo.png", file_type = "png") # ou svg
  export_graph(graphDAG, file_name = "output_figures/Supp4/Supp4_DAG2.pdf", file_type = "pdf",
               width = 250) # width in pixel, 696 pi = 7.25 inches
  
  #### Flux_layer ####
  
  ##### node ####
  
  node <- as.data.frame(
    rbind("Benthic autotrophs", "Detritus", "Fish", "Mobile invertebrates", "Sessile invertebrates", "Zooplankton",
          "Node number", "Connectance", "Nestedness", "Modularity", 
          "Coral", "Algae", "Turf",
          "Gravity", "Net Primary Production")) %>% #"Degree Heating Weeks", "Sea Surface Temperature")) %>% 
    rename(label = V1)
  
  node <- node %>% 
    mutate(label = factor(node$label, 
                          levels = c("Node number", "Connectance", "Nestedness", "Modularity", 
                                     "Benthic autotrophs", "Detritus", "Fish", "Mobile invertebrates", "Sessile invertebrates", "Zooplankton",
                                     "Coral", "Algae", "Turf",
                                     "Gravity", "Net Primary Production"))) %>% #"Degree Heating Weeks", "Sea Surface Temperature", 
    mutate(type = case_when(
      label == "Net Primary Production" |  label == "Gravity" ~ 1, # label == "Degree Heating Weeks" | label == "Sea Surface Temperature" | 
      label == "Coral" | label == "Algae" | label == "Turf" ~ 2,
      label == "Node number" | label == "Connectance" | label == "Nestedness" | label == "Modularity" ~ 3,
      label == "Benthic autotrophs" | label == "Detritus" | label == "Fish" | label == "Mobile invertebrates" | label == "Sessile invertebrates" | label == "Zooplankton" ~ 4)) %>%
    arrange(type) %>% 
    mutate(id = seq(1:15)) %>%
    select(id, label)
  
  #Prepare label sizes
  node_labels <- as.vector(node$label)
  node_widths <- nchar(node_labels) * 0.1
  node_heights <- rep(0.5, length(node_labels)) 
  
  ##### edge ####
  
  {
    .benthos <- cbind(
      c(rep("Coral", 2), rep("Algae", 2), rep("Turf", 2)),
      c(rep(c("Net Primary Production", "Gravity"), 3)), #, "Degree Heating Weeks"
      rep(1, 6))
    
    .archi <- cbind(
      c(rep("Node number", 5), rep("Connectance", 5), rep("Nestedness", 5), rep("Modularity", 5)),
      c(rep(c("Net Primary Production", "Gravity", "Coral", "Algae", "Turf"), 4)),
      rep(1, 20))
    
    .flux <- rbind(
      .bAut <- cbind(
        rep("Benthic autotrophs", 8),
        c("Algae", "Turf", "Net Primary Production", "Gravity", "Node number", "Connectance", "Nestedness", "Modularity"),
        rep(1, 8)),
      
      .det <- cbind(
        rep("Detritus", 8),
        c("Algae", "Turf", "Net Primary Production",  "Gravity", "Node number", "Connectance", "Nestedness", "Modularity"),
        rep(1, 8)),
      
      .fish <- cbind(
        rep("Fish", 6),
        c("Coral", "Gravity", "Node number", "Connectance", "Nestedness", "Modularity"),
        rep(1, 6)),
      
      .mInv <- cbind(
        rep("Mobile invertebrates", 8),
        c("Coral", "Algae", "Turf", "Gravity", "Node number", "Connectance", "Nestedness", "Modularity"),
        rep(1, 8)),
      
      .sInv <- cbind(
        rep("Sessile invertebrates", 8),
        c("Coral", "Algae", "Turf", "Gravity", "Node number", "Connectance", "Nestedness", "Modularity"),
        rep(1, 8)),
      
      .zooP <- cbind(
        rep("Zooplankton", 7),
        c("Coral", "Net Primary Production", "Gravity", "Node number", "Connectance", "Nestedness", "Modularity"),
        rep(1, 7))
    )
    
    edge <- as.data.frame(rbind(.benthos, .archi, .flux)) %>% 
      rename(var = V1, dep = V2, weight = V3)
    
    edge <- edge %>% 
      mutate(var = factor(edge$var, 
                          levels = c("Node number", "Connectance", "Nestedness", "Modularity", 
                                     "Benthic autotrophs", "Detritus", "Fish", "Mobile invertebrates", "Sessile invertebrates", "Zooplankton",
                                     "Coral", "Algae", "Turf"))) %>% 
      mutate(dep = factor(edge$dep, 
                          levels = c("Node number", "Connectance", "Nestedness", "Modularity", 
                                     "Coral", "Algae", "Turf",
                                     "Gravity",  "Net Primary Production"))) %>% #"Degree Heating Weeks", "Sea Surface Temperature", 
      mutate(weight = as.numeric(weight)) %>% 
      rename(from = dep, to = var)
    
    
    edge <- left_join(edge, node, by = c("from" = "label"))
    edge <- edge %>% rename(from_label = from, from = id)
    edge <- left_join(edge, node, by = c("to" = "label"))
    edge <- edge %>% rename(to_label = to, to = id)
  }
  
  
  {
    edge <- edge %>% 
      mutate(from_bga_to_flux = case_when(
        (to_label == "Benthic autotrophs" | to_label == "Fish" | to_label == "Detritus" | to_label == "Zooplankton" |
           to_label == "Sessile invertebrates" | to_label == "Mobile invertebrates")
        &
          (from_label == "Net Primary Production"| from_label == "Gravity") ~ TRUE, #from_label == "Degree Heating Weeks" | from_label == "Sea Surface Temperature"|  
        TRUE ~ FALSE))
    
    edge <- edge %>% 
      mutate(from_benthos_to_flux = case_when(
        (to_label == "Benthic autotrophs" | to_label == "Fish" | to_label == "Detritus" | to_label == "Zooplankton" |
           to_label == "Sessile invertebrates" | to_label == "Mobile invertebrates")
        &
          (from_label == "Coral" | from_label == "Algae" | from_label == "Turf") ~ TRUE,
        TRUE ~ FALSE))
    
    edge <- edge %>% 
      mutate(from_topo_to_flux = case_when(
        (to_label == "Benthic autotrophs" | to_label == "Fish" | to_label == "Detritus" | to_label == "Zooplankton" |
           to_label == "Sessile invertebrates" | to_label == "Mobile invertebrates")
        &
          (from_label == "Node number" | from_label == "Connectance"| from_label == "Nestedness"| from_label == "Modularity") ~ TRUE,
        TRUE ~ FALSE))
    
    edge <- edge %>% 
      mutate(to_else = case_when(
        (to_label == "Coral" | to_label == "Algae" | to_label == "Turf"|
           to_label == "Node number" | to_label == "Connectance"| to_label == "Nestedness"| to_label == "Modularity") ~ TRUE,
        TRUE ~ FALSE))
  }
  ##### graph ####
  
  graph1 <- create_graph() %>%
    
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
    
    # ENV + HP
    set_node_attrs(
      node_attr = fillcolor,
      values = "#CC4411", 
      nodes = c(1:2)) %>%
    set_node_attrs(
      node_attr = color,
      values = "#CC4411",
      nodes = c(1:2)) %>%
    set_node_attrs(
      node_attr = rank,
      values = 1,
      nodes = c(1:2)) %>%
    
    # BENTHOS
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#EEBB44",
      nodes = c(3:5)) %>%
    set_node_attrs(
      node_attr = color,
      values =  "#EEBB44",
      nodes = c(3:5)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  2,
      nodes =  c(3:5)) %>%
    
    # ARCHI S C N Qn
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#74BBCD",
      nodes = c(6:9)) %>%
    set_node_attrs(
      node_attr = color,
      values =  "#74BBCD",
      nodes = c(6:9)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  3,
      nodes =  c(6:9)) %>%
    
    #FLUX
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#E9695F",
      nodes = c(10:15)) %>%
    set_node_attrs(
      node_attr = color,
      values =  "#E9695F",
      nodes = c(10:15)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  4,
      nodes =  c(10:15))
  
  graph2 <- graph1 %>%
    add_edges_from_table(
      table = edge,
      from_col = from,
      to_col = to,
      from_to_map = id_external) %>%
    
    select_edges(conditions = from_bga_to_flux == T) %>% 
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#CC4411") %>%
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#CC4411") %>%
    clear_selection() %>% 
    
    select_edges(conditions = from_benthos_to_flux == T) %>%
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#EEBB44") %>% 
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#EEBB44") %>%
    clear_selection() %>%
    
    select_edges(conditions = from_topo_to_flux == T) %>%
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#74BBCD") %>%
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#74BBCD") %>%
    clear_selection() %>% 
    
    select_edges(conditions = to_else == T) %>% 
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#CC441100") %>%
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#CC441100") %>%
    clear_selection()
  
  graph3 <- graph2 %>% 
    set_edge_attrs( 
      edge_attr = width,
      value = abs(graph2[["edges_df"]]$weight)*1) %>% 
    copy_edge_attrs(
      edge_attr_from = width,
      edge_attr_to = penwidth)
  
  graphDAG <- graph3 %>% 
    add_global_graph_attrs(attr = "bgcolor", value = "transparent", attr_type = "graph") %>% 
    add_global_graph_attrs(attr = "width", value = 0.5, attr_type = "node") %>% 
    add_global_graph_attrs(attr = "fontname", value = "Helvetica", attr_type = "node") %>%
    add_global_graph_attrs(attr = "fontsize", value = 8, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"), 
                           attr_type = c("graph", "graph", "graph"))
  
  render_graph(graphDAG)
  
  final_graphDAG <- render_graph(graphDAG,
                                 output = NULL,
                                 as_svg = FALSE
  )
  
  
  # export_graph(graphDAG, height = 1000, file_name = "PAPER_FIGS/script_output_figs/Supp/Supp5_graphDAG_flux.png", file_type = "png") # ou svg
  export_graph(graphDAG, file_name = "output_figures/Supp4/Supp4_DAG3.pdf", file_type = "pdf",
               width = 512) # width in pixel, 696 pi = 7.25 inches
  
}