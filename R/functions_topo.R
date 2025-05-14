#### READ MATRICES ####

# get_mat_int <- function(path){
#   mat_int <- load(path) 
#   mat_int <- get(mat_int)
#   #mat_int <- as.data.frame(mat_int[[1]])
#   return(mat_int)
# }

read_topology <- function(path_to_topology){
  
  topology <- read_csv(path_to_topology)
  
  return(topology)
}

#### TOPO FUNCTIONS ####
network_complexity <- function(tibble){
  #number of nodes
  S <- round(nrow(tibble) + ncol(tibble) - 1, 4)      
  S_prey <- round(ncol(tibble) - 1, 4)
  S_pred <- round(nrow(tibble), 4)
  
  #number of links
  L <- 0
  for (j in 1:ncol(tibble)){
    for (i in 1:nrow(tibble)){
      if (is.na(tibble[i,j]) == F){
        L <- L+1
      }
    }
  }
  L <- round(L, 4)
  
  #link density
  D <- round(L/S, 4)
  
  #connectance
  C <- round(L/(nrow(tibble)*(ncol(tibble)-1)), 4)
  
  complexity <- c(S, S_prey, S_pred, L, D, C)
  
  return(complexity)
}

get_in_out_degree <- function(tibble){
  
  prey <- vector()
  for (i in 1:nrow(tibble)){
    k <- 0
    for (j in 2:ncol(tibble)){
      if (is.na(tibble[i,j]) == F){
        k <- k+1
      }
    }
    prey <- c(prey, k)
  }
  G <- round(mean(prey), 4)
  sG <- round(sd(prey), 4)

  pred <- vector()
  for (j in 2:ncol(tibble)){
    k <- 0
    for (i in 1:nrow(tibble)){
      if (is.na(tibble[i,j]) == F){
        k <- k+1
      }
    }
    pred <- c(pred, k)
  }
  V <- round(mean(pred), 4)
  sV <- round(sd(pred), 4)

  schtroumpf <- c(G, sG, V, sV)
  return(schtroumpf)
}

centralities <- function(tibble){
  tibble$predator <- as.numeric(seq(1, nrow(tibble)))
  colnames(tibble) <- c("predator", seq(nrow(tibble)+1, nrow(tibble) + ncol(tibble)))
  
  edgelist <- vector()
  
  for (i in 1:nrow(tibble)){
    for (j in 2:ncol(tibble)){
      if (is.na(tibble[i,j]) == F){
        edge1 <- cbind(as.numeric(tibble$predator[i]), as.numeric(colnames(tibble[,j])), 1)
        edge2 <- cbind(as.numeric(colnames(tibble[,j])), as.numeric(tibble$predator[i]), 1)
        edgelist <- rbind(edgelist, edge1, edge2)
      }
    } 
    colnames(edgelist) <- c("pred", "prey", "int")
  }
  
  s <- nrow(tibble) + ncol(tibble) - 1
  n <- nrow(tibble) - 1
  m <- ncol(tibble) - 2
  
  c <- as.data.frame(closeness_w(edgelist)) %>%
    mutate(
      distance = 1/closeness,
      n.closeness =  (s-1)/distance
    ) %>% 
    select(node, distance, closeness, n.closeness)
  
  bw <- as.data.frame(betweenness_w(edgelist)) %>% 
    mutate(n_betweenness_w = betweenness/(n*m)) %>% 
    rename(betweenness_w = betweenness)
  
  centralities <- c(mean(c[,3]), mean(c[,4]), mean(bw[,2]), mean(bw[,3]))
  
  return(centralities)
}

nestedness <- function(tibble){
  for (i in 1:nrow(tibble)){
    for (j in 2:ncol(tibble)){
      if (is.na(tibble[i,j]) == T){
        tibble[i,j] <- 0
      }
    }
  }
  
  tibble <- as.matrix(tibble[c(-1)])
  n <- bipartite::nested(t(tibble), method = c("NODF2", "weighted NODF"), rescale=F, normalised=TRUE)
  return(n)
}

# get_Beckett_Q <- function(tibble){
#   pred <- as.matrix(tibble[1])
#   tibble <- tibble[c(-1)]
#   
#   tibble <- tibble %>% 
#     mutate(across(colnames(tibble[1]):colnames(tibble[ncol(tibble)]), 
#                   ~ case_when(is.na(.) == T ~ 0, TRUE ~ .)))
#   
#   tibble <- as.matrix(tibble)
#   row.names(tibble) <- pred
#   
#   MOD1 = DIRT_LPA_wb_plus(tibble)
#   mod_info <- GetModularInformation(tibble, MOD1)
#   
#   Qb <- c(mod_info$modularity, mod_info$normalised_modularity, mod_info$number_of_modules)
#   
#   return(Qb)
# }

get_Newman_Q <- function(tibble){
  pred <- as.matrix(tibble[1])
  tibble <- tibble[c(-1)]
  
  tibble <- tibble %>% 
    mutate(across(colnames(tibble[1]):colnames(tibble[ncol(tibble)]), 
                  ~ case_when(is.na(.) == T ~ 0, TRUE ~ 1)))
  
  tibble <- as.matrix(tibble)
  row.names(tibble) <- pred
  
  mod_info <- computeModules(tibble, method = "Beckett")
  Qn <- mod_info@likelihood
  
  return(Qn)
}

#### ALL IN ONE TOPO ####
get_network_topo <- function(tibble){
  if (is.null(tibble) == T){
    topo <- rep(NA, 17)
  }
  else{
    complexity <- network_complexity(tibble)
    gen_vul <- get_in_out_degree(tibble)
    Cc <- centralities(tibble)
    N <- nestedness(tibble)
    #Qb <- get_Beckett_Q(tibble)
    Qn <- get_Newman_Q(tibble)
    topo <- c(complexity, gen_vul, Cc, N, Qn)
  }
  return(topo)
}

#### PARALLEL ####
get_all_topo <- function(list_mat_int, surveyID){
  topo_list <- mclapply(list_mat_int[1:562], get_network_topo, mc.cores = 52)
  site <- names(topo_list)

  topo_df <- data.frame(matrix(unlist(topo_list), nrow = 562, byrow = TRUE), stringsAsFactors = FALSE)
  topo_df$SiteCode <- site

  topo_df <- topo_df %>%
    rename(S = X1, Sprey = X2, Spred = X3, L = X4, D = X5, C = X6,
           G = X7, sG = X8, V = X9, sV = X10,
           Cc = X11, nCc = X12, Bc = X13, nBc = X14,
           NODF2 = X15, weighted_NODF = X16,
           #Qb = X17, norm_Qb = X18, nb_of_modules = X19,
           Qn = X17
           ) %>%
    select(SiteCode, S:Qn) %>%
    arrange(SiteCode)

  topo_df <- left_join(surveyID, topo_df, by = "SiteCode")
  
  return(topo_df)
}