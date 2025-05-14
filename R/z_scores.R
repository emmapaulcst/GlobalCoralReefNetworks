# DO NOT RUN
# RUNS ~ 1 OR 2 DAYS
# NEEDS MEMORY AND CORES

#### Libraries ####
library(dplyr)
library(bipartite)
library(targets)
library(parallel)

#### Load Data ####
tar_load(list_mat_int_Ic)
tar_load(global_mat_int)

# mat_int <- list_mat_int_Ic[[1]]

#### Make binary matrices ####
binary_mat <- function(mat_int){
  n <- ncol(mat_int)
  
  binary_mat_int <- mat_int %>% 
    mutate(across(colnames(mat_int[2]):colnames(mat_int[n]),  ~ case_when(is.na(.) == T ~ 0,
                                                                          TRUE ~ 1))) %>% 
    select(-predator) %>% 
    as.matrix()
  
  return(binary_mat_int)
}

make_binary_mats <- function(list_mat_int_Ic){
  n <- length(list_mat_int_Ic)
  
  list_mat_int_b <- mclapply(list_mat_int_Ic[1:n], binary_mat, mc.cores = 55)
  
  return(list_mat_int_b)
}

load("PAPER_DATA/z_scores/list_mat_int_b.rdata")
# list_mat_int_b <- make_binary_mats(list_mat_int_Ic)
# save(list = "list_mat_int_b", file = 'PAPER_DATA/z_scores/list_mat_int_b.rdata')

#### Null models #### 

# mat_int_b <- list_mat_int_b[[1]]

make_null_models <- function(mat_int_b){
  
  null_mats <- bipartite::nullmodel(mat_int_b, N = 1000, method = 4)
  
  return(null_mats)
}

# null_mats <- make_null_models(mat_int_b)

get_list_null_models <- function(list_mat_int_b){
  
  n <- length(list_mat_int_b)
  list_null_mats <- mclapply(list_mat_int_b[1:n], make_null_models, mc.cores = 55)  
  
  return(list_null_mats)
}

# load("PAPER_DATA/z_scores/list_null_mats.rdata")
# list_null_mats <- get_list_null_models(list_mat_int_b)
# save(list = "list_null_mats", file = 'PAPER_DATA/z_scores/list_null_mats.rdata')


#### Qn ####

get_Newman_Q <- function(mat_int_b){
  
  mod_info <- computeModules(mat_int_b, method = "Beckett")
  Qn <- mod_info@likelihood
  
  return(Qn)
}

get_Qn_z_score <- function(mat_int_b, null_mats){
  # Si appel dans mclapply, ces arguments pour la fonction ici
  # s
  # mat_int_b <- list_mat_int_b[[s]]
  # null_mats <- list_null_mats[[s]]
  
  Qn_observed <- get_Newman_Q(mat_int_b)
  
  n <- length(null_mats)
  Qn_nulls <- mclapply(null_mats[1:n], get_Newman_Q, mc.cores = 55)
  Qn_nulls <- data.frame(matrix(unlist(Qn_nulls), nrow = n, byrow = TRUE), stringsAsFactors = FALSE)
  names(Qn_nulls) <- "Qn"
  
  z_score <- (Qn_observed - mean(Qn_nulls$Qn))/sd(Qn_nulls$Qn) %>% 
    as.data.frame(.) %>% 
    setNames(., c("Qn"))
  
  return(z_score)
}

get_Qn_z_scores <- function(start, end, list_mat_int_b, list_null_mats){
  
  # start <- 1
  # end <- 10
  
  sites <- names(list_mat_int_b) %>% 
    as.data.frame() %>% 
    setNames("SiteCode")
  
  # n_sites <- length(list_mat_int_b)
  # n_sites <- 2
  
  # Un fct de 55 cores qui appelle une fct de 55 cores pas bon ça ...
  # list_z_score <- mclapply(1:n,
  #                          list_mat_int_b = list_mat_int_b, 
  #                          list_null_mats = list_null_mats,
  #                          get_all_Qn, mc.cores = 55)
  
  z_scores <- vector()
  
  for (i in start:end){
    z_score <- get_Qn_z_score(list_mat_int_b[[i]], list_null_mats[[i]])
    z_scores <- rbind(z_scores, z_score)
  }
  
  z_Qns <- cbind(z_scores, sites %>% slice(start:end))
  
  path_to_file <- paste("PAPER_DATA/z_scores/z_Qns/z_Qns_", end, ".rdata", sep = "")
  save(list = "z_Qns", file = path_to_file)
  
  return(z_Qns)
}

# z_Qn <- get_Qn_z_scores(n_sites = 552, list_mat_int_b, list_null_mats)
# jump <- seq(from = 510, to = 550, by = 10)

# for (i in jump){
#   start <- i
#   end <- i + 9
#   
#   z_Qn <- get_Qn_z_scores(start = start, end = end, list_mat_int_b, list_null_mats)
# }

# z_Qn <- get_Qn_z_scores(start = 560, end = 562, list_mat_int_b, list_null_mats)


# z_Qns_bind <- data.frame()
z_Qns_bind <- rbind(z_Qns_bind, z_Qn)

save(list = "z_Qns", 
     file = "PAPER_DATA/z_scores/z_Qns/z_Qns.rdata")

#### NODF ####

get_NODF2 <- function(mat_int_b){
  
  N <- bipartite::nested(mat_int_b, method = "NODF2", rescale = F, normalised = TRUE)
  
  return(N)
}

get_N_z_score <- function(mat_int_b, null_mats){
  # Si appel dans mclapply, ces arguments pour la fonction ici
  # s
  # mat_int_b <- list_mat_int_b[[s]]
  # null_mats <- list_null_mats[[s]]
  
  N_observed <- get_NODF2(mat_int_b)
  
  n <- length(null_mats)
  N_nulls <- mclapply(null_mats[1:n], get_NODF2, mc.cores = 55)
  N_nulls <- data.frame(matrix(unlist(N_nulls), nrow = n, byrow = TRUE), stringsAsFactors = FALSE)
  names(N_nulls) <- "N"
  
  z_score <- (N_observed - mean(N_nulls$N))/sd(N_nulls$N) %>% 
    as.data.frame(.) %>% 
    setNames(., c("N"))
  
  return(z_score)
}

get_N_z_scores <- function(n_sites, list_mat_int_b, list_null_mats){
  
  sites <- names(list_mat_int_b) %>% 
    as.data.frame() %>% 
    setNames("SiteCode")
  
  # n_sites <- length(list_mat_int_b)
  # n_sites <- 2
  
  z_scores <- vector()
  
  for (i in 1:n_sites){
    z_score <- get_N_z_score(list_mat_int_b[[i]], list_null_mats[[i]])
    z_scores <- rbind(z_scores, z_score)
  }
  
  z_Ns <- cbind(z_scores, sites %>% slice(1:n_sites))
  
  save(list = "z_Ns", file = 'PAPER_DATA/z_scores/z_Ns.rdata')
  
  return(z_Ns)
}

# z_N <- get_N_z_scores(n_sites = 562, list_mat_int_b, list_null_mats)

z_scores <- inner_join(z_Ns %>% rename(zN = N),
                       z_Qns %>% rename(zQn = Qn), 
                       by = "SiteCode") %>% 
  select(SiteCode, zN, zQn)
# save(list = "z_scores", file = 'PAPER_DATA/z_scores/z_scores.rdata')
# write.csv(z_scores, 'PAPER_DATA/z_scores/z_scores.csv', row.names = F)

