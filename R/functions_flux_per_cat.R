
# mat_int <- read_csv("PAPER_DATA/original_data/interaction_matrix.csv")
# prey <- as.data.frame(colnames(mat_int[2:ncol(mat_int)]))
# colnames(prey) <- "prey"
# 
# load("PAPER_DATA/script_output/list_mat_int_Ic.rdata")


get_prey_cat <- function(path_to_prey_cat){
  prey_cat <- read_delim(path_to_prey_cat, 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    mutate(prey_cat = case_when(prey_cat == "benthic autotrophs" ~ "bAut",
                                prey_cat == "detritus" ~ "det",
                                prey_cat == "fish" ~ "fish",
                                prey_cat == "mobile benthic invertebrate" ~ "mInv",
                                prey_cat == "mobile macroinvertebrate" ~ "mInv",
                                prey_cat == "mobile microinvertebrate" ~ "mInv",
                                prey_cat == "sessile benthic invertebrate" ~ "sInv",
                                prey_cat == "zooplankton" ~ "zooP",
                                TRUE ~ prey_cat)) %>% 
    mutate(prey_cat = case_when(prey == "Chordata"| prey == "Mollusca" ~ NA,
                                TRUE ~ prey_cat)) %>% 
    filter(is.na(prey_cat) == F)
}


somme_prey_cat <- function(s, prey_cat, list_mat_int){
  
  tibble <- as.data.frame(t(list_mat_int[[s]][-1]))
  tibble <- rownames_to_column(tibble, var = 'prey')
  
  carbon_fluxes_df <- left_join(prey_cat, tibble, by = "prey")  %>% 
    rowwise() %>% 
    mutate(flux_per_prey = sum(c_across(colnames(tibble[3]):colnames(tibble[ncol(tibble)])), na.rm = T)) %>% 
    select(prey, prey_cat, flux_per_prey) %>% 
    group_by(prey_cat) %>% 
    summarise(flux = round(sum(flux_per_prey), 2)) %>% 
    filter(is.na(prey_cat) == F)
  
  tibble_cat <- as.data.frame(c("bAut", "det", "fish", "mInv", "sInv", "zooP"), nm = "prey_cat")
  
  carbon_fluxes_df <- full_join(carbon_fluxes_df, tibble_cat, by = "prey_cat") %>%
    pivot_wider(names_from = prey_cat, values_from = flux) %>% 
    select("bAut", "det", "fish", "mInv", "sInv", "zooP")
  
  carbon_fluxes_df <- carbon_fluxes_df %>% 
    mutate(across('bAut':'zooP', ~ case_when(is.na(.) == T ~ 0,
                                             TRUE ~ .)))
  return(carbon_fluxes_df)
}

gives_flux_per_cat <- function(prey_cat, list_mat_int){
  list_col_sums <- mclapply(1:562,
                            prey_cat = prey_cat, 
                            list_mat_int = list_mat_int,
                            somme_prey_cat, mc.cores = 55)
  
  flux <- data.frame(matrix(unlist(list_col_sums), nrow = 562, byrow = TRUE), stringsAsFactors = FALSE) 
  colnames(flux) <- t(tibble_cat <- as.data.frame(c("bAut", "det", "fish", "mInv", "sInv", "zooP"), nm = "prey_cat"))
  flux$site <- names(list_mat_int)

  flux <- flux %>%
    select(site, 'bAut':'zooP')

  flux_prop <- flux %>%
    mutate(sum_tot = rowSums(across('bAut':'zooP'), na.rm=T)) %>%
    mutate(across('bAut':'zooP', ~ .x/sum_tot)) %>%
    rename("bAut_prop" = "bAut", "det_prop" = "det", "fish_prop" = "fish", "mInv_prop" = "mInv",
           "sInv_prop" = "sInv", "zooP_prop" = "zooP")

  flux_per_prey_Ic <- left_join(flux, flux_prop, by = "site") %>%
    select(site, 'bAut':'zooP', sum_tot, "bAut_prop":"zooP_prop")
  
  return(flux_per_prey_Ic)
}

# get_info_on_fluxes <- function(flux_per_prey_Ic){
#   mean <- flux_per_prey_Ic %>%
#     summarise(across(bAut_prop:zooP_prop, ~ round(mean(.x, na.rm = T), 2))) %>%
#     pivot_longer(cols = bAut_prop:zooP_prop, names_to = "flux", values_to = "mean")
#   
#   median <- flux_per_prey_Ic %>%
#     summarise(across(bAut_prop:zooP_prop, ~ round(median(.x, na.rm = T), 2))) %>%
#     pivot_longer(cols = bAut_prop:zooP_prop, names_to = "flux", values_to =  "median")
#   
#   max <- flux_per_prey_Ic %>%
#     summarise(across(bAut_prop:zooP_prop, ~ round(max(.x, na.rm = T), 2))) %>%
#     pivot_longer(cols = bAut_prop:zooP_prop, names_to = "flux", values_to =   "max")
#   
#   min <- flux_per_prey_Ic %>%
#     summarise(across(bAut_prop:zooP_prop, ~ round(min(.x, na.rm = T), 2))) %>%
#     pivot_longer(cols = bAut_prop:zooP_prop, names_to = "flux", values_to =  "min")
#   
#   sd <- flux_per_prey_Ic %>%
#     summarise(across(bAut_prop:zooP_prop, ~ round(sd(.x, na.rm = T), 2))) %>%
#     pivot_longer(cols = bAut_prop:zooP_prop, names_to = "flux", values_to =  "sd")
#   
#   tab <- list(mean, median, max, min, sd) %>%
#     purrr::reduce(inner_join, by = "flux")
#   
#   return(tab)
# }
