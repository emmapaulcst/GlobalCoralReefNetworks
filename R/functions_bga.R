
#### DATASETS ####

# BENTHOS #

get_benthos <- function(path_to_benthos, surveyID){
  benthos <- read_csv(path_to_benthos, 
                      col_types = "ccnnnnnnnnnnnnnnnnnnnn") %>% 
    select(-"0", - "#N/A" ) %>% 
    semi_join(., surveyID, by = c("SiteCode", "SurveyID"))  %>% 
    select("SiteCode", "Macroalgae", "Stony corals", "Substrate") %>% 
    mutate(across('Macroalgae':'Substrate',
                  ~ case_when(is.na(.) == T ~ 0, TRUE ~ .)))
  
  return(benthos)
}

# PREDICTORS #

get_predictors <- function(path_to_predictors, surveyID){
  predictors <- read_csv(path_to_predictors) %>% semi_join(., surveyID %>% select(SurveyID), by = "SurveyID")
  
  return(predictors)
}

get_z_scores <- function(path_to_z_scores){
  z_scores <- read_csv(path_to_z_scores)
  # z_scores <- get(z_scores)
  
  return(z_scores)
}

#### ASSEMBLE ####

gives_bga_dataset <- function(topology, z_scores, benthos, predictors){
  
  site_info <- predictors[1:12] %>%
    select(SiteCode, SiteLatitude:SiteLongitude, Country:Location, MPA)
  
  env <- predictors %>% 
    select(SiteCode, mean_chl_1year, mean_DHW_1year, mean_npp_1year, mean_pH_1year, 
           mean_sss_1year, mean_sst_1year, mean_wave, Branch_space, Rugosity, coral, algae, turf)
  
  anthro <- predictors %>% 
    select(SiteCode, Naturalresourcesrents, MarineEcosystemDependency, 
           neartt, poptot, gravtot1, gravtot2, gravtot3)
  
  bga <- list(topology, site_info, env, anthro, benthos) %>%
    purrr::reduce(inner_join, by = "SiteCode") %>% 
    filter(is.na(Branch_space) == F)
  
  bga <- left_join(bga, z_scores, by = "SiteCode") %>% 
    select(SiteCode:Qn, zN, zQn, SiteLatitude:Substrate)
  
  return(bga)
  }