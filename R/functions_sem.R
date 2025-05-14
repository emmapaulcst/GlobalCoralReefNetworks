# library(brms)
# library(dplyr)
# library(readr)
# library(ggplot2)
# library(ggpubr)
# library(scales)
# library(loo)
# library(parallel)
# library(brms)
# library(dplyr)
# library(tidyr)
# library(readr)
# library(ggplot2)
# library(ggpubr)
# library(DiagrammeR)
# library(scales)
# library(rstantools)
# library(grid)

gives_data_SEM <- function(bga, flux_per_prey_Ic){
  
  #### Get data ready ####
  
  ##### Load data ####
  
  flux <- flux_per_prey_Ic %>%
    select(site, bAut_prop:zooP_prop) %>%
    rename("bAut" = "bAut_prop",
           "det" = "det_prop",
           "fish" = "fish_prop",
           "mInv" = "mInv_prop",
           "sInv" = "sInv_prop",
           "zooP" = "zooP_prop")
  
  ##### assemblage data ####
  
  data <- left_join(bga, flux, by = c("SiteCode" = "site")) %>% 
    select(SiteCode, Realm,
           S, L, C, Cc, nCc, Bc, nBc, G, V, NODF2, Qn,
           mean_chl_1year:Rugosity,
           coral:turf, 
           gravtot2,
           bAut:zooP
    ) %>% 
    dplyr::rename(N = NODF2,
                  sst = mean_sst_1year,
                  npp = mean_npp_1year,
                  dhw = mean_DHW_1year, 
                  pH = mean_pH_1year,
                  wave = mean_wave,
                  gravity = gravtot2,
    ) %>% 
    ungroup() %>% 
    mutate(npp = npp*500/1000) %>% # NPP mgC per m2 per day *500 (transect) / 1000 (g)
    mutate(carbon_flow = bAut + det + fish + mInv + sInv + zooP) %>% 
    mutate(
      # carbon flow
      carbon_flow = (log(carbon_flow+1) - mean(log(carbon_flow+1))) / sd(log(carbon_flow+1)),

      # archi
      S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
      C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
      Cc = (log(Cc+1) - mean(log(Cc+1))) / sd(log(Cc+1)),
      nCc = (log(nCc+1) - mean(log(nCc+1))) / sd(log(nCc+1)),
      Bc = (log(Bc+1) - mean(log(Bc+1))) / sd(log(Bc+1)),
      nBc = (log(nBc+1) - mean(log(nBc+1))) / sd(log(nBc+1)),
      N = (log(N+1) - mean(log(N+1))) / sd(log(N+1)),
      Qn = (log(Qn+1) - mean(log(Qn+1))) / sd(log(Qn+1)),

      L = (log(L+1) - mean(log(L+1))) / sd(log(L+1)),
      G = (log(G+1) - mean(log(G+1))) / sd(log(G+1)),
      V = (log(V+1) - mean(log(V+1))) / sd(log(V+1)),

      # bga
      npp = (log(npp+1) - mean(log(npp+1))) / sd(log(npp+1)),
      dhw = (log(dhw+1) - mean(log(dhw+1))) / sd(log(dhw+1)),
      wave = (log(wave+1) - mean(log(wave+1))) / sd(log(wave+1)),
      gravity = (log(gravity+1) - mean(log(gravity+1))) / sd(log(gravity+1))) %>%

    select(
      SiteCode, Realm,
      dhw,
      sst, wave, npp, gravity,
      coral, algae, turf,
      S, C, Cc, nCc, Bc, nBc, N, Qn,
      L, G, V,
      bAut:zooP,
      carbon_flow)
  
  k <- 0.00001

  data <- data %>%
    mutate(
      coral = coral/100,
      algae = algae/100,
      turf = turf/100,
      sum_benthos = coral + algae + turf) %>%
    filter(sum_benthos <= 1) %>%
    mutate(across(coral:turf,  ~ case_when(. == 0 ~ k,
                                           TRUE ~ .))) %>%
    mutate(across(coral:turf,  ~ case_when(. == 1 ~ 1-k,
                                           TRUE ~ .)))

  data <- data %>%
    mutate(across(bAut:zooP,  ~ case_when(. == 0 ~ k,
                                          TRUE ~ .))) %>%
    mutate(across(bAut:zooP,  ~ case_when(. == 1 ~ 1-k,
                                          TRUE ~ .)))
  
  return(data)
}

make_SEM <- function(data_sem){
  #### SEM ####
  
  ##### benthos | layer 2 ####
  
  coral <- bf(coral ~ 1 + npp + gravity + dhw + (1|Realm), family = Beta) 
  algae <- bf(algae ~ 1 + npp + gravity + dhw + (1|Realm), family = Beta) 
  turf <- bf(turf ~ 1 + npp + gravity + dhw + (1|Realm), family = Beta)
  
  ##### topo | layer 3 ####
  
  s <- bf(S ~ 1 + coral + algae + turf + npp + gravity + sst + (1|Realm))
  c <- bf(C ~ 1 + coral + algae + turf + npp + gravity + sst + (1|Realm))
  n <- bf(N ~ 1 + coral + algae + turf + npp + gravity + sst + (1|Realm))
  q <- bf(Qn ~ 1 + coral + algae + turf + npp + gravity + sst + (1|Realm))
  
  ##### fluxes | layer 4 ####
  
  bAut <- bf(bAut ~ 1 + algae + turf + gravity + S + C + N + Qn + (1|Realm), family = Beta)
  det <- bf(det ~ 1 + algae + turf + gravity + npp + S + C + N + Qn + (1|Realm), family = Beta)
  fish <- bf(fish ~ 1 + coral + gravity + S + C + N + Qn + (1|Realm), family = Beta)
  mInv <- bf(mInv ~ 1 + coral + algae + turf + gravity + S + C  + N + Qn + (1|Realm), family = Beta)
  sInv <- bf(sInv ~ 1 + coral + algae + turf + gravity + S + C + N + Qn + (1|Realm), family = Beta)
  zooP <- bf(zooP ~ 1 + coral + npp + gravity + S + C + N + Qn + (1|Realm), family = Beta)
  
  semPriors <- get_prior(data = data_sem,
                         bAut + det + fish + mInv + sInv + zooP +
                           s + c + n + q +
                           coral + algae + turf) %>%
    mutate(prior = case_when(class == as.character("b") & coef != "" ~ "normal(0, 1)",
                             TRUE ~ prior))
  
  fit_sem <- brm(
    bAut + det + fish + mInv + sInv + zooP +
      s + c + n + q +
      coral + algae + turf,
    data = data_sem,
    prior = semPriors,
    chains = 4, cores = 4, iter = 2000, warmup = 1000,
    backend = "cmdstanr",
    threads = 12)
  
  return(fit_sem)
}

make_SEM_5000 <- function(data_sem){
  #### SEM ####
  
  ##### benthos | layer 2 ####
  
  coral <- bf(coral ~ 1 + npp + gravity + dhw + (1|Realm), family = Beta) 
  algae <- bf(algae ~ 1 + npp + gravity + dhw + (1|Realm), family = Beta) 
  turf <- bf(turf ~ 1 + npp + gravity + dhw + (1|Realm), family = Beta)
  
  ##### topo | layer 3 ####
  
  s <- bf(S ~ 1 + coral + algae + turf + npp + gravity + sst + (1|Realm))
  c <- bf(C ~ 1 + coral + algae + turf + npp + gravity + sst + (1|Realm))
  n <- bf(N ~ 1 + coral + algae + turf + npp + gravity + sst + (1|Realm))
  q <- bf(Qn ~ 1 + coral + algae + turf + npp + gravity + sst + (1|Realm))
  
  ##### fluxes | layer 4 ####
  
  bAut <- bf(bAut ~ 1 + algae + turf + gravity + S + C + N + Qn + (1|Realm), family = Beta)
  det <- bf(det ~ 1 + algae + turf + gravity + npp + S + C + N + Qn + (1|Realm), family = Beta)
  fish <- bf(fish ~ 1 + coral + gravity + S + C + N + Qn + (1|Realm), family = Beta)
  mInv <- bf(mInv ~ 1 + coral + algae + turf + gravity + S + C  + N + Qn + (1|Realm), family = Beta)
  sInv <- bf(sInv ~ 1 + coral + algae + turf + gravity + S + C + N + Qn + (1|Realm), family = Beta)
  zooP <- bf(zooP ~ 1 + coral + npp + gravity + S + C + N + Qn + (1|Realm), family = Beta)
  
  semPriors <- get_prior(data = data_sem,
                         bAut + det + fish + mInv + sInv + zooP +
                           s + c + n + q +
                           coral + algae + turf) %>%
    mutate(prior = case_when(class == as.character("b") & coef != "" ~ "normal(0, 1)",
                             TRUE ~ prior))
  
  fit_sem <- brm(
    bAut + det + fish + mInv + sInv + zooP +
      s + c + n + q +
      coral + algae + turf,
    data = data_sem,
    prior = semPriors,
    chains = 4, cores = 4, iter = 5000, warmup = 2500,
    backend = "cmdstanr",
    threads = 12)
  
  return(fit_sem)
}

##### analysis ####
# 
# loo(fit_sem)
# 
# bayes_R2(fit_sem)
# 
# summary(fit_sem)



