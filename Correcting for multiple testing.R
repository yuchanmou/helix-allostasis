##' @title Multiple testing  
##' @author Yuchan Mou
##' @details Created on September 2023 

##' @description 


# 1. Multiple testing 
library(dplyr)
library(psych) # for describe
library(poolr) # for meff: calculating effective number of tests

# Calculate number of effective tests for the outcomes
load("/Users/ymou/helix_project/data/analysis_data/imp_full_ds.Rdata")
analysis_date <- Sys.Date()

ap <- c(
  "hs_no2_yr_hs_t",
  "hs_pm25_yr_hs_t",
  "hs_pm25abs_yr_hs_t",
  "hs_pm10_yr_hs_t"
)
noise <- c("hs_lden_c_h", "hs_lden_c_s")
out <- c("AL_2_tot", "AL_z2_tot")

#Use Galwey to estimate number of effective tests
df <- imp_full$data %>% 
  select(all_of(ap), all_of(noise), all_of(out)) %>% 
  mutate_if(is.factor, as.numeric) %>%
  cor(use="pairwise.complete.obs", method = "spearman")
corrplot::corrplot(df, method = "square", tl.col = "black", tl.cex=0.5)
for(i in 1:8)df[i,i]<-1 #To make sure the diagonal is one
meff(df, method = "galwey") #-----> n = 5

out_ps <- c("AL_2_cardio", "AL_2_metab", "AL_2_immune", "AL_2_neuro", 
            "AL_z2_cardio", "AL_z2_metab", "AL_z2_immune", "AL_z2_neuro")

#Use Galwey to estimate number of effective tests
df <- imp_full$data %>% 
  select(all_of(ap), all_of(out_ps)) %>% 
  mutate_if(is.factor, as.numeric) %>%
  cor(use="pairwise.complete.obs", method = "spearman")
corrplot::corrplot(df, method = "square", tl.col = "black", tl.cex=0.5)
for(i in 1:12)df[i,i]<-1 #To make sure the diagonal is one
meff(df, method = "galwey") #-----> n = 5
