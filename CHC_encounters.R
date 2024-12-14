## TB-Free Chuuk R code
## CHC Encounters for reporting to CHC CEO

## variables needed
##date_encounter	  last_name	    first_name	        island	
##village	          site	        date_of_birth	      age	
##age_group	        sex	          chief_complain      weight	
##height	          a1c           ppd	(Y/N)           xray (Y/N)     sputum_afb (Y/N)

#PACKAGES
library(tidyverse) 
library(readxl)
library(openxlsx)
library(lubridate)
library(stringr)

#formulas
`%notin%` <- Negate(`%in%`)

#data for southern CHC
south_chc_encounters <- read_excel("Data/flatfile.xlsx", col_names = TRUE, guess_max = 200000) %>%
  rename(date_encounter = date_screening, island = municipality) %>%
  filter(screening_site == "South CHC – Weno" & is.not.na(date_encounter)) %>%
  mutate(site = "South CHC - Neauo",
         chief_complain = "TB MASS SCREENING", 
         ppd = ifelse(had_tst_placed == 1, "Y", NA),
         xray = ifelse(xray_indicated == "Y", "Y", NA),
         sputum_afb = ifelse(sputum_sent == 1, "Y", NA),
         weight = as.numeric(weight),
         height = as.numeric(height)) %>%
  select(`date_encounter`, `onsite_id`, `last_name`, `first_name`, `island`, `village`, `site`, `date_of_birth`,
         `age`, `age_group`, `sex`, `chief_complain`, `weight`, `height`, `a1c`, `ppd`, `xray`, `sputum_afb`) %>%
  mutate_at(c("height", "weight"), ~na_if(., 0)) %>%
  arrange(date_encounter)

write.xlsx(south_chc_encounters, 'Data/All Neauo CHC Encounters_x.xlsx')
