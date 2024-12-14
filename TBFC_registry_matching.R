#REGISTRY MATCHING


#PACKAGES
library(tidyverse) 
library(readxl)
library(openxlsx)
library(lubridate)
library(stringr)
library(janitor)
library(naniar)
library(fuzzyjoin)

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

####
#### FUZZY MATCHING
####


##FUZZY MATCH

#get flatfile data
flatfile_to_match <- read_excel("Data/flatfile_clean.xlsx",
           guess_max = 20000, col_names = TRUE) %>%
  mutate(name = paste(toupper(last_name), toupper(first_name)),
         rev_name = paste(toupper(first_name),toupper(last_name))) %>%
  select(registration_no, name, rev_name, date_of_birth, 
         tst_date_read, tst_result, tb_disease_exposure, tb_disease_exposure_name,
         treated_tb_ltbi, treated_tb_ltbi_program, xray_result_preliminary,
         active_tb, outcome_case_conference, epi_status, active_tb_tx)

#get tkk registry to match
tkk_registry <- read_excel("Data/Registry Matching/TKK Registry 24 to TBFC 23 Matching.xlsx",
                           guess_max = 20000, col_names = TRUE) %>%
  clean_names() %>%
  mutate(name = paste(toupper(last_name), toupper(first_name)),
         rev_name = paste(toupper(first_name),toupper(last_name)))


#HSOB Match datasets
fuzzy_names <- stringdist_left_join(
  tkk_registry, flatfile_to_match,
  # by= c(name = "name"),
  by= c(name = "rev_name"),
  max_dist=3, method = "lv", distance_col="distance"
) %>%
  filter(is.not.na(distance))

write.xlsx(fuzzy_names, "Data/Registry Matching/registry_match_rev_name.xlsx")

# 
# tkk_registry$match_date_minus3 <- ymd(tkk_registry$date_of_birth) - days(3)
# tkk_registry$match_date_plus3 <- ymd(tkk_registry$date_of_birth) + days(3)
# 
# tkk_registry$match_date_minus3 <- as.Date(tkk_registry$match_date_minus3)
# tkk_registry$match_date_plus3 <- as.Date(tkk_registry$match_date_plus3)
# 
# fuzzy_dates<-fuzzy_left_join(tkk_registry, flatfile_to_match,
#                              by = c("name" = "name",
#                                     'date_of_birth' = "match_date_minus3",
#                                     'date_of_birth' = "match_date_plus3"),
#                              match_fun = list(`==`, `>`, `<`))
# 
# write.xlsx(fuzzy_dates, "Data/Registry Matching/registry_match_dates.xlsx")



#######
#######
#######
#######
#HANSENS DISEASE


#PACKAGES
library(tidyverse) 
library(readxl)
library(openxlsx)
library(lubridate)
library(stringr)
library(janitor)
library(naniar)
library(fuzzyjoin)

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

####
#### FUZZY MATCHING
####


##FUZZY MATCH

#get flatfile data
hd_registry <- read_excel("Data/Hansen's Disease 2024.xlsx",
                                sheet = "2023",
                                guess_max = 30, col_names = TRUE) %>%
  select(1:13) %>%
  rbind(read_excel("Data/Hansen's Disease 2024.xlsx",
                   sheet = "2024",
                   guess_max = 20, col_names = TRUE) %>%
          select(1:13)) %>%
  clean_names() %>%
  mutate(name = paste(toupper(last_name), toupper(first_name)),
         rev_name = paste(toupper(first_name),toupper(last_name))) %>%
  rename(date_of_birth=dob) %>%
  select(name, rev_name, date_of_birth, 
         sex, age, village, municipality, jurisdiction)

#get tkk registry to match
tkk_registry <- read_excel("Data/Registry Matching/TKK Registry 24 to TBFC 23 Matching.xlsx",
                           guess_max = 20000, col_names = TRUE) %>%
  clean_names() %>%
  mutate(name = paste(toupper(last_name), toupper(first_name)),
         rev_name = paste(toupper(first_name),toupper(last_name)))


#HSOB Match datasets
fuzzy_names <- stringdist_left_join(
  tkk_registry, flatfile_to_match,
  # by= c(name = "name"),
  by= c(name = "rev_name"),
  max_dist=3, method = "lv", distance_col="distance"
) %>%
  filter(is.not.na(distance))

write.xlsx(fuzzy_names, "Data/Registry Matching/registry_match_rev_name.xlsx")
