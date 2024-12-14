## TB-Free Chuuk R code
## Data cleaning for duplicates and compiling overwritten data

#PACKAGES
library(tidyverse)#pipes, etc
library(readxl) #reading excel
library(openxlsx) #write to excel
library(lubridate) #deal with dates
library(stringr) #deal with strings
library(vtable) #allows sumtable
library(janitor) #cleannames
library(eHDPrep) #quality of data checks
library(naniar) #clean up and replace blanks,etc with NAs
library(fuzzyjoin) #allows fuzzy matching of tables


#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

# write out all the offending strings
na_strings <- c(common_na_strings,"MISSING")

#base excel sheets
raw_excel_read <-function(x){
  read_excel(x,guess_max = 20000, col_names = TRUE) %>%
    modify_if(is.POSIXt, as_date) %>%
    mutate(file = x)
}

#shorten registration table to essential elements
short_reg <- function(x){ 
  x %>% ungroup() %>% 
    select(registration_id,registration_no,onsite_id,last_name,first_name,
           date_of_birth,sex,state,village,municipality,date_updated,file) %>% 
    mutate(registration_no = ifelse(str_length(registration_no) > 7, registration_no,
                                    sprintf("%07d", as.numeric(registration_no)))
    )
}

#shorten patient passport table to essential elements
short_pp <- function(x){ 
  x %>% ungroup() %>% 
    select("registration_id", "patient_passport_id", "date_updated", "file", "date_of_visit1", 
           "tst_date_read", "tst_result", "weight", "height", "a1c", "hd_prevention")
}


##READ IN REG FOR RBIND
reg_read_in <-function(x){
  x %>% 
    raw_excel_read() %>%
    short_reg()
}

##READ IN PP FOR RBIND
pp_read_in <-function(x){
  x %>% 
    raw_excel_read() %>%
    filter(date_updated  %in% seq(max(date_updated)-3,max(date_updated), by="days")) %>%
    short_pp()
}

#add raw tables from current excel exports

# raw_reg <- raw_excel_read("Data/Raw Excel Exports/REGISTRATION.xlsx") %>%
#   short_reg()  %>%
#   replace_with_na_if(.predicate = is.character,
#                      condition = ~.x %in% na_strings) %>%
#   replace_with_na_if(.predicate = is.Date,
#                      condition = ~.x < as_date("1930-01-01"))
# 
# raw_pp <- raw_excel_read("Data/Raw Excel Exports/PATIENT_PASSPORT.xlsx") %>%
#   short_pp() %>%
#   replace_with_na_if(.predicate = is.character,
#                      condition = ~.x %in% na_strings) %>%
#   replace_with_na_if(.predicate = is.Date,
#                      condition = ~.x < as_date("1930-01-01"))
# 
# write.xlsx(raw_reg,"Data/REGISTRATION_raw_Mar2_clean.xlsx")
# write.xlsx(raw_pp,"Data/PATIENT_PASSPORT_raw_Mar2_clean.xlsx")

raw_reg <- read_excel("Data/REGISTRATION_raw_Mar2_clean.xlsx")
raw_pp <- read_excel("Data/PATIENT_PASSPORT_raw_Mar2_clean.xlsx")


##
##REGISTRATION BULK
##

#list of all extracted registration excel sheets in HQ backups from 2023
reg_files <- list.files(path="HQ Backups/REGISTRATION", 
                        pattern="registration_2023", 
                        full.names=TRUE, recursive=FALSE)

bulk_reg <- raw_reg %>%
  rbind(do.call(rbind.data.frame, lapply(reg_files, reg_read_in))) 

working_reg <- bulk_reg %>%
  group_by(registration_id, registration_no, onsite_id, last_name, first_name,
           date_of_birth,sex,state,village,municipality) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(has_onsite_id = if_else(is.not.na(str_extract(`onsite_id`,"(\\d{6,6})")), 1, 
                                 if_else(grepl("LG", onsite_id), 1, 
                                         if_else(grepl("W",toupper(onsite_id)), 1, 0)))
         ) %>%
  group_by(registration_id,first_name,last_name) %>%
  arrange(desc(has_onsite_id)) %>%
  fill(everything(), .direction = "downup") %>%
  slice(1)%>%
  group_by(registration_id,date_of_birth) %>%
  arrange(desc(has_onsite_id)) %>%
  fill(everything(), .direction = "downup") %>%
  slice(1) %>%
  group_by(registration_id) %>%
  mutate(keep_decision = if_else(n() == 1, 1, NA),
         overwritten = NA) %>%
  rbind(read_excel("Data/Cleaning Files/duplicate_reg_ids.xlsx") %>%
          filter(keep_decision == 1)) %>%
  filter(keep_decision == 1) %>%
  select(-c(has_onsite_id,keep_decision,overwritten))


# 
# write.xlsx(working_reg %>%
#        ungroup() %>%
#        group_by(registration_id) %>%
#        filter(n() > 1) %>%
#        group_by(registration_id,date_of_birth) %>%
#        arrange(desc(has_onsite_id)) %>%
#        fill(everything(), .direction = "downup") %>%
#        slice(1) %>%
#        group_by(registration_id) %>%
#        filter(n() > 1),
#        "Data/Cleaning Files/duplicate_reg_ids.xlsx"
# )

overwritten <- read_excel("Data/Cleaning Files/duplicate_reg_ids.xlsx") %>%
        filter(overwritten == 1) %>%
  mutate(type = "overwrite")

dup_registration_nos <- working_reg %>%
  group_by(registration_no) %>%
  filter(n() > 1) %>%
  mutate(type = "dup_reg")

passports_needed <- rbind(overwritten %>% select(-c(keep_decision,has_onsite_id,overwritten)),
                          dup_registration_nos) %>%
  select(registration_id) %>%
  merge(bulk_reg %>% select(registration_id, first_name, file, date_updated))
  
  # write.xlsx(working_reg,"Data/Cleaning Files/current_working_reg.xlsx")
  # write.xlsx(dup_registration_nos,"Data/Cleaning Files/dup_registration_nos.xlsx")
  
  
 ##
 ##PATIENT PASSPORT BULK
 ##
 
#list of all registration databases in HQ backups from 2023
pp_files <- list.files(path="HQ Backups/PASSPORT", 
                       pattern="registration_2023", 
                       full.names=TRUE, recursive=FALSE)

#combine all pps
bulk_pp <- raw_pp %>%
  rbind(do.call(rbind.data.frame, lapply(pp_files, pp_read_in))) 

#cleannnnnn

working_pp <- bulk_pp %>%
  group_by(registration_id,patient_passport_id,date_updated,date_of_visit1,
           tst_date_read,tst_result,weight,height,a1c,hd_prevention) %>%
  slice(1) %>%
  mutate(tst_result = if_else(tst_result < 0, NA, tst_result)) %>%
  ungroup() %>%
  group_by(registration_id, patient_passport_id, date_of_visit1)  %>%
  arrange(desc(date_updated)) %>%
  fill(everything(), .direction = "downup") %>%
  slice(1) %>%
  ungroup() %>%
  mutate(na_count = rowSums(is.na(select(., c(date_of_visit1,
                                              tst_date_read,tst_result,weight,
                                              height,a1c,hd_prevention))))) %>%
  group_by(registration_id,patient_passport_id) %>%
  arrange(na_count) %>%
  slice(1) %>%
  select(-na_count)


pp_retrieved <- passports_needed %>%
  merge(bulk_pp, by= c("registration_id","file"))



##FULL  COMPILATION FILES
write.xlsx(working_reg, "Data/Cleaning Files/working_reg.xlsx")
write.xlsx(working_pp, "Data/Cleaning Files/working_pp.xlsx")



####
#### FUZZY MATCHING
####


##FUZZY MATCH

#prep fuzzymatch datasets
# prev_matches <- read_excel("Data/Cleaning Files/name_match_FEB12.xlsx") %>%
#   select(registration_no.x) %>%
#   rename(matched_reg = registration_no.x)

fuzzy <- bulk_reg  %>%
  filter(is.not.na(last_name) & is.not.na(first_name)) %>%
  mutate(name = paste(last_name, first_name),
         rev_name = paste(first_name,last_name)) %>%
  select(registration_no, municipality, village, sex, name, rev_name, date_of_birth, file)

#HSOB Match datasets
fuzzy_names <- stringdist_left_join(
  fuzzy, fuzzy,
  by= c(name = "name"),
  # by= c(name = "rev_name"),
  max_dist=3, method = "lv", distance_col="distance"
  ) %>%
  filter(is.not.na(distance)) %>%
  filter(registration_no.x %notin% prev_matches$matched_reg)

write.xlsx(fuzzy_names, "Matching/name_match.xlsx")


fuzzy$match_date_minus3 <- ymd(fuzzy$date_of_birth) - days(3)
fuzzy$match_date_plus3 <- ymd(fuzzy$date_of_birth) + days(3)

fuzzy$match_date_minus3 <- as.Date(fuzzy$match_date_minus3)
fuzzy$match_date_plus3 <- as.Date(fuzzy$match_date_plus3)

fuzzy_dates<-fuzzy_left_join(fuzzy, fuzzy,
                by = c("name" = "name",
                       'date_of_birth' = "match_date_minus3",
                       'date_of_birth' = "match_date_plus3"),
                match_fun = list(`==`, `>`, `<`)) %>%
  filter(is.not.na(registration_id.y))




###
## DATA QUALITY STEPS
###

# create a consistency table containing consistency rules
# below states: if a patient has a type of diabetes, they should have diabetes
ct <- tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
              "diabetes_type", "diabetes", NA, "Type I", "Yes",
              "diabetes_type", "diabetes", NA, "Type II", "Yes")


assess_quality(data = bulk_reg, id_var = registration_id, consis_tbl = ct)

import_var_classes(file = raw_reg)

apply_quality_control


review_quality_ctrl(before_tbl=raw_reg, after_tbl=bulk_reg, id_var=registration_id)



review_quality_ctrl(before_tbl=flatfile, after_tbl=tbfc_data, id_var=registration_id)




### 

# CHANGE LOG FOR FILES

#datasets
change_log <- read_excel("Data/Cleaning Files/data_cleaning_log.xlsx",
                           guess_max = 20000, col_names = TRUE) %>%
  clean_names() %>%
  filter(is.na(complete))

#remove registrations
reg_id <- change_log %>% filter(type == "Reg ID") %>% select(id)

registration_cleaning <- read_excel("Data/Raw Excel Exports/REGISTRATION.xlsx",
                           guess_max = 20000, col_names = TRUE) %>%
  filter(registration_id %notin% reg_id$id)

write.xlsx(registration_cleaning, "Data/Raw Excel Exports/REGISTRATION.xlsx")


#remove passports
pp_id <- change_log %>% filter(type == "PP ID") %>% select(id)

pp_cleaning <- read_excel("Data/Raw Excel Exports/PATIENT_PASSPORT.xlsx",
                                    guess_max = 20000, col_names = TRUE) %>%
  filter(patient_passport_id %notin% pp_id$id)

write.xlsx(pp_cleaning, "Data/Raw Excel Exports/PATIENT_PASSPORT.xlsx")


#remove ltbis
ltbi_id <- change_log %>% filter(type == "LTBI ID") %>% select(id)

ltbi_cleaning <- read_excel("Data/Raw Excel Exports/LTBI_TREATMENT.xlsx",
                          guess_max = 20000, col_names = TRUE) %>%
  filter(ltbi_treatment_id %notin% ltbi_id$id)

write.xlsx(ltbi_cleaning, "Data/Raw Excel Exports/LTBI_TREATMENT.xlsx")


# ADD WORKING PP AND REG DATA TO FLATFILE

tbfc_data <- read_excel("Data/flatfile.xlsx",
                                   guess_max = 20000, col_names = TRUE)  %>%
  replace_with_na_if(.predicate = is.Date,
                       condition = ~.x < as_date("1930-01-01")) %>%
  mutate(tst_result = case_when(tst_result < 0 ~ NA,
                                .default = tst_result)) %>%
  left_join(working_pp %>% group_by(patient_passport_id) %>% arrange(desc(tst_date_read)) %>% slice(1) %>% ungroup(), by="patient_passport_id") %>%
  left_join(working_reg, by=c("registration_id","last_name")) %>%
  mutate(onsite_id = coalesce(onsite_id.y,onsite_id.x),
         date_of_birth = coalesce(date_of_birth.y,date_of_birth.x),
         sex = coalesce(sex.y,sex.x),
         village = coalesce(village.y,village.x),
         date_of_visit1 = coalesce(date_of_visit1.y,date_of_visit1.x),
         tst_date_read = coalesce(tst_date_read.y,tst_date_read.x),
         tst_result = coalesce(tst_result.y,tst_result.x),
         weight = coalesce(weight.y,weight.x),
         height = coalesce(height.y,height.x),
         a1c = coalesce(a1c.y,a1c.x),
         hd_prevention = coalesce(hd_prevention.y,hd_prevention.x)  ) %>%
  rename(first_name = first_name.x, state = state.x, municipality = municipality.x) %>%
  select(all_of(names))

write.xlsx(tbfc_data, "Data/flatfile.xlsx")

###BIRTHDAYS

#examine all persons missing a birthday
bday_miss <- read_excel("Data/flatfile_clean.xlsx",
                          guess_max = 20000, col_names = TRUE) %>%
  filter(is.na(date_of_birth))
  