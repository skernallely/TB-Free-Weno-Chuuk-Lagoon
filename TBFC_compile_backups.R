## TB-Free Chuuk R code
## Compile HQ backups

#PACKAGES
library(tidyverse)#pipes, etc
library(readxl) #reading excel
library(openxlsx) #write to excel
library(lubridate) #deal with dates
library(stringr) #deal with strings
library(vtable) #allows sumtable
library(janitor) #cleannames
library(RODBC) # connection to ACCB
library(eHDPrep) #quality of data checks
library(naniar) #clean up and replace blanks,etc with NAs
 
#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

#list of all registration databases in HQ backups from 2023
files <- list.files(path="HQ Backups", 
                    pattern="registration_2023", 
                    full.names=TRUE, recursive=FALSE)

# #list raw and builder tables
# raw_tables <- c("raw_reg","raw_pp",
#                 "builder_reg","builder_pp")

raw_tables <- c("raw_reg","raw_pp")
                   
#list raw file origins
raw_files <-c("Data/Raw Excel Exports/REGISTRATION.xlsx",
                 "Data/Raw Excel Exports/PATIENT_PASSPORT.xlsx")

# write out all the offending strings
na_strings <- c(common_na_strings,"MISSING")

#base excel sheets
raw_excel_read <-function(x,y,z){
  x<-read_excel(y,guess_max = 20000, col_names = TRUE) %>%
    modify_if(is.POSIXt, as_date) %>%
    z()
}

#read in tables from obdc
read_in_function <- function(x,y,z){ 
  sqlFetch(x, y)   %>%
    z() %>%
    modify_if(is.POSIXt, as_date)
}

#shorten registration table to essential elements
short_reg <- function(x){ 
  x %>% ungroup() %>% 
    select(registration_id,registration_no,onsite_id,last_name,first_name,
           date_of_birth,sex,state,village,municipality,date_updated) %>% 
    mutate(registration_no = ifelse(str_length(registration_no) > 7, registration_no,
                                    sprintf("%07d", as.numeric(registration_no)))
    )
}

short_pp <- function(x){ 
  x %>% ungroup() %>% 
  select("registration_id", "patient_passport_id", "date_updated", "date_of_visit1", 
         "tst_date_read", "tst_result", "weight", "height", "a1c", "hd_prevention")
}



##Compile all HQ backups
#add password when running
for(file in files){
  #make current file link to accdb
  current_db <- odbcConnectAccess2007(file, pwd="")
  
  reg_table <- read_in_function(current_db,"REGISTRATION",short_reg) %>%
    filter(date_updated  %in% seq(max(date_updated)-3,max(date_updated), by="days")) %>%
    replace_with_na_if(.predicate = is.character,
                       condition = ~.x %in% na_strings) %>%
    replace_with_na_if(.predicate = is.Date,
                       condition = ~.x < as_date("1930-01-01"))
  
  #load in current accdb tables 
  write.xlsx(reg_table,
             paste0("HQ Backups/REGISTRATION/",str_replace(file, "^[^/]+/([^.]+)\\..*", "\\1"),".xlsx"))
  
  pp_table <- read_in_function(current_db,"PATIENT_PASSPORT",short_pp) %>%
    filter(date_updated  %in% seq(max(date_updated)-3,max(date_updated), by="days")) %>%
    replace_with_na_if(.predicate = is.character,
                       condition = ~.x %in% na_strings) %>%
    replace_with_na_if(.predicate = is.Date,
                       condition = ~.x < as_date("1930-01-01"))
  
  write.xlsx(pp_table,
             paste0("HQ Backups/PASSPORT/",str_replace(file, "^[^/]+/([^.]+)\\..*", "\\1"),".xlsx"))

  odbcClose(current_db)
}

