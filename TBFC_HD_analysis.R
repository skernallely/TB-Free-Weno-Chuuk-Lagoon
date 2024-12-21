## TB-Free Chuuk R code
## Hansens's Disease Screening
## questions: descriptive epi of hansen's referrals and cases
##            follow-up for HD cases in Chuuk 1 year on

#PACKAGES
library(tidyverse) #pipes
library(readxl) #excel load-in
library(vtable)
library(janitor)

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

###
##DATA
###

#load in clean flatfile
hd_analysis <- read_excel("Data/tbfc_analysis_dataset.xlsx",
                       guess_max = 20000, col_names = TRUE) %>%
  filter(screened_at_clinic == 1) %>%
  mutate(age_group = factor(age_group, 
                            levels=c("0-4","5-9","10-19","20-39","40-59","60+")),
         
         hd_prev_or_ltbi = case_when(ltbi_doses_completed > 0 ~ 1,
                                      hd_prev_given == 1 ~ 1,
                             .default = 0),
         
         hd_dose_type = case_when(ltbi_doses_completed > 0 ~ "3HP",
                                  hd_prev_given == 1 ~ "Single-dose"),
         
         hd_referral = case_when(is.not.na(hd_confirmed) ~ 1,
                                 .default = 0),
         hd_assessed = case_when(hd_confirmed != "Not Assessed" ~ 1,
                                 hd_confirmed == "Not Assessed" ~ 0)
  )

#number of people referred for HD screening (hd_referral==1)
hd_analysis %>%
  tabyl(hd_referral)

#number with assessment completed by HD program (hd_assessed==1)
hd_analysis %>%
  tabyl(hd_assessed)

#number of new cases of leprosy identified and HD case rate per 10k screened
hd_analysis %>%
  summarise(new_hd_cases = sum(hd_confirmed == "Leprosy",na.rm=T),
            new_hd_rate = new_hd_cases/sum(screened_at_clinic==1,na.rm=T)*10000)

#number of people who received single-dose rifapentine for HD prevention
hd_analysis %>%
  filter(age >= 2) %>%
  tabyl(hd_prev_given) %>%
  adorn_totals()

#number of people who received any kind of HD prevention
hd_analysis %>%
  filter(age >= 2) %>%
  tabyl(hd_prev_or_ltbi)
     
#--------------------

##HD cases in Chuuk post-screening
hd_cases_post_screening <- read_excel("Data/Leprosy Cases since TBFC Cohort.xlsx",
                                 guess_max = 20000, col_names = TRUE) %>%
  clean_names() %>%
  select(chk_case_id, tbfc_id, tbfc_status, date_diagnosed, mode_of_detection, classification, type) %>%
  rename(registration_no = tbfc_id) %>%
  left_join(tbfc_analysis, by="registration_no") %>%
  mutate(time_from_screening_to_diagnosis = as.numeric(difftime(date_diagnosed, 
                                                                date_screening, units = "days"))
  )

#hd cases post-screening by whether given hd prevention
hd_cases_post_screening %>%
  tabyl(tbfc_status,hd_prev_given) %>%
  adorn_totals(c("row","col"))

#hd cases post-screening by ltbi diagnosis
hd_cases_post_screening %>%
  tabyl(tbfc_status,ltbi_diagnosis) %>%
  adorn_totals(c("row","col"))

#hd cases post-screening missed by TBFC on-site screening
hd_cases_post_screening %>%
  filter(tbfc_status == "Screened but not referred" & hd_prev_given == 1) %>%
  summarise(mean(time_from_screening_to_diagnosis, na.rm =T))
