## TB-Free Chuuk R code
## Hansens's Disease Screening
## questions: descriptive epi of hansen's referrals and cases

#PACKAGES
library(tidyverse) #pipes
library(readxl) #excel load-in
library(openxlsx) #excel load-in
library(lubridate) #dealing with dates
library(stringr) #dealing with strings
library(vtable) #allows sumtable
library(janitor) #allows tabyl & cleaning names
library(ggplot2) #make graphs
library(ggpubr) #special aggregate of plots
library(ggthemes) #makes prettier graphs
library(gridExtra) #tiled grid of plots
library(scales) #percent
library(epiR) # sens and spec calculations
library(pROC) # make ROC and AUC calculations
library(rstatix) # pipe-friendly stats tests
library(table1) #make table 1111

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)## TB-Free Chuuk R code

###PVALUE CALCULATOR
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}


render.NEW <- function(x, name, data2, ...) {
  MIN <- min(x, na.rm = T)
  MAX <- max(x, na.rm = T)
  median <- median(x, na.rm = T)
  Q1 <- quantile(x, 0.25, na.rm = T)
  Q3 <- quantile(x, 0.75, na.rm = T)
  N = length(x) - sum(is.na(x))
  
  out <- c("",
           "[min, max]" = paste0("[", sprintf("%.2f", MIN), ", ", sprintf("%.2f", MAX), "]"),
           "Median [Q1, Q3]" = paste0(sprintf("%.2f", median), " [", sprintf("%.2f", Q1), ", ", sprintf("%.2f", Q3), "]"),
           "N" = N)
  out
}

###
##DATA
###

#load in clean flatfile
hd_prevention <- read_excel("Data/flatfile_clean.xlsx",
                       guess_max = 20000, col_names = TRUE) %>%
  mutate(
    screened_at_clinic = if_else(is.not.na(tst_date_read) |
                                   is.not.na(active_tb) |
                                   is.not.na(result_hd_assessment) |
                                   is.not.na(weight) | 
                                   is.not.na(treatment_status) |
                                   is.not.na(outcome_case_conference) |
                                   is.not.na(date_screening) | 
                                   is.not.na(tb_outcome),
                                 1, 0,
                                 missing = 0)) %>%
  filter(screened_at_clinic == 1) %>%
  mutate(age_group = factor(age_group, 
                            levels=c("0-4","5-9","10-19","20-39","40-59","60+")),
         
         hd_prev_and_ltbi = case_when(doses_completed > 0 ~ 1,
                             hd_prevention == "Y" ~ 1,
                             .default = 0),
         
         hd_dose_type = case_when(hd_prevention == "Y" ~ "Single-dose",
                                  doses_completed > 0 ~ "3HP"),
  ) %>%
  filter(hd_prev_and_ltbi == 1) %>%
    select(registration_id, registration_no, age, age_group, sex, municipality,
           village, date_screening, weight, height, failure_to_thrive, hd_exposure,
           hd_exposure_name, skin_lesions, result_hd_assessment, hd_prevention,
           hd_prevention_text, ltbi_treatment_id, ltbi_tx_started, medication_order_full,
           doses_completed,epi_status,hd_program_assessment_date,hd_program_assesment_completed,
           hd_program_assesment_result, screened_at_clinic,ltbi_diagnosis,active_tb_tx,
           hd_further_assessment, hd_confirmed, hd_prev_given,hd_dose_type)
# 
# table1(~ age + factor(sex) + factor(region) + bmi+ factor(current_smoker) + factor(prior_tb) +
#          factor(known_tb_exposure) + factor(al_one_symptom) + factor(abnormal_xray),
#        render.continuous = render.NEW,
#        render.categorical = \(x)  c("", sapply(stats.apply.rounding(stats.default(x)), 
#                                                function(y) with(y,sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT)))), 
#        overall=c(left="Total"),
#        data=screened)

sumtable(hd_prevention, digits=3)


median(hd_prevention$age, na.rm=T)

     
     
hd_cases_post_screening <- read_excel("Data/Leprosy Cases since TBFC Cohort.xlsx",
                                 guess_max = 20000, col_names = TRUE) %>%
  clean_names() %>%
  select(chk_case_id, tbfc_id, tbfc_status, date_diagnosed, mode_of_detection, classification, type) %>%
  rename(registration_no = tbfc_id) %>%
  left_join(tbfc_analysis, by="registration_no") %>%
  mutate(time_from_screening_to_diagnosis = as.numeric(difftime(date_diagnosed, 
                                                                date_screening, units = "days"))
  )

# names(hd_cases_post_screening)

hd_cases_post_screening %>%
  tabyl(tbfc_status,hd_prev_given) %>%
  adorn_totals(c("row","col"))

hd_cases_post_screening %>%
  tabyl(tbfc_status,ltbi_diagnosis) %>%
  adorn_totals(c("row","col"))

hd_cases_post_screening %>%
  filter(tbfc_status == "Screened but not referred" & hd_prev_given == 1) %>%
  summarise(mean(time_from_screening_to_diagnosis, na.rm =T))
