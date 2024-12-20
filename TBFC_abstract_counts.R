## TB-Free Chuuk R code
## Numbers for abstract in order of appearance

#--------------------------
#WORKING DIRECTORY
setwd("~/PIHOA/TBFC/R Analysis/Weno_Chuuk_Lagoon")

#PACKAGES
library(readxl)
library(tidyverse)
library(janitor)

#FORMULAS
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

#--------------------------

##DATA
#load in clean flatfile
tbfc_analysis <- read_excel("Data/tbfc_analysis_dataset.xlsx",
                            guess_max = 20000, col_names = TRUE) %>%
  mutate(age_group = factor(age_group, 
                            levels=c("0-4","5-9","10-19","20-39","40-59","60+"))
  )

##COUNTS

##Abstract 
#total screened
tbfc_analysis %>%
  summarise(sum(screened_at_clinic))

#tst positivity
tbfc_analysis %>%
  filter(is.not.na(tst_result_10)) %>%
  tabyl(tst_result_10) %>%
  adorn_totals()

#ltbi diagnoses
tbfc_analysis %>%
  summarise(sum(ltbi_diagnosis))

#TB disease
tbfc_analysis %>%
  summarise(sum(active_tb_tx))

#LTBI treatment completion with 12 doses
tbfc_analysis %>%
  filter(ltbi_doses_completed >= 12) %>%
  summarise(n = n())

#Received single dose rifapentine
tbfc_analysis %>%
  filter(hd_prev_given ==1) %>%
  count()

#percent of ltbi completed 12 doses
tbfc_analysis %>%
  filter(ltbi_diagnosis == 1) %>%
  mutate(complete = case_when(ltbi_doses_completed >= 12 ~ 1,
                              .default = 0)) %>%
  tabyl(complete) %>%
  adorn_totals()

#percent of screened with any HD prevention (LTBI dose or HD prev)
tbfc_analysis %>%
  filter(screened_at_clinic == 1) %>%
  mutate(any_hd_prev = case_when(ltbi_doses_completed > 0 ~ 1,
                                 hd_prev_given == 1 ~ 1,
                                 .default = 0)) %>%
  tabyl(any_hd_prev) %>%
  adorn_totals()
