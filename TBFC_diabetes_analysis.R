## TB-Free Chuuk R code
## Diabetes Screening
## questions: what are the diabetes screening results

#PACKAGES
library(tidyverse) #pipes
library(readxl) #excel load-in
library(janitor) #allows tabyl & cleaning names

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

###
##DATA
###

#load in clean flatfile
diabetes_analysis <- read_excel("Data/tbfc_analysis_dataset.xlsx",
                            guess_max = 20000, col_names = TRUE) %>%
  mutate(tested_with_a1c = is.not.na(a1c)) %>%
  filter(age >= 18 & screened_at_clinic==1) #get only adults that were screened

#number of people 18+ tested with HbA1c for diabetes
diabetes_analysis %>%
  tabyl(tested_with_a1c) %>%
  adorn_totals()

#Of those with HBa1c result available, number with A1c >=6.5
diabetes_analysis %>%
  filter(is.not.na(a1c)) %>%
  tabyl(dm_a1c_result)

#Number of people who reported history of diabetes
diabetes_analysis %>%
  tabyl(history_diabetes)

#number of people with hx of diabetes using medication for the condition
diabetes_analysis %>%
  filter(history_diabetes == "Y") %>%
  tabyl(medications_for_diabetes)

#Number of people newly diagnosed with diabetes
diabetes_analysis %>%
  filter(dm_a1c_result == 1) %>%
  tabyl(new_dm_result)


##TB and Diabetes
diabetes_analysis |>
  filter(is.not.na(dm_a1c_result)) |>
  tabyl(tb_classification,dm_a1c_result) |>
  adorn_totals(c("row","col")) |>
  adorn_percentages() |>
  adorn_pct_formatting() |>
  adorn_ns()
  
