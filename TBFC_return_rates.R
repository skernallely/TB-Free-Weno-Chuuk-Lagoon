## TB-Free Chuuk R code
## TST Return Rates

#PACKAGES
library(tidyverse) #pipes, scales, lubridate, stringr
library(readxl) #excel load-in
library(janitor) #allows tabyl & cleaning names
library(gtsummary) #allows summary tabyl and p-value

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

#datasets
#all patients registered
tbfc_analysis <- read_excel("Data/tbfc_analysis_dataset.xlsx",
                          guess_max = 20000, col_names = TRUE) %>%
  mutate(age_group_new = case_when(age_group == "5-9" | age_group == "10-19" ~ "5-19",
                                   .default = age_group),
         age_group = factor(age_group_new, 
                            levels=c("0-4","5-19","20-39","40-59","60+")),
         municipality = str_to_upper(municipality),
         village = str_to_upper(village),
         village = if_else(grepl("KUCHUWA",village), "KUCHUWA",
                           if_else(grepl("NANTAKU",village), "NEPUKOS",
                                   village)
         ),
         full_village_name = paste(municipality,village)
  )%>%
  mutate(area = case_when(region %in% c("FAICHUUK",
                                        "SOUTHERN NAMONEAS") ~ "LAGOON",
                          .default = "WENO")
  )


#clinic return data by age group and sex
tbfc_analysis %>%
  group_by() %>%
  tabyl(age_group, screened_at_clinic, sex) %>%
  # adorn_percentages()
  adorn_totals("col")

#clinic return data by age group and sex
tbfc_analysis %>%
  group_by() %>%
  tabyl(area, screened_at_clinic, sex) %>%
  # adorn_percentages()
  adorn_totals("col")


#chi square of screened at clinic by sex
tbfc_analysis %>%
  tbl_summary(by = sex, include = c(screened_at_clinic),
              digits = ~ 1) %>%
  add_p()

#chi square of screened at clinic by age group
tbfc_analysis %>%
  filter(sex == "F") %>%
  tbl_summary(by = age_group, include = c(screened_at_clinic),
              digits = ~ 1) %>%
  add_p()

#chi square of screened at clinic by lagoon vs weno
tbfc_analysis %>%
  group_by() %>%
  tbl_summary(by = area, include = c(screened_at_clinic),
              digits = ~ 1) %>%
  add_p()
