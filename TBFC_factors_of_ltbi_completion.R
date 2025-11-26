## TB-Free Chuuk R code
## LTBI Completion


#PACKAGES
library(tidyverse) 
library(readxl)
library(openxlsx)
library(lubridate)
library(stringr)
library(vtable) #allows sumtable
library(janitor)
library(scales)
library(naniar) #clean up and replace blanks,etc with NAs
library(table1) #make table 1111



#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

###
#FUNCTIONS
###



###
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


#load in clean flatfile
ltbi <- read_excel("Data/tbfc_analysis_dataset.xlsx",
                             guess_max = 20000, col_names = TRUE) %>%
  filter(ltbi_diagnosis == 1) %>%
  mutate(age_group = case_when(age <= 9 ~ "0-9",
                               age <= 19 & age > 9 ~ "10-19",
                               age <= 39 & age > 19 ~ "20-39",
                               age <= 59 & age > 39 ~ "40-59",
                               age > 59 ~ "60+"),
         age_group = factor(age_group,
                            levels=c("0-9","10-19","20-39","40-59","60+")),
         region = case_when(region == "NW" | region == "MORT" ~ "NORTHERN NAMONEAS",
                            .default = region),
         completed_yn = case_when(epi_status == "Completed treatment" | 
                                    epi_status == "Treatment completion date is after latest allowable completion - check dates"
                                  ~ "Completed",
                                  .default = "Did not complete"),
         area = case_when(region == "NORTHERN NAMONEAS" ~ "Weno",
                          region == "SOUTHERN NAMONEAS" | 
                            region == "FAICHUUK" ~ "Lagoon")
  )


## LTBI completion by region and age group

table1(~ factor(age_group) 
       | area, 
       data=ltbi %>% filter(completed_yn == "Completed"),
       overall=F, 
       extra.col=list(`P-value`=pvalue))

#chi square of completion by age group
ltbi %>%
  tbl_summary(by = area, include = c(completed_yn),
              digits = ~ 1) %>%
  add_p()

age_region_ltbi <- ltbi %>%
  group_by(age_group, area) %>%
  summarise(
    no_ltbi = n(),
    no_complete = sum(completed_yn == "Completed")
  ) %>%
  mutate(
    pct_complete = no_complete/no_ltbi
  ) %>%
  pivot_wider(id_cols = c("age_group"), names_from="area", values_from="pct_complete")
  

rm(ltbi,age_region_ltbi)


##ltbi completion based on the mechanism of referral
##how many people referred to case conference got ltbi?
ltbi |>
  summarise(count = sum(refer_to_conference))
##what proportion completed treatment, compared to non-case conference
ltbi |>
  tabyl(refer_to_conference,completed_yn) |>
  adorn_percentages() |>
  adorn_pct_formatting() |>
  adorn_ns()
