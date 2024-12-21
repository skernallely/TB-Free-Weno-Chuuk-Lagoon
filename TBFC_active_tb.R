## TB-Free Chuuk R code
## Active TB Diagnosis and Outcomes

#PACKAGES
library(tidyverse) #pipes
library(readxl) #excel load-in
library(table1)
library(gtsummary) #allows summary tabyl and p-value


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

##DATA
###

#load in clean flatfile
screened <- read_excel("Data/tbfc_analysis_dataset.xlsx",
                       guess_max = 20000, col_names = TRUE) %>%
  filter(screened_at_clinic == 1) %>%
  mutate(age_group = factor(age_group, 
                            levels=c("0-4","5-9","10-19","20-39","40-59","60+")),
         
         region = case_when(region == "NW" | region == "MORT" ~ "NORTHERN NAMONEAS",
                            .default = region)
  )

#Number of possible cases of TB referred
screened %>% 
  summarise(n= sum(refer_to_conference))

#number of active TB cases diagnosed
screened %>% 
  summarise(n= sum(active_tb_tx == 1))

#rate of active TB cases diagnosed per 100k screened
screened %>% 
  summarise(rate= sum(active_tb_tx == 1) / n() * 100000)

#number of active TB cases that were lab confirmed
screened %>% 
  summarise(n= sum(lab_confirmed, na.rm=T))

#number needed to screen to diagnose one active TB case
screened %>% 
  summarise(n= n() / sum(active_tb_tx == 1))

#descriptive factors for TB cases
screened %>% 
  filter(tb_classification == "TB") %>%
  tbl_summary(include = c(prior_tb, known_tb_exposure, al_one_symptom,
                          abnormal_xray, lab_confirmed,
                          current_smoker, a1c,
                          bmi_cat),
              digits = ~ 1)

#number who completed TB treatment
screened %>% 
  summarise(n= sum(tb_outcome == "Complete", na.rm=T))

##TB FACTORS
#associated factors with an active TB diagnosis
table1(~ factor(tst_result_10) + factor(known_tb_exposure) + factor(prior_tb) + 
         factor(al_one_symptom) + factor(current_smoker) +
         factor(bmi_cat) + a1c
       | tb_classification, 
       data=screened %>% filter(tb_classification != 'LTBI'),
       overall=F, 
       extra.col=list(`P-value`=pvalue))
