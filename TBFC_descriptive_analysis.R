## TB-Free Chuuk R code
## Descriptive Analysis

#PACKAGES
library(tidyverse) #pipes
library(readxl) #excel load-in
library(openxlsx) #excel load-in
library(lubridate) #dealing with dates
library(stringr) #dealing with strings
library(vtable) #allows sumtable
library(janitor) #allows tabyl & cleaning names
library(scales) #percent
library(table1) #make table 1111
library(naniar)



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

#table of demographic characteristics for TBFC
##TABLE 1
table1(~ age + factor(sex) + factor(region) + bmi+ factor(current_smoker) + factor(known_tb_exposure) +
         factor(prior_tb) + factor(al_one_symptom) + factor(abnormal_xray),
       render.continuous = render.NEW,
       render.categorical = \(x)  c("", sapply(stats.apply.rounding(stats.default(x)), 
                                               function(y) with(y,sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT)))), 
       overall=c(left="Total"),
       data=screened)

#tst result by tb classification
table1(~ factor(tst_result_10) + factor(hd_prev_given) + factor(ltbi_tx_started)
       | tb_classification, 
       render.categorical = \(x)  c("", sapply(stats.apply.rounding(stats.default(x)), 
                                               function(y) with(y,sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT)))),
       data=screened,
       overall=c(left="Total"))

#bmi and a1c by tb classification
table1(~ factor(bmi_cat) + a1c
       | tb_classification, 
       data=screened %>% filter(age >= 18),
       overall=c(left="Total"))

##LTBI FACTORS
#associated factors with an LTBI diagnosis
table1(~ factor(tst_result_10) + factor(known_tb_exposure) + factor(prior_tb) + 
         factor(hd_prev_given)
         + factor(bmi_cat) + a1c
       | tb_classification, 
       data=screened %>% filter(tb_classification != 'TB'),
       overall=F, 
       extra.col=list(`P-value`=pvalue))

#examine missing pattern for a1c
gg_miss_var(screened %>% select(a1c), show_pct = TRUE)

#count of pre-diabetes with a1c > 5.4 and <6.5
screened %>%
  filter(is.not.na(a1c)) %>%
  summarise(
    no_diabetes = sum(a1c<6.5 & a1c>5.4)
  )