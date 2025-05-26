## TB-Free Chuuk R code
## Active TB Diagnosis and Outcomes

#PACKAGES
library(tidyverse) #pipes
library(readxl) #excel load-in
library(table1)
library(gtsummary) #allows summary tabyl and p-value
library(cowplot)
library(scales) #percents for graphs
library(janitor)

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
  filter(tb_classification == "TB") %>%
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

#number of active TB cases by route to diagnosis
screened %>% 
  filter(tb_classification == 'TB') %>%
  mutate(dm_a1c_result = case_when(is.na(a1c) ~ NA,
                                   a1c < 6.5 ~ "Normal",
                                   a1c >= 6.5 ~ "Diabetes"),
         al_one_symptom = case_when(al_one_symptom == 1 ~ "Symptomatic",
                                    al_one_symptom == 0 ~ "Asymptomatic",
                                    is.na(al_one_symptom) ~ NA),
         known_tb_exposure = case_when(known_tb_exposure == 1 ~ "Known-exposure",
                                       known_tb_exposure == 0 ~ "No-known-exposure",
                                       is.na(known_tb_exposure) ~ NA),
         prior_tb = case_when(prior_tb == 1 ~ "Prior-TB",
                              prior_tb == 0 ~ "No-prior-TB",
                              is.na(prior_tb) ~ NA),
         abnormal_xray = case_when(is.na(abnormal_xray) ~ NA,
                                   abnormal_xray == 1 ~ "Abnormal-xray",
                                   abnormal_xray == 0 ~ "Normal-xray"),
         id_method = paste(tst_result_10, 
                       dm_a1c_result, 
                       al_one_symptom, 
                       known_tb_exposure, 
                       prior_tb),
         routed_by_a1c = case_when(abnormal_xray != 1 &
                                     prior_tb != 1 &
                                     known_tb_exposure != 1 &
                                     al_one_symptom != 1 & 
                                     dm_a1c_result == "Diabetes" ~ 1),
         aided_by_a1c = case_when(dm_a1c_result == "Diabetes" ~ 1)) %>%
  tabyl(routed_by_a1c)

##Graph of screened TB outcomes by 10-year age groups
##stacked bar graph of  x-axis age groups with y-axis percent of TB cases treated
tb_outcomes_data <-
  screened %>%
  mutate(tb_outcome_clean = case_when(tb_outcome == "Complete" ~ "Completed treatment/Cured",
                                tb_outcome == "LTFU" ~ "Lost to follow-up",
                                tb_outcome == "Refused" ~ "Lost to follow-up",
                                tb_outcome == "Default" ~ "Lost to follow-up",
                                tb_outcome == "Transfer" ~ "Transferred out",
                                tb_outcome == "On Treatment" ~ "Currently treating",
                                tb_outcome == "Died" ~ "Died"),
         tb_outcome_clean = factor(tb_outcome_clean, levels = c("Completed treatment/Cured",
                                                                "Currently treating",
                                                                "Lost to follow-up")),
         age_group_10 = case_when(age < 10 ~ "0-9",
                                  age >= 10 & age < 20 ~ "10-19",
                                  age >= 20 & age < 30 ~ "20-29",
                                  age >= 30 & age < 40 ~ "30-39",
                                  age >= 40 & age < 50 ~ "40-49",
                                  age >= 50 & age < 60 ~ "50-59",
                                  age >= 60 ~ "60+")
  ) %>%
  filter(tb_classification == "TB") %>%
  tabyl(age_group_10, tb_outcome_clean) %>%
  adorn_totals("col") %>%
  mutate(labels = paste(age_group_10,paste0("n=",Total),sep="\n")) %>%
  mutate(labels = factor(labels, levels = labels)) %>%
  pivot_longer(cols=c(2:4), names_to="tb_outcome_clean", values_to = "num") %>%
  mutate(pct = num / Total) %>%
  filter(num > 0)
  

  
tb_outcomes_gg <-
  ggplot(data = tb_outcomes_data, aes(x=labels, y=pct, fill=fct_rev(tb_outcome_clean),
                                      label=percent(pct, accuracy = 0.1))) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(label = percent(pct, accuracy = 0.1)), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Completed treatment/Cured" = "#4f6e9f",
                               "Currently treating" = "#b6d3ff",
                               "Transferred out" = "#FE9D5D",
                               "Lost to follow-up" = "#FDDE86",
                               "Died" = "red"),
                    breaks=c("Completed treatment/Cured","Currently treating",
                             "Transferred out",
                             "Lost to follow-up","Died")) +
  scale_y_continuous(name="Percent of persons diagnosed with tuberculosis disease",
                     labels = percent)+
  labs(x="Age group (years)",
       fill = "Treatment outcome") +
  theme_classic() + 
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        legend.margin = margin(t = 0, r = 0, b = 0, l = -45, unit = "pt"),
        )

#Save bar chart of tb outcome by 10-year age groups
ggsave("Figures/Figure 3 - TB treatment outcomes by age group.png",
       plot = tb_outcomes_gg, 
       width = 1280, height = 1024, units = "px", scale = 2, dpi=300)
