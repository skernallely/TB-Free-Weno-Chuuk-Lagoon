## TB-Free Chuuk R code
## AI X-ray Analysis
## questions: match AI dataset to flatfile, sens/spec of AI, ROC

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

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

make_roc_graph <- function(data) {
  ggroc(data) +
    labs(
      subtitle = "Assuming case conference outcome of active/current/possible TB is true disease",
      x = "Specificity (%)",
      y = "Sensitivity (%)") +  # title and caption
    theme_classic() +
    theme(panel.background = element_blank(), 
          panel.border = element_blank(),
          legend.title = element_blank(),
          legend.position="bottom",
          plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
    scale_color_brewer(palette="Set1") + 
    scale_x_reverse(labels = percent_format()) +
    scale_y_continuous(labels = percent_format())
}

#datasets

ai_data <- read_excel("Data/Fuji X-ray data Oct 8.xlsx", 
                      col_names = TRUE) %>%
  clean_names() %>%
  mutate(registration_number = as.character(parse_number(gsub("\\-[0-9]","",patient_id)))) %>%
  mutate(registration_no = ifelse(str_length(registration_number) > 7, registration_number,
                                  sprintf("%07d", as.numeric(registration_number)))) %>%
  group_by(registration_no)

#overall ai_data summary stats
sumtable(ai_data) 


### SINGLE PERSON DATASET FOR MATCHING

ai_analysis_data <- read_excel("Data/Fuji X-ray data Oct 8.xlsx", 
                               col_names = TRUE) %>%
  clean_names() %>%
  filter(tb_ai_impression != "Invalid Xray") %>%
  mutate(patient_id = str_remove(patient_id, "[\\s_]+"),
         registration_no = case_when(str_length(patient_id) > 7 ~ patient_id,
                                     grepl("[a-zA-Z]",patient_id) ~ patient_id,
                                     .default = sprintf("%07d", as.numeric(patient_id)))
  ) %>%
  group_by(registration_no) %>%
  arrange(desc(tuberculosis),desc(abnormal)) %>%
  slice(1) %>%
  select(registration_no, tb_ai_impression, ai_score, report, x_ray_analysis, abnormal, tuberculosis) %>%
  filter(is.not.na(registration_no) & registration_no != "     NA") %>%
  left_join(read_excel("Data/flatfile.xlsx",
                       guess_max = 20000, col_names = TRUE) %>%
              select(registration_no,
                     xray_result_preliminary,active_tb,ltbi_actions,tb_sputum,result_xpert,
                     xray_conference_result,outcome_case_conference,actions_not_active_tb,
                     notes_case_conference, ltbi_diagnosis,
                     age, age_group, sex, municipality, village, tst_pos,a1c,weight,height) %>%
              mutate(matched = 1),
            by = "registration_no") %>%
  arrange(desc(weight)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(ai_xray_result = case_when(
    tuberculosis == TRUE ~ "Abnormal - Active TB",
    abnormal == TRUE ~ "Abnormal - Other",
    x_ray_analysis == "Normal" ~ "Normal",
    .default = "Normal"),
    
    human_xray_result_cc =  case_when(
      xray_conference_result == "Likely TB" ~ "Abnormal - Active TB",
      xray_conference_result == "Possible TB" ~ "Abnormal - Active TB",
      xray_conference_result == "Unlikely TB - old/inactive TB" ~ "Abnormal - Other",
      xray_conference_result == "Unlikely TB - appears normal" ~ "Normal",
      xray_conference_result == "Unlikely TB - other pathology" ~ "Abnormal - Other",
      notes_case_conference == "OLD TB CASE 2022 - TREATED" ~ "Abnormal - Other",
      outcome_case_conference == "Not TB" ~ "Normal",
      .default = NA),
    
    human_cc_cat =  case_when(
      xray_conference_result == "Likely TB" ~ "Likely TB",
      xray_conference_result == "Possible TB" ~ "Possible TB",
      grepl("Unlikely",xray_conference_result) ~ "Unlikely TB",
      outcome_case_conference == "Not TB" ~ "Unlikely TB",
      .default = NA) %>% 
      factor(c("Likely TB", "Possible TB", "Unlikely TB"),ordered = T),
    
    human_xray_result_prelim = case_when(
      xray_result_preliminary == "Likely TB" ~ "Abnormal - Active TB", 
      xray_result_preliminary == "Possible TB" ~ "Abnormal - Active TB",
      xray_result_preliminary == "Unlikely TB - appears normal" ~ "Normal",
      xray_result_preliminary == "Unlikely TB - other pathology" ~ "Abnormal - Other",
      xray_result_preliminary == "Unlikely TB - old/inactive TB" ~ "Abnormal - Other",
      active_tb == "Unlikely/Negative for TB" ~ "Normal",
      .default = "Normal"),
    
    human_prelim_cat =  case_when(
      xray_result_preliminary == "Likely TB" ~ "Likely TB",
      xray_result_preliminary == "Possible TB" ~ "Possible TB",
      grepl("Unlikely",xray_result_preliminary) ~ "Unlikely TB",
      grepl("Unlikely",active_tb) ~ "Unlikely TB",
      active_tb == "Unlikely/Negative for TB" ~ "Unlikely TB",
      .default = "Unlikely TB") %>% 
      factor(c("Likely TB", "Possible TB", "Unlikely TB"),ordered = T),
    
    ai_bin = case_when(
      ai_xray_result == "Abnormal - Active TB" ~ 1,
      is.not.na(ai_xray_result) ~ 0,
      .default = 0),
    
    human_bin_cc = case_when(
      human_xray_result_cc == "Abnormal - Active TB" ~ 1,
      is.not.na(human_xray_result_cc) ~ 0),
    
    human_bin_prelim = case_when(
      human_xray_result_prelim == "Abnormal - Active TB" ~ 1,
      is.not.na(human_xray_result_prelim) ~ 0),
    
    cc_bin = case_when(
      outcome_case_conference == "Active TB" ~ 1,
      outcome_case_conference == "Current TB - on active tx" ~ 1,
      outcome_case_conference == "Possible TB - further workup now" ~ 1,
      .default = NA) %>% 
      factor(levels = c(1,0), labels = c("TB", "Not TB")),
    
    ai_score = as.numeric(ai_score),
    
    ai_score_group = case_when(ai_score < 0.1 ~ "0.00-0.09",
                               ai_score < 0.2 & ai_score >= 0.1 ~ "0.10-0.19",
                               ai_score < 0.3 & ai_score >= 0.2 ~ "0.20-0.29",
                               ai_score < 0.4 & ai_score >= 0.3 ~ "0.30-0.39",
                               ai_score < 0.5 & ai_score >= 0.4 ~ "0.40-0.49",
                               ai_score < 0.6 & ai_score >= 0.5 ~ "0.50-0.59",
                               ai_score < 0.7 & ai_score >= 0.6 ~ "0.60-0.69",
                               ai_score < 0.8 & ai_score >= 0.7 ~ "0.70-0.79",
                               ai_score < 0.9 & ai_score >= 0.8 ~ "0.80-0.89",
                               ai_score >= 0.9 ~ "0.90-1.00") %>%
      factor(levels=c("0.00-0.09","0.10-0.19","0.20-0.29","0.30-0.39",
                      "0.40-0.49","0.50-0.59","0.60-0.69","0.70-0.79",
                      "0.80-0.89","0.90-1.00"), ordered = T),
    
    followup_bins = case_when(
      
      cc_bin == "TB" ~ "Treated for TB",
      
      ltbi_diagnosis == 1 ~ "LTBI",
      
      xray_conference_result == "Unlikely TB - old/inactive TB" ~ "Old TB",
      xray_result_preliminary == "Unlikely TB - old/inactive TB" ~ "Old TB",
      notes_case_conference == "OLD TB CASE 2022 - TREATED" ~ "Old TB",
      
      .default = "No Treatment or Follow-up"
    ) %>% 
      factor(c("Treated for TB", "LTBI", "Old TB", "No Treatment or Follow-up"),ordered = T)
  )

sumtable(ai_analysis_data)


#sample size to determine sens and spec of a test

## Our example:
## The AI reading of Xrays is new and we'd like to conduct a study
## to determine its diagnostic sensitivity and specificity which we believe should be in the
## order of 0.91 and .65, respectively. How many subjects should be enrolled if the prevalence of
## the disease outcome of interest is 0.00742 and we'd like to be 95% confident
## that our estimate of sensitivity is within 0.07 of the true population
## value?
## SENS AND SPEC ESTIMATES FROM: Zhan Y, et al. doi:10.3390/jcm12010303

epi.ssdxsesp(test = 0.91, type = "se", Py = 0.00742, epsilon = 0.07,
             error = "absolute", nfractional = FALSE, conf.level = 0.95)

epi.ssdxsesp(test = 0.65, type = "sp", Py = 0.00742, epsilon = 0.07,
             error = "absolute", nfractional = FALSE, conf.level = 0.95)

## A total of 8654 subjects need to be enrolled to meet the requirements of the
## study for sensitivity and 180 for specificity.

## in the absence of a gold standard
# 1st pop = ai, 2nd pop = all xrayed
# 3555 people got an xray in the 2nd pop and 1121 in matched sample for ai 1st pop = 0.315 ratio
# SENS AND SPEC FOR HUMAN XRAY FROM: World Health Organization. TB Guidelines. 2014
# human cxr read --> sens = .98 and spec = .75

epi.ssdxtest(pi = c(0.044603033,0.04192685), se = c(0.91,0.98), sp = c(0.65,0.75),
             epsilon.api = c(0.05,0.05), epsilon.ase = c(0.05,0.05),
             epsilon.asp = c(0.05,0.05), epsilon.asesp = c(0.05,0.05),
             r = 0.315, nfractional = FALSE, verbose = FALSE, conf.level = 0.95)

###BASIC HISTOGRAMS

##Distribution of AI score by outcome
ai_analysis_data %>%
  #filter(cc_bin == "Not TB") %>%
  shapiro_test(ai_score)

# W = 0.54884, p-value < 2.2e-16 | not normally distributed

#difference by group in the following groups

#groups: cc_bin, ai_xray_result, human_xray_result_prelim, human_xray_result_cc

ai_analysis_data %>% 
  kruskal_test(ai_score ~ human_xray_result_cc)

ai_analysis_data %>% 
  select(ai_score, human_xray_result_cc) %>%
  st(group = 'human_xray_result_cc', 
     summ = c('notNA(x)', 'mean(x)', 'median(x)', 'pctile(x)[25]', 'pctile(x)[75]'),
     summ.names = c('N','Mean', 'Median','Pctl.25','Pctl.75'),
     out='viewer',
     title='',
     digits=3
  )



#make histo and density graphs

histo_ai_score <- ai_analysis_data %>%
  ggplot( mapping = aes(x=ai_score, fill = cc_bin)) +
  geom_histogram(binwidth = 0.085) +
  facet_wrap(~cc_bin, scales = "free") +
  labs(
    title = "Frequency of case conference outcomes by AI score assigned",
    x = "AI Score",
    y = "No. of patients") +  # title and caption
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position="bottom",
        plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
  scale_fill_brewer(palette="Set1")

density_ai_score <- ai_analysis_data %>%
  ggplot(mapping = aes(x = ai_score, y = after_stat(density), fill = cc_bin)) +
  geom_density(size = 1, alpha = 0.5)+
  facet_wrap(~cc_bin)+
  labs(
    title = "Proportion of case conference outcomes by AI score assigned",
    x = "AI Score",
    y = "% of patients") +  # title and caption
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position="bottom",
        plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
  scale_fill_brewer(palette="Set1")


ggsave(plot=ggarrange(
  histo_ai_score,
  density_ai_score,
  heights = c(1,1),
  font.label= list("plain","black",size=12),
  common.legend = TRUE,
  label.x = 0,
  legend = "none",
  nrow = 2),
  "Figures/Frequency of Case Conference Outcomes by AI Score.jpg",
  width = 1280, height = 1280, units = "px", scale = 3, dpi=300)



## SENS AND SPEC FOR AI TESTING VS. CASE CONF OUTCOME

ai_cc_tb_tbt <- ai_analysis_data %>%
  mutate_at(c('ai_bin', 'human_bin_cc', 'human_bin_prelim','cc_bin'), ~replace_na(., 0)) %>%
  tabyl(ai_bin,cc_bin) %>%
  rename(tes = ai_bin) %>%
  pivot_longer(cols=2:3, names_to="dis", values_to = "n") %>%
  mutate(dis = factor(dis, levels = c(1,0), labels = c("Dis+","Dis-")),
         tes = factor(tes, levels = c(1,0), labels = c("Test+","Test-")),
         n = as.integer(n)) %>%
  group_by(tes, dis)

rval_ai_human <- epi.tests(ai_cc_tb_tbt, method = "exact", digits = 2, 
                           conf.level = 0.95)
summary(rval_ai_human)
## Test sensitivity is 0.340  (95% CI 0.212 to 0.488). Test specificity is
## 0.969 (95% CI 0.957 to 0.979). The likelihood ratio of a positive test
## is 11.03 (95% CI 6.61  to 18.41). The diagnostic accuracy was 0.941 
## (95% CI 0.926  to 0.954)


prelim_cc_tb_tbt <- ai_analysis_data %>%
  mutate_at(c('ai_bin', 'human_bin_cc', 'human_bin_prelim','cc_bin'), ~replace_na(., 0)) %>%
  tabyl(human_bin_prelim,cc_bin) %>%
  rename(tes = human_bin_prelim) %>%
  pivot_longer(cols=2:3, names_to="dis", values_to = "n") %>%
  mutate(dis = factor(dis, levels = c(1,0), labels = c("Dis+","Dis-")),
         tes = factor(tes, levels = c(1,0), labels = c("Test+","Test-")),
         n = as.integer(n)) %>%
  group_by(tes, dis)

rval_ai_human <- epi.tests(prelim_cc_tb_tbt, method = "exact", digits = 2, 
                           conf.level = 0.95)
summary(rval_ai_human)
## Test sensitivity is 0.780  (95% CI 0.640 to 0.885). Test specificity is
## 0.961 (95% CI 0.947 to 0.972). The likelihood ratio of a positive test
## is 19.89 (95% CI 14.29  to 27.69). The diagnostic accuracy was 0.953 
## (95% CI 0.939  to 0.*964)


cc_cc_tb_tbt <- ai_analysis_data %>%
  mutate_at(c('ai_bin', 'human_bin_cc', 'human_bin_prelim','cc_bin'), ~replace_na(., 0)) %>%
  tabyl(human_bin_cc,cc_bin) %>%
  rename(tes = human_bin_cc) %>%
  pivot_longer(cols=2:3, names_to="dis", values_to = "n") %>%
  mutate(dis = factor(dis, levels = c(1,0), labels = c("Dis+","Dis-")),
         tes = factor(tes, levels = c(1,0), labels = c("Test+","Test-")),
         n = as.integer(n)) %>%
  group_by(tes, dis)

rval_ai_human <- epi.tests(cc_cc_tb_tbt, method = "exact", digits = 2, 
                           conf.level = 0.95)
summary(rval_ai_human)
## Test sensitivity is 0.920  (95% CI 0.808 to 0.978). Test specificity is
## 0.999 (95% CI 0.948 to 0.999). The diagnostic accuracy was 0.996 
## (95% CI 0.990 to 0.999)

#ROC / AUC ANALYSIS
# measures the performance of a classifier and is frequently applied for method comparison.
## A higher AUC means a better classification.
# can use partial AUC if interested in how well test does in certain range of spec/sens

#AI READ
ai_roc <- ai_analysis_data %>%
  mutate_at(c('ai_bin', 'human_bin_cc',  'human_bin_prelim'), ~replace_na(., 0)) %>%
  roc(response=cc_bin, predictor=ai_bin, 
      #partial.auc = c(100, 80), partial.auc.correct = TRUE, percent = TRUE
  )

#HUMAN PRELIM READ
prelim_roc <- ai_analysis_data %>%
  mutate_at(c('ai_bin', 'human_bin_cc',  'human_bin_prelim'), ~replace_na(., 0)) %>%
  roc(response=cc_bin, predictor=human_bin_prelim, 
      #partial.auc = c(100, 80), partial.auc.correct = TRUE, percent = TRUE
  )

#HUMAN CC READ
cc_roc <- ai_analysis_data %>%
  mutate_at(c('ai_bin', 'human_bin_cc',  'human_bin_prelim'), ~replace_na(., 0)) %>%
  roc(response=cc_bin, predictor=human_bin_cc)

#AI SCORE
ai_score_roc <- ai_analysis_data %>%
  roc(response=cc_bin, predictor=ai_score, 
      #partial.auc = c(100, 90), partial.auc.correct = TRUE, percent = TRUE
  )

(make_roc_graph(list("AI Score"=ai_score_roc, "AI Read"=ai_roc, "Preliminary Human Read"=prelim_roc)) + 
    labs(title = "AI Score and Read Result vs. Preliminary Human Read ROC") ) %>%
  ggsave("Figures/Sensitivity and Specificity of AI vs Human Reads.png", .,
         width = 720, height = 720, units = "px", scale = 3.5, dpi=300)


## BOX PLOTS FOR AI SCORE BY CATEGORY

box_ai_score <- ai_analysis_data %>%
  ggplot( mapping = aes(y=ai_score, x=followup_bins)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of AI score by follow-up category for patients (n=1511) 
    whose X-ray was read by AI tool, TB-Free Chuuk 2023 ",
    x = "Follow-up Category",
    y = "AI Score") +  # title and caption
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position="bottom",
        plot.margin = unit(c(1,2,1,2), "cm"))

plot(box_ai_score)


ggsave(plot=box_ai_score,
       "Figures/Boxplot of AI score by follow-up category.jpg",
       width = 1200, height = 828, units = "px", scale = 2, dpi=300)


ai_analysis_data %>%
  tabyl(ai_score_group,followup_bins)
