## TB-Free Chuuk R code
## SitRep Pivots

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


#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)


###
#FUNCTIONS
###

#population and rate estimates
rate_estimates_age <- read_excel("Data/TB rate estimates.xlsx", col_names=TRUE,
                                 sheet="overall_rates") %>%
  pivot_longer(3:23, names_to="age_group", values_to="rate") %>%
  pivot_wider(id_cols = c("area","age_group"), names_from="measure", values_from="rate") %>%
  rename(est_ltbi_rate = pct_tst_pos_HI) %>%
  mutate(est_tb_rate = est_tb_rate/100000,
         est_tb_cases = round(est_tb_rate*pop),
         est_ltbi_cases = round(est_ltbi_rate*pop)
  )

rate_estimates_village <- read_excel("Data/TB rate estimates.xlsx", col_names=TRUE,
                                     sheet="village_rates")%>%
  mutate(est_tb_cases = round(est_tb_rate*pop),
         est_ltbi_cases = round(est_ltbi_rate*pop),
         island = str_to_upper(island),
         village = str_to_upper(village),
         full_village_name = paste(island,village)
  )

#standards
lagoon_list <- c("WENO","PAATA","ONEI","TOL","POLLE","UDOT","FEFEN",
                 "UMAN", "TONOAS")

lagoon_region <- tribble(
  ~municipality, ~region,
  "WENO", "WENO",
  "PAATA","FAICHUUK",
  "ONEI","FAICHUUK",
  "TOL","FAICHUUK",
  "POLLE","FAICHUUK",
  "UDOT","FAICHUUK",
  "FEFEN","SOUTHERN NAMONEAS", 
  "UMAN", "SOUTHERN NAMONEAS", 
  "TONOAS","SOUTHERN NAMONEAS"
)

graph_order <- c("pop","no_registered","no_screened","no_active_tb",
                 "no_ltbi","no_hd_prevention")

graph_order_long <- c("Total Population","No. Registered", "No. Screened", 
                        "No. Active TB", "No. LTBI", "No. HD Prevention")
###
##DATA
###

#load in clean flatfile
flatfile_clean <- read_excel("Data/flatfile_clean.xlsx",
                             guess_max = 20000, col_names = TRUE) %>%
  mutate(age_group = factor(age_group, 
                            levels=c("0-4","5-9","10-19","20-39","40-59","60+"))
         )


sitrep_pivots_age <-   flatfile_clean %>%
  group_by(age_group) %>%
  summarise(no_registered = n(),
            no_screened = sum(screened_at_clinic),
            
            no_active_tb = sum(active_tb_tx),
            
            no_ltbi = sum(ltbi_diagnosis),
            no_ltbi_recommended = sum(ltbi_tx_indicated),
            no_ltbi_started = sum(ltbi_tx_started),
            
            no_hd_referrals = sum(hd_further_assessment),
            no_hd_prevention = sum(hd_prev_given),
            
            no_diabetes = sum(dm_a1c_result),
            no_new_diabetes = sum(new_dm_result)
  ) %>%
  adorn_totals()

sitrep_pivots_sex<- flatfile_clean %>%
  group_by(sex) %>%
  summarise(no_registered = n(),
            no_screened = sum(screened_at_clinic),
            
            no_active_tb = sum(active_tb_tx),
            
            no_ltbi = sum(ltbi_diagnosis),
            no_ltbi_recommended = sum(ltbi_tx_indicated),
            no_ltbi_started = sum(ltbi_tx_started),
            
            no_hd_referrals = sum(hd_further_assessment),
            no_hd_prevention = sum(hd_prev_given),
            
            no_diabetes = sum(dm_a1c_result),
            no_new_diabetes = sum(new_dm_result)
  ) %>%
  adorn_totals()


##COMPLETE VILLAGE TABLE

complete_village<- flatfile_clean %>%
  mutate(full_village_name = paste(toupper(municipality),toupper(village))) %>%
  group_by(full_village_name) %>%
  summarise(no_registered = n(),
            no_screened = sum(screened_at_clinic),
            pct_seen_of_registered = no_screened/no_registered,
            
            no_active_tb = sum(active_tb_tx),
            pct_active_of_screened = no_active_tb/no_screened,
            
            no_ltbi = sum(tst_pos),
            pct_ltbi_of_screened = no_ltbi/no_screened,

            no_hd_prevention = sum(hd_prev_given),
            pct_hd_prev_of_screened = no_hd_prevention/no_screened,

            ) %>%
  filter(no_registered > 30) %>%
  #merge with rate_estimates by age group dataset for all ages, total rates
  merge(rate_estimates_village %>% 
          select(full_village_name, pop),by="full_village_name") %>%

  mutate(pct_registered_of_pop = no_registered/pop
         ) %>%
  mutate_at(c("pct_seen_of_registered","pct_active_of_screened","pct_ltbi_of_screened",
              "pct_hd_prev_of_screened","pct_registered_of_pop"), label_percent())

complete_village %>%
  select(full_village_name,pop,no_registered,no_screened,no_active_tb,
           no_ltbi,no_hd_prevention) %>%
  #rearrange into long form for ggplot
  pivot_longer(cols=2:7, names_to="category", values_to = "count") %>%
  group_by(full_village_name) %>%
  #calculate cascade pcts by full_village_name
  mutate(municipality = gsub( " .*$", "", full_village_name),
         category = factor(category, 
                           levels=graph_order))


complete_village %>%
  select(full_village_name,pop,no_registered,no_screened,no_active_tb,
         no_ltbi,no_hd_prevention) %>%
  #rearrange into long form for ggplot
  pivot_longer(cols=2:7, names_to="category", values_to = "count") %>%
  #calculate cascade pcts by full_village_name
  mutate(municipality = gsub( " .*$", "", full_village_name),
         category = factor(category, 
                           levels=graph_order)) %>%
  group_by(municipality) %>%
  ggplot(aes(x=category, y=count, fill=category)) +
  geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
  labs(x = "",
       y="No. of people") +  # title and caption
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
  scale_fill_brewer(palette="Set1") + 
  scale_x_discrete(labels = str_wrap(graph_order_long, width = 10)) +
  facet_wrap( ~ municipality, scales='free') +
  labs(
    title = "Clinic Screening Numbers for TB-Free Chuuk, 2023",
    subtitle = "All Ages, Chuuk"
  )



flatfile_clean %>%
  group_by(municipality) %>%
  mutate(tst_placed = case_when(tst_place_visit == "Y" ~ 1,
                                .default = 0),
         screened_at_clinic = if_else(is.not.na(tst_date_read) |
                                        is.not.na(active_tb) |
                                        is.not.na(result_hd_assessment) |
                                        is.not.na(weight),
                                      1, 0,
                                      missing = 0)) %>%
  summarise(no_tst_placed = sum(tst_placed),
            no_screened = sum(screened_at_clinic),
            no_tst_read = sum(tst_read)) %>%
  filter(municipality %in% c("POLLE","PAATA","ONEI")) %>%
  adorn_totals()



### MARCH 25 BOARD MEETING SITREP UPDATE

board_update_sitrep <-   flatfile_clean %>%
  group_by(age_group) %>%
  summarise(no_registered = n(),
            no_screened = sum(screened_at_clinic),
            
            no_active_tb = sum(active_tb_tx),
            no_tb_confirmed = sum(lab_confirmed == "YES", na.rm = TRUE),
            no_tb_tx_complete = sum(tb_outcome == "Complete" & lab_confirmed == "YES", na.rm = TRUE),
            
            no_ltbi = sum(ltbi_diagnosis),
            no_ltbi_complete = sum(epi_status == "Completed treatment" |
                                     epi_status == "Previous treatment", na.rm = TRUE),
            
            no_hd_referrals = sum(hd_further_assessment),
            no_hd_cases = sum(hd_confirmed),
            no_hd_prevention = sum(hd_prev_given),
            
            no_diabetes = sum(dm_a1c_or_hx, na.rm = TRUE),
            no_new_diabetes = sum(new_dm_result, na.rm = TRUE)
  ) %>%
  adorn_totals() %>%
  merge(rate_estimates_age %>% filter(area == "All Chuuk Lagoon") %>% select(age_group,pop_2010), 
        by="age_group") %>%
  ungroup() %>%
  mutate(pct_screened = no_screened/no_registered,
         
         active_tb_100k = round(no_active_tb/pop_2010*100000,1),
         pct_tb_tx_complete = no_tb_tx_complete/no_tb_confirmed,
         
         pct_screened_ltbi = no_ltbi / no_screened,
         pct_ltbi_tx_complete = no_ltbi_complete/no_ltbi,
         
         hd_cases_100k = round(no_hd_cases/pop_2010*100000,1),
         
         #8154 people screened 18+
         pct_screened_diabetes = no_diabetes / 8154
         ) %>%
  arrange(age_group) %>%
  mutate_at(c("pct_screened","pct_tb_tx_complete","pct_screened_ltbi",
              "pct_ltbi_tx_complete","pct_screened_diabetes"), 
            label_percent(1)) %>%
  select(age_group, no_registered, no_screened, pct_screened, 
         no_active_tb, active_tb_100k,no_tb_tx_complete,pct_tb_tx_complete, 
         no_ltbi, pct_screened_ltbi, no_ltbi_complete, pct_ltbi_tx_complete, 
         no_hd_referrals, no_hd_cases, hd_cases_100k,no_hd_prevention, 
         no_diabetes, pct_screened_diabetes, no_new_diabetes) %>%
  t() %>%
  as.data.frame() %>%
  row_to_names(1)



###FOR HEALTH SUMMIT

#load in clean flatfile
flatfile_clean %>%
  mutate(is_12_to_17 = case_when(age >= 12 & age <= 17 ~ 1,
                                 .default = 0)) %>%
  group_by(is_12_to_17) %>%
  summarise(no_registered = n(),
            no_screened = sum(screened_at_clinic),
            
            no_active_tb = sum(active_tb_tx),
            
            no_ltbi = sum(ltbi_diagnosis),
            no_ltbi_recommended = sum(ltbi_tx_indicated),
            no_ltbi_started = sum(ltbi_tx_started),
            
            no_hd_referrals = sum(hd_further_assessment),
            no_hd_prevention = sum(hd_prev_given),
            
            no_diabetes = sum(dm_a1c_result),
            no_new_diabetes = sum(new_dm_result)
  ) %>%
  adorn_totals()

### Using TBFC analysis flatfile

tbfc_analysis %>% 
  tabyl(ltbi_diagnosis)



board_update_sitrep <-   tbfc_analysis %>%
  group_by(age_group) %>%
  summarise(no_registered = n(),
            no_screened = sum(screened_at_clinic),
            
            no_active_tb = sum(active_tb_tx),
            no_tb_confirmed = sum(lab_confirmed == "YES", na.rm = TRUE),
            no_tb_tx_complete = sum(tb_outcome == "Complete" & lab_confirmed == "YES", na.rm = TRUE),
            
            no_ltbi = sum(ltbi_diagnosis),
            no_ltbi_complete = sum(epi_status == "Completed treatment" |
                                     epi_status == "Previous treatment", na.rm = TRUE),
            
            no_hd_referrals = sum(hd_further_assessment),
            # no_hd_cases = sum(hd_confirmed),
            no_hd_cases = 0,
            ##
            no_hd_prevention = sum(hd_prev_given),
            
            no_diabetes = sum(dm_a1c_or_hx, na.rm = TRUE),
            no_new_diabetes = sum(new_dm_result, na.rm = TRUE)
  ) %>%
  adorn_totals() %>%
  merge(rate_estimates_age %>% filter(area == "All Chuuk Lagoon") %>% select(age_group,pop_2010), 
        by="age_group") %>%
  ungroup() %>%
  mutate(pct_screened = no_screened/no_registered,
         
         active_tb_100k = round(no_active_tb/pop_2010*100000,1),
         pct_tb_tx_complete = no_tb_tx_complete/no_tb_confirmed,
         
         pct_screened_ltbi = no_ltbi / no_screened,
         pct_ltbi_tx_complete = no_ltbi_complete/no_ltbi,
         
         hd_cases_100k = round(no_hd_cases/pop_2010*100000,1),
         
         #8154 people screened 18+
         pct_screened_diabetes = no_diabetes / 8154
  ) %>%
  arrange(age_group) %>%
  mutate_at(c("pct_screened","pct_tb_tx_complete","pct_screened_ltbi",
              "pct_ltbi_tx_complete","pct_screened_diabetes"), 
            label_percent(1)) %>%
  select(age_group, no_registered, no_screened, pct_screened, 
         no_active_tb, active_tb_100k,no_tb_tx_complete,pct_tb_tx_complete, 
         no_ltbi, pct_screened_ltbi, no_ltbi_complete, pct_ltbi_tx_complete, 
         no_hd_referrals, no_hd_cases, hd_cases_100k,no_hd_prevention, 
         no_diabetes, pct_screened_diabetes, no_new_diabetes) %>%
  t() %>%
  as.data.frame() %>%
  row_to_names(1)
