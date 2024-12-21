## TB-Free Chuuk R code
## LTBI cascade
## question: when looking at all of the people who are estimated to have ltbi, how many did we identify?
## how many completed treatment?
# of TST pos in our villages we screened | # tested with TST
# positives (aka # test read) | # recommended for 3HP | # starting 3HP | # completing (or expected to complete 3HP

##TB estimates	
#Target Pop	- 100% | #TST Placed - 85% | #Screened	- 90% | #Referred - 8%
#LTBI - 20% | #Active TB - 1.1%

#PACKAGES
library(tidyverse) #pipes
library(readxl) #excel load-in
library(lubridate) #dealing with dates
library(stringr) #dealing with strings
library(vtable) #allows sumtable
library(janitor) #allows tabyl & cleaning names
library(ggplot2) #make graphs
library(ggpubr) #special aggregate of plots
library(ggthemes) #makes prettier graphs
library(gridExtra) #tiled grid of plots
library(scales) #percent
library(gtsummary)

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

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

cascade_order <- c("no_ltbi_diagnosis", "no_rec_ltbi",
                   "no_start_ltbi", "no_complete_ltbi")

cascade_order_long <- c("Skin Test Read", 
                        "Latent Tx Recommended", 
                        "Treatment Started", "Treatment Completed")


cascade_order_brostrom_spec <- c("no_tst_placed", "no_ltbi_diagnosis", "no_rec_ltbi",
                   "no_start_ltbi", "no_complete_ltbi")

cascade_order_long_brostrom_spec <- c("Skin Test Placed", 
                        "Skin Test Read", "Latent Tx Recommended", 
                        "Treatment Started", "Treatment Completed")
###
#FUNCTIONS
###

make_cascade_graph <- function(data) {
  ggplot(data = data, aes(x=category, y=count, fill=category)) +
    geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
    labs(
      #title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
      #subtitle = "Age >= 5 y.o., Lagoon vs. Weno",
      x = "",
      y="No. of people") +  # title and caption
    theme_classic() +
    theme(panel.background = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
    scale_fill_brewer(palette="Set1") + 
    scale_x_discrete(labels = str_wrap(cascade_order_long, width = 10)) +
    scale_y_continuous(labels = comma_format()) +  # add commas
    geom_label(aes(label = paste(label_comma()(count), scales::percent(percent),sep="\n")), 
               vjust = 1, nudge_y = .5, size = 3)
}

#datasets
#estimated cascade
tribble(
  ~category, ~estimate,
  "Target Pop", 1,
  "TST Placed", .85,
  "Screened", .9,
  "Referred", .08,
  "LTBI", .2,
  "Active TB", .011) %>%
  mutate(category = factor(category, 
                            levels=c("Target Pop", "TST Placed", "Screened",
                                     "LTBI",  "Referred", "Active TB"))
  ) %>%
  ggplot(aes(x=category, y=estimate)) +
  geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
  labs(y="Percent") +  # title and caption
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
  scale_y_continuous(labels = percent) # add pct

#population and rate estimates
rate_estimates_age <- read_excel("Data/TB rate estimates.xlsx", col_names=TRUE,
                             sheet="overall_rates") %>%
  pivot_longer(3:19, names_to="age_group", values_to="rate") %>%
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


#cascade datasets
cascade_dataset <- read_excel("Data/tbfc_analysis_dataset.xlsx") %>%
  merge(read_excel("Data/flatfile_clean.xlsx") %>%
          select(registration_no, ltbi_tx_indicated, 
                 island_ltbi, village_ltbi, notes, treatment_stop_reason,
                 treatment_status), 
        by="registration_no") %>%
  mutate(full_village_name = paste(municipality,village),
         area = case_when(toupper(municipality) == "WENO" | 
                            toupper(island_ltbi) == "WENO" | 
                            region %in% c("NW", "MORT") ~ "Weno",
                          toupper(municipality) != "WENO" | 
                            toupper(island_ltbi) != "WENO" ~ "Lagoon"
         )) %>%
  select(registration_no, screened_at_clinic, tst_read_yn, tst_result, 
         age_group,
         ltbi_diagnosis, ltbi_tx_indicated, ltbi_tx_started,
         ltbi_doses_completed,
         epi_status, region, municipality, village, 
         island_ltbi, village_ltbi, area,
         full_village_name, notes, treatment_stop_reason,
         treatment_status) %>%
  mutate(ltbi_tx_completed = case_when(epi_status == "Completed treatment" | 
                                         grepl("completion",epi_status)  ~ 1,
                                       is.not.na(epi_status) ~ 0),
         tst_place_visit = case_when(tst_read_yn == "No TST" ~ "N",
                                     .default = "Y")) %>%
  filter(screened_at_clinic == 1)

#BASIC COUNTS

#among all screened, total of ltbi diagnoses
cascade_dataset %>%
  tabyl(ltbi_diagnosis)

#number of ltbi diagnosed people who initiated treatment
cascade_dataset %>%
  filter(ltbi_diagnosis == 1) %>%
  tabyl(ltbi_tx_started)

#number of people completed ltbi treatment
cascade_dataset %>%
  filter(ltbi_diagnosis == 1) %>%
  tabyl(ltbi_tx_completed)

#treatment completion in lagoon higher than weno for all age groups
#all groups
cascade_dataset %>%
  filter(ltbi_diagnosis == 1) %>%
  select(ltbi_tx_completed, area) %>%
  tbl_summary(by = area, missing = "no",
              statistic = ltbi_tx_completed ~ "{p}%",
              digits = ~ 1) %>%
  add_p()

#by age group
cascade_dataset %>%
  filter(ltbi_diagnosis == 1) %>%
  select(ltbi_tx_completed, area, age_group) %>%
  tbl_strata(strata = c(age_group), 
             .tbl_fun =
               ~ .x %>%
               tbl_summary(by=area, missing = "no",
                           statistic = ltbi_tx_completed ~ "{p}%",
                           digits = ~ 1) %>%
               add_p()
  )

#most common reasons for failure to complete ltbi treatment
cascade_dataset %>%
  filter(ltbi_tx_completed != 1 & ltbi_diagnosis ==1) %>%
  tabyl(treatment_stop_reason) %>%
  adorn_totals()


#most common reason for treatment discontinuation?
#alcohol use
cascade_dataset %>%
  filter(ltbi_tx_completed != 1 & ltbi_diagnosis ==1) %>%
  mutate(alcohol_use = case_when(grepl(c("DRINK","ETOH","BEER","WINE","LIQUOR","SAKE",
                                         "ALCOHOL","DRUNK","12 PACK","ALCOHOLIC","ETHANOL"),
                                       toupper(notes)) ~ 1)
         ) %>%
  tabyl(alcohol_use)


##ORIGINAL CASCADE WITHOUT FIRST BAR OF TOTAL TESTED
### OVERALL CHUUK LAGOON CASCADE

#Weno and lagoon cascade
overall_cascade <- cascade_dataset %>%
  summarise(
    #number of people per village that had a skin test placed
    # no_tst_placed = sum(tst_read_yn == "Read" | tst_read_yn == "Not read"),
    #number of patients with an ltbi diagnosis def as having ltbi_treatment_id and not id as active in CC
    no_ltbi_diagnosis = sum(ltbi_diagnosis == 1),
    #number recommended ltbi using ltbi_treatment data
    no_rec_ltbi = sum(ltbi_tx_indicated == 1),
    #number started treatment def as ltbi_diagnosis == 1 % either start date in ltbi_tx or ltbi_dpot data
    no_start_ltbi = sum(ltbi_tx_started == 1),
    #defined as having completed tx status in dpot sheet or completed 11+ doses
    no_complete_ltbi = sum(ltbi_tx_completed == 1, na.rm=T),
    area = "All Chuuk Lagoon"
  ) %>%
  # #merge with rate_estimates by age group dataset for all ages, total rates
  # merge(rate_estimates_age %>% filter(age_group == "5+") %>% 
  #         select(area, pop, est_ltbi_cases) %>% rename(no_estimated_pos=est_ltbi_cases), by="area") %>%
  # #estimate no of tst placed on estimated ltbi cases using pct of village that had a skin test placed
  # mutate(no_tst_placed_on_ltbi = round(no_estimated_pos*(total_tst_placed/pop))
  # ) %>%
  # select(-total_tst_placed,-pop, -no_estimated_pos) %>%
  #rearrange into long form for ggplot
  pivot_longer(cols=1:4, names_to="category", values_to = "count") %>%
  # group_by(area) %>%
  #calculate cascade pcts by area
  mutate(percent = count/max(count),
         category = factor(category, 
                           levels=cascade_order)) %>%
  #rearrange into appropriate order based on cascade_order in above cascade_order listing
  #all categories should exist in the list or an error will result
  arrange(category)



#OVERALL TST READ RATE
all_cascade_g <- overall_cascade %>%
  ggplot(aes(x=category, y=count)) +
  geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
  labs(
    #title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
    #subtitle = "Age >= 5 y.o., All Chuuk Lagoon",
    x = "",
    y="No. of people"
    # caption = 
    #    "Total TST Positive (est.) = Hawaii % TST pos * Chuuk 2010 population 
    #Skin Test Placed (est.) = Total TST Pos (est.) * Percent of 2010 pop that had TSTs placed
    #Treatment Completed = Number completed 11+ doses of 3HP"
  ) +  # title and caption
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
  scale_fill_brewer(palette="Set1") + 
  facet_wrap( ~ area) + #tile the graphs
  scale_x_discrete(labels = str_wrap(cascade_order_long, width = 10)) +
  scale_y_continuous(labels = comma_format()) +  # add commas
  geom_label(aes(label = paste(label_comma()(count), scales::percent(percent),sep="\n")), 
             vjust = 1, nudge_y = .5)



#Weno and lagoon cascade
wl_cascade <- cascade_dataset %>%
  group_by(area) %>%
  filter(is.not.na(area)) %>%
  summarise(
    #estimate pct of village that had a skin test placed
    # no_tst_placed = sum(tst_place_visit == "Y"),
    #number of patients with an ltbi diagnosis def as having ltbi_treatment_id and not id as active in CC
    no_ltbi_diagnosis = sum(ltbi_diagnosis == 1),
    #number recommended ltbi using ltbi_treatment data
    no_rec_ltbi = sum(ltbi_tx_indicated == 1),
    #number started treatment def as ltbi_diagnosis == 1 % either start date in ltbi_tx or ltbi_dpot data
    no_start_ltbi = sum(ltbi_tx_started == 1),
    #defined as having completed tx status in dpot sheet or completed 11+ doses
    no_complete_ltbi = sum(ltbi_tx_completed == 1, na.rm=T)
  ) %>%
  #merge with rate_estimates by age group dataset for all ages, total rates
  # merge(rate_estimates_age %>% filter(age_group == "5+") %>% 
  #         select(area, pop, est_ltbi_cases) %>% rename(no_estimated_pos=est_ltbi_cases), by="area") %>%
  # #estimate no of tst placed on estimated ltbi cases using pct of village that had a skin test placed
  # mutate(no_tst_placed_on_ltbi = round(no_estimated_pos*(total_tst_placed/pop))
  #        ) %>%
  # select(-total_tst_placed,-pop, -no_estimated_pos) %>%
  #rearrange into long form for ggplot
  pivot_longer(cols=2:5, names_to="category", values_to = "count") %>%
  group_by(area) %>%
  #calculate cascade pcts by area
  mutate(percent = count/max(count),
         category = factor(category, 
                           levels=cascade_order)) %>%
  #rearrange into appropriate order based on cascade_order in above cascade_order listing
  #all categories should exist in the list or an error will result
  arrange(category)



#OVERALL TST READ RATE
wl_cascade_g <- wl_cascade %>%
  ggplot(aes(x=category, y=count)) +
  geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
  labs(
    #title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
    #subtitle = "Age >= 5 y.o., Lagoon vs. Weno",
    x = "",
    y="No. of people",
    caption = 
      # "Total TST Positive (est.) = Hawaii % TST pos * Chuuk 2010 population 
      # Skin Test Placed (est.) = Total TST Pos (est.) * Percent of 2010 pop that had TSTs placed
      "Treatment Completed = Received 11 or more doses of 3HP within 16 weeks") +  # title and caption
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
  scale_fill_brewer(palette="Set1") + 
  facet_wrap( ~ area) + #tile the graphs
  scale_x_discrete(labels = str_wrap(cascade_order_long, width = 10)) +
  scale_y_continuous(labels = comma_format()) +  # add commas
  geom_label(aes(label = paste(label_comma()(count), scales::percent(percent),sep="\n")), 
             vjust = 1, nudge_y = .5, size = 3)

### ARRANGE ALL LTBI CASCADE INTO ONE GRAPH

ggsave(plot=ggarrange(all_cascade_g,
                      wl_cascade_g, 
                      heights = c(.75,1),
                      labels= c("LTBI Treatment Cascade for TB-Free Chuuk, 2023",
                                "By Island Area"),
                      font.label= list("plain","black",size=12),
                      common.legend = TRUE,
                      label.x = 0,
                      legend = "bottom",
                      nrow = 2),
       "Figures/LTBI Treatment Cascade for TB-Free Chuuk 2023 - Updated Nov 2024.jpg",
       width = 1280, height = 1280, units = "px", scale = 2.5, dpi=300)
#-----------------------------------------
####TEMPORARY GRAPHS WITH FIRST BAR OF NO TESTED
### OVERALL CHUUK LAGOON CASCADE

#Weno and lagoon cascade
overall_cascade <- cascade_dataset %>%
  summarise(
    #number of people per village that had a skin test placed
    no_tst_placed = sum(tst_read_yn == "Read" | tst_read_yn == "Not read"),
    #number of patients with an ltbi diagnosis def as having ltbi_treatment_id and not id as active in CC
    no_ltbi_diagnosis = sum(ltbi_diagnosis == 1),
    #number recommended ltbi using ltbi_treatment data
    no_rec_ltbi = sum(ltbi_tx_indicated == 1),
    #number started treatment def as ltbi_diagnosis == 1 % either start date in ltbi_tx or ltbi_dpot data
    no_start_ltbi = sum(ltbi_tx_started == 1),
    #defined as having completed tx status in dpot sheet or completed 11+ doses
    no_complete_ltbi = sum(ltbi_tx_completed == 1, na.rm=T),
    area = "All Chuuk Lagoon"
  ) %>%
  # #merge with rate_estimates by age group dataset for all ages, total rates
  # merge(rate_estimates_age %>% filter(age_group == "5+") %>% 
  #         select(area, pop, est_ltbi_cases) %>% rename(no_estimated_pos=est_ltbi_cases), by="area") %>%
  # #estimate no of tst placed on estimated ltbi cases using pct of village that had a skin test placed
  # mutate(no_tst_placed_on_ltbi = round(no_estimated_pos*(total_tst_placed/pop))
  # ) %>%
  # select(-total_tst_placed,-pop, -no_estimated_pos) %>%
  #rearrange into long form for ggplot
  pivot_longer(cols=1:5, names_to="category", values_to = "count") %>%
  # group_by(area) %>%
  #calculate cascade pcts by area
  mutate(percent = count/max(count),
         category = factor(category, 
                           levels=cascade_order_brostrom_spec)) %>%
  #rearrange into appropriate order based on cascade_order in above cascade_order listing
  #all categories should exist in the list or an error will result
  arrange(category)



#OVERALL TST READ RATE
all_cascade_g <- overall_cascade %>%
    ggplot(aes(x=category, y=count)) +
    geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
    labs(
      #title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
      #subtitle = "Age >= 5 y.o., All Chuuk Lagoon",
      x = "",
      y="No. of people"
    # caption = 
    #    "Total TST Positive (est.) = Hawaii % TST pos * Chuuk 2010 population 
    #Skin Test Placed (est.) = Total TST Pos (est.) * Percent of 2010 pop that had TSTs placed
    #Treatment Completed = Number completed 11+ doses of 3HP"
    ) +  # title and caption
    theme_classic() +
    theme(panel.background = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
    scale_fill_brewer(palette="Set1") + 
    facet_wrap( ~ area) + #tile the graphs
    scale_x_discrete(labels = str_wrap(cascade_order_long_brostrom_spec, width = 10)) +
    scale_y_continuous(labels = comma_format()) +  # add commas
    geom_label(aes(label = paste(label_comma()(count), scales::percent(percent),sep="\n")), 
               vjust = 1, nudge_y = .5)



#Weno and lagoon cascade
wl_cascade <- cascade_dataset %>%
  group_by(area) %>%
  filter(is.not.na(area)) %>%
  summarise(
    #estimate pct of village that had a skin test placed
    no_tst_placed = sum(tst_place_visit == "Y"),
    #number of patients with an ltbi diagnosis def as having ltbi_treatment_id and not id as active in CC
    no_ltbi_diagnosis = sum(ltbi_diagnosis == 1),
    #number recommended ltbi using ltbi_treatment data
    no_rec_ltbi = sum(ltbi_tx_indicated == 1),
    #number started treatment def as ltbi_diagnosis == 1 % either start date in ltbi_tx or ltbi_dpot data
    no_start_ltbi = sum(ltbi_tx_started == 1),
    #defined as having completed tx status in dpot sheet or completed 11+ doses
    no_complete_ltbi = sum(ltbi_tx_completed == 1, na.rm=T)
            ) %>%
  #merge with rate_estimates by age group dataset for all ages, total rates
  # merge(rate_estimates_age %>% filter(age_group == "5+") %>% 
  #         select(area, pop, est_ltbi_cases) %>% rename(no_estimated_pos=est_ltbi_cases), by="area") %>%
  # #estimate no of tst placed on estimated ltbi cases using pct of village that had a skin test placed
  # mutate(no_tst_placed_on_ltbi = round(no_estimated_pos*(total_tst_placed/pop))
  #        ) %>%
  # select(-total_tst_placed,-pop, -no_estimated_pos) %>%
  #rearrange into long form for ggplot
  pivot_longer(cols=2:6, names_to="category", values_to = "count") %>%
  group_by(area) %>%
  #calculate cascade pcts by area
  mutate(percent = count/max(count),
         category = factor(category, 
                           levels=cascade_order_brostrom_spec)) %>%
  #rearrange into appropriate order based on cascade_order in above cascade_order listing
  #all categories should exist in the list or an error will result
  arrange(category)
  
  

#OVERALL TST READ RATE
wl_cascade_g <- wl_cascade %>%
  ggplot(aes(x=category, y=count)) +
  geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
  labs(
    #title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
    #subtitle = "Age >= 5 y.o., Lagoon vs. Weno",
    x = "",
    y="No. of people",
    caption = 
    # "Total TST Positive (est.) = Hawaii % TST pos * Chuuk 2010 population 
    # Skin Test Placed (est.) = Total TST Pos (est.) * Percent of 2010 pop that had TSTs placed
    "Treatment Completed = Received 11 or more doses of 3HP within 16 weeks") +  # title and caption
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
  scale_fill_brewer(palette="Set1") + 
  facet_wrap( ~ area) + #tile the graphs
  scale_x_discrete(labels = str_wrap(cascade_order_long_brostrom_spec, width = 10)) +
  scale_y_continuous(labels = comma_format()) +  # add commas
  geom_label(aes(label = paste(label_comma()(count), scales::percent(percent),sep="\n")), 
             vjust = 1, nudge_y = .5, size = 3)

### ARRANGE ALL LTBI CASCADE INTO ONE GRAPH

ggsave(plot=ggarrange(all_cascade_g,
                      wl_cascade_g, 
                      heights = c(.75,1),
                      labels= c("LTBI Treatment Cascade for TB-Free Chuuk, 2023",
                                "By Island Area"),
                      font.label= list("plain","black",size=12),
                      common.legend = TRUE,
                      label.x = 0,
                      legend = "bottom",
                      nrow = 2),
       "Figures/LTBI Treatment Cascade for TB-Free Chuuk 2023 - Updated Dec 2024.jpg",
       width = 1280, height = 1280, units = "px", scale = 2.5, dpi=300)
#-----------------------------------------

###
#MAKE CASCADES FOR ALL VILLAGES IN THE LAGOON
###



#village cascade
village_cascade <- cascade_dataset %>%
  group_by(full_village_name) %>%
  filter(is.not.na(full_village_name)) %>%
  summarise(
    #estimate pct of village that had a skin test placed
    total_tst_placed = sum(tst_place_visit == "Y"),
    #number of patients with an ltbi diagnosis def as having ltbi_treatment_id and not id as active in CC
    no_ltbi_diagnosis = sum(ltbi_diagnosis == 1),
    #number recommended ltbi using ltbi_treatment data
    no_rec_ltbi = sum(ltbi_tx_indicated == 1),
    #number started treatment def as ltbi_diagnosis == 1 % either start date in ltbi_tx or ltbi_dpot data
    no_start_ltbi = sum(ltbi_tx_started == 1),
    #defined as having completed tx status in dpot sheet or completed 11+ doses
    no_complete_ltbi = sum(ltbi_tx_completed == 1)
  ) %>%
  #merge with rate_estimates by age group dataset for all ages, total rates
  merge(rate_estimates_village %>% 
          select(full_village_name, pop, est_ltbi_cases) %>% 
          rename(no_estimated_pos=est_ltbi_cases), by="full_village_name") %>%
  #estimate no of tst placed on estimated ltbi cases using pct of village that had a skin test placed
  mutate(no_tst_placed_on_ltbi = round(no_estimated_pos*(total_tst_placed/pop))
  ) %>%
  select(-total_tst_placed,-pop, -no_estimated_pos) %>%
  #rearrange into long form for ggplot
  pivot_longer(cols=2:6, names_to="category", values_to = "count") %>%
  group_by(full_village_name) %>%
  #calculate cascade pcts by full_village_name
  mutate(percent = count/max(count),
         category = factor(category, 
                           levels=cascade_order),
         municipality = gsub( " .*$", "", full_village_name)) %>%
  #rearrange into appropriate order based on cascade_order in above cascade_order listing
  #all categories should exist in the list or an error will result
  arrange(category) %>%
  merge(lagoon_region, by="municipality")



#GRAPH FOR EACH MUNI
village_cascade_weno <- make_cascade_graph(village_cascade %>%
                                                   filter(region == "WENO")) +
  facet_wrap( ~ full_village_name, scales='free') +
  labs(
    title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
    subtitle = "Age >= 5 y.o., Weno and Faichuuk"
  )

village_cascade_faichuuk <- make_cascade_graph(village_cascade %>%
                                                       filter(region == "FAICHUUK")) +
  facet_wrap( ~ full_village_name, scales='free') +
  labs(
    #title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
    #subtitle = "Age >= 5 y.o., Lagoon vs. Weno",
    caption = 
      "Skin Test Placed (est.) = Total TST Pos (est.) * Percent of 2010 pop that had TSTs placed
    Treatment Completed = Number completed 11+ doses of 3HP")  # title and caption

village_cascade_uman_fefen <- make_cascade_graph(village_cascade %>%
                                                         filter(region == "SOUTHERN NAMONEAS" &
                                                                  municipality != "TONOAS")) +
  facet_wrap( ~ full_village_name, scales='free') +
  labs(
    title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
    subtitle = "Age >= 5 y.o., Fefen, Uman, Tonoas"
  )

village_cascade_village_tonoas <- make_cascade_graph(village_cascade %>%
                                                     filter(municipality == "TONOAS")) +
  facet_wrap( ~ full_village_name, scales='free') +
  labs(
    #title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
    #subtitle = "Age >= 5 y.o., Lagoon vs. Weno",
    caption = 
      "Skin Test Placed (est.) = Total TST Pos (est.) * Percent of 2010 pop that had TSTs placed
    Treatment Completed = Number completed 11+ doses of 3HP")  # title and caption


ggsave(plot=ggarrange(village_cascade_weno,
                      village_cascade_faichuuk,
                      heights = c(1,1),
                      font.label= list("plain","black",size=12),
                      common.legend = TRUE,
                      label.x = 0,
                      legend = "none",
                      nrow = 2),
       "Figures/LTBI cascade by village - Weno and Faichuuk.jpg",
       width = 3000, height = 4500, units = "px", scale = 1.5, dpi=300)


ggsave(plot=ggarrange(
                      village_cascade_uman_fefen,
                      village_cascade_village_tonoas,
                      heights = c(.5,1),
                      font.label= list("plain","black",size=12),
                      common.legend = TRUE,
                      label.x = 0,
                      legend = "none",
                      nrow = 2),
       "Figures/LTBI cascade by village - Southern Namoneas.jpg",
       width = 3000, height = 4500, units = "px", scale = 1.5, dpi=300)

