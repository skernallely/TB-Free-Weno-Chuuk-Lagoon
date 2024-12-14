## TB-Free Chuuk R code
## TST Positivity

#PACKAGES
library(tidyverse) #pipes, scales, lubridate, stringr
library(readxl) #excel load-in
library(janitor) #allows tabyl & cleaning names
library(ggplot2) #make graphs
library(ggpubr) #special aggregate of plots
library(ggthemes) #makes prettier graphs
library(gridExtra) #tiled grid of plots

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

#standards

lagoon_region <- tribble(
  ~municipality, ~region,
  "WENO", "NORTHERN NAMONEAS",
  # "FONO", "NORTHERN NAMONEAS",
  # "PIIS-PANEU", "NORTHERN NAMONEAS",
  
  "PAATA","FAICHUUK",
  "ONEI","FAICHUUK",
  "TOL","FAICHUUK",
  "POLLE","FAICHUUK",
  "UDOT","FAICHUUK",
  # "FANAPANGES", "FAICHUUK",
  # "ROMANUM", "FAICHUUK",
  # "EOT", "FAICHUUK",
  
  "FEFEN","SOUTHERN NAMONEAS", 
  "UMAN", "SOUTHERN NAMONEAS", 
  "TONOAS","SOUTHERN NAMONEAS",
  # "PAREM", "SOUTHERN NAMONEAS",
  # "SIIS", "SOUTHERN NAMONEAS",

  # "FANANU", "NORTHWEST",
  # "HOUK", "NORTHWEST",
  # "MAKUR", "NORTHWEST",
  "MURILLO", "NORTHWEST",
  # "NOMWIN", "NORTHWEST",
  # "ONOUN", "NORTHWEST",
  # "ONO", "NORTHWEST",
  # "PIHERARH", "NORTHWEST",
  # "POLLAP", "NORTHWEST",
  # "POLOWAT", "NORTHWEST",
  "RUO", "NORTHWEST",
  # "TAMATAM", "NORTHWEST",
  # "UNANU", "NORTHWEST",
# 
#   "ETTAL", "MORTLOCKS",
#   "KUTTU", "MORTLOCKS",
  "LEKINIOCH", "MORTLOCKS",
  # "LOSAP", "MORTLOCKS",
  # "MOCH", "MORTLOCKS",
  # "NAMA", "MORTLOCKS",
  # "NAMOLUK", "MORTLOCKS",
  "ONEOP", "MORTLOCKS",
  # "PIIS-EMWAR", "MORTLOCKS",
  "SATOWAN", "MORTLOCKS",
  # "TA", "MORTLOCKS"
)

#datasets
#subset of patients who had a tst placed
tst_dataset <- read_excel("Data/flatfile_clean.xlsx",
                           guess_max = 20000, col_names = TRUE) %>%
  
  select("registration_no", "age", "age_group", "sex", "municipality", "village",
         "screening_site", "date_screening", "date_of_visit1", "tst_date_read", 
         "tst_result", "tst_interpretation", "tb_disease_exposure", "treated_tb_ltbi",
         "current_tb_symptoms_none", "current_tb_symptoms_any_duration",
         "weight", "height", "result_hd_assessment", "xray_indicated",
         "active_tb", "ltbi_actions", "tb_sputum", "a1c", "history_diabetes",
         "medications_for_diabetes", "smoking_history", "hd_prevention",
         "hd_prevention_text", "treatment_recommended", "treatment_started",
         "medication_order_full", "outcome_case_conference", 
         "actions_not_active_tb", "notes_case_conference", "tst_read",
         "tst_place_visit","tst_result_10","tst_pos","tst_neg","tst_read_elapse",
         "tst_read_cat", "tst_read_yn") %>%
  mutate(municipality = case_when(municipality %in% c("MURILLO","RUO",
                                                      "ONEOP","SATOWAN",
                                                      "LEKINIOCH") ~ "WENO",
                                  .default = municipality),
         age_group_new = case_when(age_group == "5-9" | age_group == "10-19" ~ "5-19",
                                   .default = age_group),
         age_group = factor(age_group_new, 
                            levels=c("5-19","20-39","40-59","60+")),
         municipality = str_to_upper(municipality),
         village = str_to_upper(village),
         village = if_else(grepl("KUCHUWA",village), "KUCHUWA",
                           if_else(grepl("NANTAKU",village), "NEPUKOS",
                                   village)
                           ),
         full_village_name = paste(municipality,village)
  ) %>%
  merge(lagoon_region, by="municipality")


###FUNCTIONS

##make TST read rate graphs

make_tst_read_graphs <- function(data) {
  ggplot(data = data, 
         aes(x=age_group, y=pct, fill=tst_read_yn)) +
    geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
    labs(y="% of people with TSTs placed",
         x="Age Group",
         caption = "Age group bins are not of equal size; exercise caution when drawing conclusions") +  # title and caption
    theme_classic() +
    theme(panel.background = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
    scale_fill_brewer(palette="Paired") +
    scale_y_continuous(labels = percent) # add pct
}

##make TST positivity graphs

make_tst_pos_graphs <- function(data) {
  ggplot(data = data, 
         aes(x=age_group, y=pct, fill=tst_result_10)) +
    geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
    labs(
      y="% of TSTs read",
      x="Age Group") +  # title and caption
    theme_classic() +
    theme(panel.background = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
    scale_fill_tableau(palette = "Superfishel Stone") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(labels = percent) # add pct
}

###
#TST READ RATE
###

#tst read data for those 5+
tst_read_rate <- tst_dataset %>%
  filter(tst_read_yn %in% c("Read","Not read")) %>%
  tabyl(age_group, tst_read_yn) %>%
  adorn_percentages() %>%
  pivot_longer(cols=2:3, names_to="tst_read_yn", values_to = "pct") %>%
  filter(age_group != "0-4")
  
#OVERALL TST READ RATE
tst_read_age_g <- make_tst_read_graphs(tst_read_rate)

#TST READ RATE FOR LAGOON
tst_read_rate_lagoon <- tst_dataset %>%
  filter(tst_read_yn %in% c("Read","Not read")) %>%
  group_by(age_group, region, tst_read_yn) %>%
  summarise(count=n()) %>%
  filter(is.not.na(age_group)) %>%
  ungroup() %>%
  group_by(age_group, region) %>%
  mutate(pct =  count/sum(count)) %>% 
  ungroup() %>%
  filter(age_group != "0-4")

#GRAPH FOR EACH MUNI
tst_read_age_lagoon_g <- make_tst_read_graphs(tst_read_rate_lagoon) +
  facet_wrap( ~ region, scales='free')

ggsave(plot=ggarrange(tst_read_age_g,
                      tst_read_age_lagoon_g, 
                      heights = c(1,2),
                      labels= c("TST read rate by municipality and age for patients 5 and older",
                                "By Island"),
                      font.label= list("plain","black",size=12),
                      common.legend = TRUE,
                      label.x = 0,
                      legend = "bottom",
                      nrow = 2),
       "Figures/TST read rate by lagoon island and age group.jpg",
       width = 1280, height = 1280, units = "px", scale = 2.5, dpi=300)


###
#TST POSITIVITY
###

#tst positivity for those 5+
tst_pos_rate <- tst_dataset %>%
  filter(is.not.na(tst_result_10)) %>%
  tabyl(age_group, tst_result_10) %>%
  adorn_percentages() %>%
  pivot_longer(cols=2:3, names_to="tst_result_10", values_to = "pct") %>%
  filter(age_group != "0-4")

#OVERALL TST POS RATE
tst_pos_age_g <- make_tst_pos_graphs(tst_pos_rate)

#TST READ RATE FOR LAGOON
tst_pos_rate_lagoon <- tst_dataset %>%
  mutate(municipality = case_when(municipality %notin% lagoon_region$municipality ~ "WENO",
                            .default=municipality)) %>%
  filter(tst_read_yn %in% c("Read") & is.not.na(tst_result_10)) %>%
  group_by(age_group, municipality, tst_result_10) %>%
  summarise(count=n()) %>%
  filter(is.not.na(age_group)) %>%
  ungroup() %>%
  group_by(age_group, municipality) %>%
  mutate(pct =  count/sum(count)) %>% 
  ungroup() %>%
  filter(age_group != "0-4" & municipality %in% lagoon_region$municipality)

#GRAPH FOR EACH MUNI
tst_pos_age_lagoon_g <- make_tst_pos_graphs(tst_pos_rate_lagoon) +
  facet_wrap( ~ municipality, scales='free') +
  coord_cartesian(ylim = c(0, .6))  +
  theme(text = element_text(family = 'Open Sans'),
        plot.background = element_rect(fill = "#f3f6f8"), 
        panel.background = element_rect(fill = "#f3f6f8", colour="#f3f6f8"), 
        panel.border = element_blank(),
        legend.position="none",
        legend.background = element_rect(fill = "#f3f6f8"),
        legend.title = element_blank(),
        plot.margin = unit(c(.2,.2,.2,.2), "cm"))

ggsave(plot=ggarrange(tst_pos_age_g,
                      tst_pos_age_lagoon_g, 
                      heights = c(1,2),
                      labels= c("TST positivity rate by municipality and age for patients 5 and older",
                                "By Island"),
                      font.label= list("plain","black",size=12),
                      common.legend = TRUE,
                      label.x = 0,
                      legend = "bottom",
                      nrow = 2),
       "Figures/TST positivity rate by lagoon island and age group.jpg",
       width = 1280, height = 1280, units = "px", scale = 2.5, dpi=300)


ggsave(plot=tst_pos_age_lagoon_g,
       "Figures/TST positivity rate by island and age group.jpg",
       width = 1280, height = 1000, units = "px", scale = 2.5, dpi=300)


###
##BY VILLAGE
###


#TST READ RATE FOR VILLAGE
tst_read_rate_village <- tst_dataset %>%
  filter(tst_read_yn %in% c("Read","Not read")) %>%
  group_by(age_group, full_village_name, tst_read_yn) %>%
  summarise(count=n()) %>%
  filter(is.not.na(age_group)) %>%
  ungroup() %>%
  group_by(age_group, full_village_name) %>%
  mutate(pct =  count/sum(count),
         municipality = gsub( " .*$", "", full_village_name)) %>% 
  ungroup() %>%
  filter(age_group != "0-4" & full_village_name %in% villages_screened_30_plus) %>%
  merge(lagoon_region, by="municipality")

#GRAPH FOR EACH MUNI
tst_read_rate_village_g <- make_tst_read_graphs(tst_read_rate_village)

#TST READ RATE FOR VILLAGE
tst_pos_rate_village <- tst_dataset %>%
  filter(tst_read_yn %in% c("Read") & is.not.na(tst_result_10)) %>%
  group_by(age_group, full_village_name, municipality, tst_result_10) %>%
  summarise(count=n()) %>%
  filter(is.not.na(age_group)) %>%
  ungroup() %>%
  group_by(age_group, full_village_name, municipality) %>%
  mutate(pct =  count/sum(count)) %>% 
  ungroup() %>%
  filter(age_group != "0-4" & full_village_name %in% villages_screened_30_plus) %>%
  merge(lagoon_region, by="municipality")

#GRAPH FOR EACH MUNI
tst_pos_rate_village_weno <- make_tst_pos_graphs(tst_pos_rate_village %>%
                                                   filter(region == "WENO")) +
  facet_wrap( ~ full_village_name, scales='free') +
  labs(
    title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
    subtitle = "Age >= 5 y.o., Weno"
  )

tst_pos_rate_village_faichuuk <- make_tst_pos_graphs(tst_pos_rate_village %>%
                                                   filter(region == "FAICHUUK")) +
  facet_wrap( ~ full_village_name, scales='free') +
  labs(
    subtitle = "Faichuuk"
  )

tst_pos_rate_village_uman_fefen <- make_tst_pos_graphs(tst_pos_rate_village %>%
                                                       filter(region == "SOUTHERN NAMONEAS" &
                                                                municipality != "TONOAS")) +
  facet_wrap( ~ full_village_name, scales='free')  +
  labs(
    subtitle = "Uman & Fefen"
  )

tst_pos_rate_village_tonoas <- make_tst_pos_graphs(tst_pos_rate_village %>%
                                                         filter(municipality == "TONOAS")) +
  facet_wrap( ~ full_village_name, scales='free') +
  labs(
    subtitle = "Tonoas",
    caption = "Age group bins are not of equal size; exercise caution when drawing conclusions")


ggsave(plot=ggarrange(tst_pos_rate_village_weno,
                      tst_pos_rate_village_faichuuk,
                      tst_pos_rate_village_uman_fefen,
                      tst_pos_rate_village_tonoas,
                      heights = c(1,1,1,1),
                      font.label= list("plain","black",size=12),
                      common.legend = TRUE,
                      label.x = 0,
                      legend = "bottom",
                      nrow = 4),
       "Figures/TST positivity rate by village and age group.jpg",
       width = 1280, height = 3600, units = "px", scale = 2.5, dpi=300)







###
##BOARD MEETING GRAPH

##TST READ
ggsave(plot = ggplot(data = tst_read_rate, 
       aes(x=age_group, y=pct, fill=tst_read_yn)) +
  geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
  labs(y="% of people with TSTs placed",
       x="Age Group",
       title="People with TSTs placed by screening status and age group",
       subtitle="TB-Free Chuuk 2023") +  # title and caption
  theme_classic() +
  theme(text = element_text(family = 'Open Sans'),
        plot.background = element_rect(fill = "#f3f6f8"), 
        panel.background = element_rect(fill = "#f3f6f8", colour="#f3f6f8"), 
        panel.border = element_blank(),
        legend.position="right",
        legend.background = element_rect(fill = "#f3f6f8"),
        legend.title = element_blank(),
        plot.margin = unit(c(.2,.2,.2,.2), "cm")) +# turn off minor 
  scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels = percent)
  # geom_text(aes(label = paste(round(pct*100,1), "%")), 
  #           position = position_stack(vjust = 0.5,reverse = FALSE))
  ,
  "Figures/TST read rate by age group.png",
  width = 1280, height = 675, units = "px", scale = 1.5, dpi=300)

##TST POS
ggsave(plot = ggplot(data = tst_read_rate, 
                     aes(x=age_group, y=pct, fill=tst_read_yn)) +
         geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
         labs(y="% of people with TSTs placed",
              x="Age Group",
              title="People with TSTs placed by screening status and age group",
              subtitle="TB-Free Chuuk 2023") +  # title and caption
         theme_classic() +
         theme(text = element_text(family = 'Open Sans'),
               plot.background = element_rect(fill = "#f3f6f8"), 
               panel.background = element_rect(fill = "#f3f6f8", colour="#f3f6f8"), 
               panel.border = element_blank(),
               legend.position="right",
               legend.background = element_rect(fill = "#f3f6f8"),
               legend.title = element_blank(),
               plot.margin = unit(c(.2,.2,.2,.2), "cm")) +# turn off minor 
         scale_fill_brewer(palette="Paired") +
         scale_y_continuous(labels = percent)
       # geom_text(aes(label = paste(round(pct*100,1), "%")), 
       #           position = position_stack(vjust = 0.5,reverse = FALSE))
       ,
       "Figures/TST read rate by age group.png",
       width = 1280, height = 675, units = "px", scale = 1.5, dpi=300)

ggsave(plot = ggplot(data = tst_pos_rate, 
       aes(x=age_group, y=pct, fill=tst_result_10)) +
  geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
  labs(
    y="% of TSTs read",
    x="Age Group",
    title="TST positivity by age for patients 5 and older",
    subtitle="TB-Free Chuuk 2023") +  # title and caption) +  # title and caption
  theme_classic() +
  theme(text = element_text(family = 'Open Sans'),
        plot.background = element_rect(fill = "#f3f6f8"), 
        panel.background = element_rect(fill = "#f3f6f8", colour="#f3f6f8"), 
        panel.border = element_blank(),
        legend.position="right",
        legend.background = element_rect(fill = "#f3f6f8"),
        legend.title = element_blank(),
        plot.margin = unit(c(.2,.2,.2,.2), "cm")) +# turn off minor 
  scale_fill_tableau(palette = "Superfishel Stone") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = percent),
  "Figures/TST pos rate by age group.png",
  width = 1280, height = 675, units = "px", scale = 1.5, dpi=300)




#### TST POS FOR REGION
tst_pos_rate_region <- tst_dataset %>%
  filter(tst_read_yn %in% c("Read") & is.not.na(tst_result_10)) %>%
  mutate(region = case_when(region %in% c("MORT","NW") ~ "NORTHERN NAMONEAS",
                            .default=region)) %>%
  group_by(age_group, region, tst_result_10) %>%
  summarise(count=n()) %>%
  filter(is.not.na(age_group)) %>%
  ungroup() %>%
  group_by(age_group, region) %>%
  mutate(pct =  count/sum(count)) %>% 
  ungroup() %>%
  filter(age_group != "0-4" & region %in% lagoon_region$region)

#GRAPH FOR EACH MUNI
make_tst_pos_graphs(tst_pos_rate_region %>% filter(region == "FAICHUUK")) +
  labs(title="TST positivity rate by age group, Faichuuk") +
  theme(text = element_text(family = 'Open Sans'),
        plot.background = element_rect(fill = "#f3f6f8"), 
        panel.background = element_rect(fill = "#f3f6f8", colour="#f3f6f8"), 
        panel.border = element_blank(),
        legend.position="right",
        legend.background = element_rect(fill = "#f3f6f8"),
        legend.title = element_blank(),
        plot.margin = unit(c(.2,.2,.2,.2), "cm")) # turn off minor

ggsave(plot=ggplot(data = tst_pos_rate_region %>% filter(region == "FAICHUUK"), 
       aes(x=age_group, y=pct, fill=tst_result_10)) +
  geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
  labs(title="TST positivity rate by age group, Faichuuk",
    y="% of TSTs read",
    x="Age Group") +  # title and caption
  theme_classic() +
  theme(text = element_text(family = 'Open Sans'),
        plot.background = element_rect(fill = "#f3f6f8"), 
        panel.background = element_rect(fill = "#f3f6f8", colour="#f3f6f8"), 
        panel.border = element_blank(),
        legend.position="none",
        legend.background = element_rect(fill = "#f3f6f8"),
        legend.title = element_blank(),
        plot.margin = unit(c(.2,.2,.2,.2), "cm")) +# turn off minor 
  scale_fill_tableau(palette = "Superfishel Stone") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = percent),
  "Figures/TST pos rate by age group - Faichuuk.png",
  width = 1280, height = 675, units = "px", scale = 1.5, dpi=300)


ggsave(plot=ggplot(data = tst_pos_rate_region %>% filter(region == "NORTHERN NAMONEAS"), 
                   aes(x=age_group, y=pct, fill=tst_result_10)) +
         geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
         labs(title="TST positivity rate by age group, Northern Namoneas",
              y="% of TSTs read",
              x="Age Group") +  # title and caption
         theme_classic() +
         theme(text = element_text(family = 'Open Sans'),
               plot.background = element_rect(fill = "#f3f6f8"), 
               panel.background = element_rect(fill = "#f3f6f8", colour="#f3f6f8"), 
               panel.border = element_blank(),
               legend.position="none",
               legend.background = element_rect(fill = "#f3f6f8"),
               legend.title = element_blank(),
               plot.margin = unit(c(.2,.2,.2,.2), "cm")) +# turn off minor 
         scale_fill_tableau(palette = "Superfishel Stone") +
         coord_cartesian(ylim = c(0, 1)) +
         scale_y_continuous(labels = percent),
       "Figures/TST pos rate by age group - NN.png",
       width = 1280, height = 675, units = "px", scale = 1.5, dpi=300)

ggsave(plot=ggplot(data = tst_pos_rate_region %>% filter(region == "SOUTHERN NAMONEAS"), 
                   aes(x=age_group, y=pct, fill=tst_result_10)) +
         geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
         labs(title="TST positivity rate by age group, Southern Namoneas",
              y="% of TSTs read",
              x="Age Group") +  # title and caption
         theme_classic() +
         theme(text = element_text(family = 'Open Sans'),
               plot.background = element_rect(fill = "#f3f6f8"), 
               panel.background = element_rect(fill = "#f3f6f8", colour="#f3f6f8"), 
               panel.border = element_blank(),
               legend.position="none",
               legend.background = element_rect(fill = "#f3f6f8"),
               legend.title = element_blank(),
               plot.margin = unit(c(.2,.2,.2,.2), "cm")) +# turn off minor 
         scale_fill_tableau(palette = "Superfishel Stone") +
         coord_cartesian(ylim = c(0, 1)) +
         scale_y_continuous(labels = percent),
       "Figures/TST pos rate by age group - SN.png",
       width = 1280, height = 675, units = "px", scale = 1.5, dpi=300)
