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
tst_dataset <- read_excel("Data/tbfc_analysis_dataset.xlsx",
                          guess_max = 20000, col_names = TRUE) %>%
  mutate(age_group_new = case_when(age_group == "5-9" | age_group == "10-19" ~ "5-19",
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
  filter(tst_read_yn != "No TST")

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

#GRAPH FOR EACH LAGOON MUNI
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
