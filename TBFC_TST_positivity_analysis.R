## TB-Free Chuuk R code
## TST Positivity rate calculations by age group, sex and geographic location

#PACKAGES
library(tidyverse) #pipes
library(scales) #percents for graphs
library(readxl) #excel load-in
library(janitor) #allows tabyl & cleaning names
library(ggplot2) #make graphs
library(ggpubr) #special aggregate of plots
library(ggthemes) #makes prettier graphs
library(gridExtra) #tiled grid of plots
library(gtsummary) #allows summary tabyl and p-value

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

###FUNCTIONS

##make TST positivity graphs
make_tst_pos_graphs <- function(data, fill) {
  ggplot(data = data, 
         aes(x=age_group, y=pct, fill = {{fill}})) +
    geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
    labs(
      y="% of TSTs read",
      x="Age Group") +  # title and caption
    theme_classic() +
    theme(panel.background = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.background = element_blank(),
          legend.title = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
    scale_fill_tableau(palette = "Superfishel Stone") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(labels = percent) # add pct
}

#datasets
#subset of patients who had a tst placed
tst_dataset <- read_excel("Data/tbfc_analysis_dataset.xlsx",
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
  ) %>%
  filter(tst_read_yn != "No TST") %>%
  mutate(region = case_when(region %in% c("MORT",
                                          "NW") ~ "NORTHERN NAMONEAS",
                            .default = region))

#------------------------------

#TST POSITIVITY COUNTS and RATES

#number of people with TSTs placed
tst_dataset %>%
  count()

#number of people with TST results
tst_dataset %>%
  count(is.not.na(tst_result_10))

#number of people with TST positive >=10mm
tst_dataset %>%
  filter(is.not.na(tst_result_10)) %>%
  tabyl(tst_result_10)

#tst positivity by age group
tst_dataset %>%
  filter(is.not.na(tst_result_10)) %>%
  tabyl(age_group, tst_result_10) %>%
  adorn_totals() %>%
  adorn_percentages() %>%
  pivot_longer(cols=2:3, names_to="tst_result_10", values_to = "pct") %>%
  filter(tst_result_10 != "<10 mm TST" &
           age_group != "0-4" & is.not.na(age_group))

#tst positivity by sex
tst_dataset %>%
  filter(is.not.na(tst_result_10)) %>%
  tbl_summary(by = sex, include = c(tst_result_10),
              digits = ~ 1) %>%
  add_p()

#tst positivity by municipality
tst_dataset %>%
  filter(is.not.na(tst_result_10)) %>%
  tabyl(municipality, tst_result_10) %>%
  adorn_totals(c('row','col')) %>%
  filter(Total > 30) %>%
  adorn_percentages() %>%
  select(-Total) %>%
  pivot_longer(cols=2:3, names_to="tst_result_10", values_to = "pct") %>%
  filter(tst_result_10 != "<10 mm TST")


##positivity by sex, age_group and island
tst_dataset %>%
  filter(is.not.na(tst_result_10) & age_group != "0-4" & is.not.na(age_group) &
           municipality %in% toupper(island_labels$name)) %>%
  group_by(sex,municipality,age_group) %>%
  mutate(sex = case_when(sex == 'F' ~ 'Female',
                         sex == 'M' ~ 'Male',
                         .default = sex)
  ) %>%
  summarise(num_tst = n(), 
            tst_pos = sum(tst_result_10 == ">= 10 mm TST"),
            pct = tst_pos / num_tst) %>%
  arrange(desc(pct))

#-----------------------

##GRAPHS and FIGURES

#OVERALL TST POS RATE GRAPH
#tst positivity by age group for those 5+
tst_pos_rate <- tst_dataset %>%
  filter(is.not.na(tst_result_10)) %>%
  tabyl(age_group, tst_result_10) %>%
  adorn_percentages() %>%
  pivot_longer(cols=2:3, names_to="tst_result_10", values_to = "pct") %>%
  filter(age_group != "0-4") %>%
  filter(tst_result_10 != "<10 mm TST")

##basic TST positivity plot
basic_age_pos <- make_tst_pos_graphs(tst_pos_rate, tst_result_10) +
         theme(legend.position="none") + 
         coord_cartesian(ylim = c(0, .5)) +
         scale_y_continuous(name="% of TSTs read >= 10 mm",
                            labels = percent)

#tst positivity by age group and sex
tst_pos_age_sex <- tst_dataset %>%
  filter(is.not.na(tst_result_10) & age_group != "0-4" & is.not.na(age_group)) %>%
  group_by(sex,age_group) %>%
  mutate(sex = case_when(sex == 'F' ~ 'Female',
                         sex == 'M' ~ 'Male',
                         .default = sex)
         ) %>%
  summarise(num_tst = n(), 
            tst_pos = sum(tst_result_10 == ">= 10 mm TST"),
            pct = tst_pos / num_tst)

##make graph by sex
basic_sex_pos <- make_tst_pos_graphs(tst_pos_age_sex,"none") +
  facet_wrap('sex') +
  theme(legend.position="none") + 
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_y_continuous(name="% of TSTs read >= 10 mm",
                     labels = percent)


#make graph stack for overall positivity by age and sex
ggsave(plot=ggarrange(basic_age_pos,
                      basic_sex_pos, 
                      heights = c(1,1),
                      font.label= list("plain","black",size=12),
                      common.legend = TRUE,
                      label.x = 0,
                      legend = "none",
                      nrow = 2),
       "Figures/Figure 2 - TST positivity rate by age group and sex.png",
       width = 1080, height = 1280, units = "px", scale = 2, dpi=300)

##make graph by sex grouped
grouped_sex_pos <-
  ggplot(data = tst_pos_age_sex, 
                            aes(x=age_group, y=pct, fill = sex)) +
  geom_bar(stat="identity",position = "dodge") +
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.position="bottom",
        legend.background = element_blank(),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  + # turn off minor 
    labs(
      x="Age group (years)",
      fill = "Sex") +  # title and caption
  scale_fill_manual(values=c("#255683","#b6d3ff")) +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_y_continuous(name="% of tuberculin skin tests read >= 10 mm",
                     labels = percent)

#Save grouped bar chart with positivity by age and sex
ggsave(plot=grouped_sex_pos,
       "Figures/Figure 2 - Grouped TST positivity rate by age and sex.png",
       width = 1280, height = 720, units = "px", scale = 2, dpi=300)

#---------------------------

##ADDITIONAL EXPLORATORY GRAPHS

#make graph for tst positivity by age group
tst_pos_age_g <- make_tst_pos_graphs(tst_pos_rate,tst_result_10)

#TST POS RATE FOR LAGOON
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
tst_pos_age_lagoon_g <- make_tst_pos_graphs(tst_pos_rate_lagoon,tst_result_10) +
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

#TST POS RATE FOR VILLAGE
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
                                                   filter(region == "WENO"),
                                                 tst_result_10) +
  facet_wrap( ~ full_village_name, scales='free') +
  labs(
    title = "LTBI Treatment Cascade for TB-Free Chuuk, 2023",
    subtitle = "Age >= 5 y.o., Weno"
  )

tst_pos_rate_village_faichuuk <- make_tst_pos_graphs(tst_pos_rate_village %>%
                                                   filter(region == "FAICHUUK"),
                                                   tst_result_10) +
  facet_wrap( ~ full_village_name, scales='free') +
  labs(
    subtitle = "Faichuuk"
  )

tst_pos_rate_village_uman_fefen <- make_tst_pos_graphs(tst_pos_rate_village %>%
                                                       filter(region == "SOUTHERN NAMONEAS" &
                                                                municipality != "TONOAS"),
                                                       tst_result_10) +
  facet_wrap( ~ full_village_name, scales='free')  +
  labs(
    subtitle = "Uman & Fefen"
  )

tst_pos_rate_village_tonoas <- make_tst_pos_graphs(tst_pos_rate_village %>%
                                                         filter(municipality == "TONOAS"),
                                                   tst_result_10) +
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

#GRAPH FOR EACH REGION
make_tst_pos_graphs(tst_pos_rate_region %>% filter(region == "FAICHUUK"),tst_result_10) +
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

