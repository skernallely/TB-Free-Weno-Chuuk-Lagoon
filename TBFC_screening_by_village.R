## TB-Free Chuuk R code
## Village Analysis
## questions: population by village, screening by village tables
##  which villages had more than 30 people screened?

#PACKAGES
library(tidyverse) #pipes, stringr, lubridate, scales
library(readxl) #excel load-in

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

#standards
lagoon_list <- c("WENO","PAATA","ONEI","TOL","POLLE","UDOT","FEFEN",
                 "UMAN", "TONOAS")


#Villages where >=30 people were screened during the project
village_names <- read_excel("Data/flatfile.xlsx",
                            guess_max = 20000, col_names = TRUE) %>%
  filter(is.not.na(registration_id) & is.not.na(patient_passport_id) & is.not.na(registration_no)) %>%
  select(registration_no, municipality, village, village_ltbi, island_ltbi) %>%
  mutate(area = if_else(municipality == "WENO" | island_ltbi == "WENO", "Weno", 
                        "Lagoon", missing = "Lagoon"),
         municipality = str_to_upper(municipality),
         village = str_to_upper(village),
         village = if_else(grepl("KUCHUWA",village), "KUCHUWA",
                           if_else(grepl("NANTAKU",village), "NEPUKOS",
                                   village)
         )
                           
  ) %>%
  group_by(municipality,village) %>%
  summarise(
    #no of people by village
    count = n()
  ) %>%
  filter(count > 29)

villages_screened_30_plus <- paste(village_names$municipality,village_names$village)
