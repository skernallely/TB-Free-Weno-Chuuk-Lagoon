## TB-Free Chuuk R code
## Hybrid survey data

#PACKAGES
library(tidyverse) 
library(readxl)
library(openxlsx)
library(lubridate)
library(stringr)
library(vtable) #allows sumtable
library(janitor)

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

hybrid <- read_excel("Data/Chuuk_Hybrid_2024_data_2JAN2024.xlsx",
                     guess_max = 20000, col_names = TRUE) %>%
  clean_names() %>%
  mutate(hhid = if_else(grepl("^A",hhid),toupper(str_squish(str_replace(hhid, "A", ""))),
                         toupper(str_squish(str_replace(hhid, "[[:punct:]]", ""))) ),
         # clean_id = case_when(str_length(hhid) < 3 ~ paste0(format.Date(as_date(interview_date),"%m%d%y"),"A",str_remove(hhid, "^0+")),
         #                       .default = hhid),
         unique_id = paste0(hhid,village,surveyor_id1,gender,age)
         ) %>%
  select(-clean_id) %>%
  inner_join(read_excel("Data/Cleaning Files/hybrid_matching.xlsx",
             guess_max = 20000, col_names = TRUE) %>% select(unique_id,clean_id),
            by="unique_id")


write.xlsx(hybrid, "Data/Cleaning Files/hybrid_matching.xlsx")

View(hybrid)                           
