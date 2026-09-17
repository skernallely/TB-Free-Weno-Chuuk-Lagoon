## TB-Free Chuuk R code
## Hybrid survey data management

#PACKAGES
library(tidyverse) 
library(readxl)
library(lubridate)
library(stringr)

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

# Define the correct pattern: XXMMDDYYA## (2 letters + 6 digit date + A + 2 digits)
full_pattern <- "^[A-Z]{2}[0-9]{6}A[0-9]{2}$"
missing_zero_pattern <- "^[A-Z]{2}[0-9]{6}A[0-9]$"

#read in hybrid data and clean up the ids for standardization
hybrid <- read_excel(here::here("Data","raw","Chuuk_Hybrid_2024_data_2JAN2024.xlsx"),
                     guess_max = 20000, col_names = TRUE) %>%
  clean_names() %>%
  mutate(hhid = if_else(grepl("^A",hhid),toupper(str_squish(str_replace(hhid, "A", ""))),
                         toupper(str_squish(str_replace(hhid, "[[:punct:]]", ""))) )
         ) %>%
  select(-clean_id) |>
  select(interview_date,hhid,everything())


#make cleaning dataset for the hybrids that don't have nice standardized ids
# cleaning_data_set <- hybrid |>  filter(is.na(hhid_standardized))
# write.xlsx(cleaning_data_set, here::here("Data","raw","hybrid_id_fixing.xlsx"))

#read back in cleaned ids from cleaning dataset
cleaned_ids <- read_xlsx(here::here("Data","raw","hybrid_id_fixing.xlsx")) |>
  select(interview_date,hhid,corrected_hhid)

# merge cleaning dataset back into the hybrid data and standardize onsite IDs to format in flatfiles
clean_hybrid <- hybrid |>
  left_join(cleaned_ids, by=c("hhid","interview_date")) |>
  mutate(hhid = case_when(is.not.na(corrected_hhid) ~ corrected_hhid,
                          .default=hhid),

    # Step 1: Set prefix conditionally: LG if island is in list, otherwise WN
    prefix = ifelse(island == "Weno", "WN", "LG"),
    
    # Step 2: Convert date column to MMDDYY format (assumes Date or POSIXct class)
    date_formatted = format(as.Date(interview_date), "%m%d%y"),
    
    # Step 3: Extract the suffix number from the hhid column
    # If hhid is only numbers (e.g., "12" or "5"), use it directly
    # If hhid has full pattern (e.g., "AB011525A12"), extract the number after "A"
    suffix_extracted = case_when(
      str_detect(hhid, "^[0-9]+$") ~ str_trim(hhid),  # Only numbers, extract as-is
      str_detect(hhid, "A[0-9]+$") ~ str_extract(hhid, "(?<=A)[0-9]+"),  # Has "A" prefix, extract what follows
      TRUE ~ str_trim(hhid)  # Fallback - trim whitespace
    ),
    
    # Step 4: Pad suffix to always be 2 digits (adds leading zero if needed)
    suffix_padded = str_pad(suffix_extracted, width = 2, side = "left", pad = "0"),
    
    # Step 5: Build the complete standardized ID from scratch
    hhid_standardized = paste0(prefix,"-", date_formatted, "-A", suffix_padded),
    hhid_standardized = ifelse(
      nchar(hhid_standardized) > 14,
      NA_character_,
      hhid_standardized
    )
  ) |>
  select(interview_date,hhid_standardized,everything())
  
#export dataset for matching with flatfile for hybrid data
write.xlsx(clean_hybrid, here::here("Data","matching","hybrid_for_matching.xlsx"))
