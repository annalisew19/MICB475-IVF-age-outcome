library(readr)
library(tidyverse)
# Load metadata
metaFP <- "IVF_metadata.tsv"
meta <- read_delim(file = metaFP, delim = "\t")

#  Update to include only: sample-id, AGE, disease columns
meta_select <- meta %>%
  select(`sample-id`, AGE, disease)

# Create new column pregnancy_success with Successful ("Live birth", "Ongoing pregnancy") and Unsuccessful ("No pregnancy", "Biochemical pregnancy", "Clinical miscarriage","Ectopic pregnancy")
IVF_metadata_status <- meta_select %>%
  mutate(pregnancy_success = case_when(
    disease %in% c("Live birth", "Ongoing pregnancy") ~ "Successful",
    disease %in% c("No pregnancy", "Biochemical pregnancy", "Clinical miscarriage","Ectopic pregnancy") ~ "Unsuccessful",
    TRUE ~ NA_character_  # Handle any other unexpected values
  )) #%>% filter(!is.na(pregnancy_success))  # Filter out rows where pregnancy_success is NA

# Filter out AGE > 25
# Create new column age_group with "26-30", "31-35", "36-40", "41-45", "46-50"
IVF_metadata_status_age <- IVF_metadata_status %>%
  filter(AGE > 25) %>%
  mutate(age_group = case_when(
    AGE >= 26 & AGE <= 30 ~ "26-30",
    AGE >= 31 & AGE <= 35 ~ "31-35",
    AGE >= 36 & AGE <= 40 ~ "36-40",
    AGE >= 41 & AGE <= 45 ~ "41-45",
    AGE >= 46 & AGE <= 50 ~ "46-50",
    TRUE ~ "Other"  # Assign "Other" to ages outside the specified range
  )) #%>% filter(!is.na(age_group))  # Filter out rows where pregnancy_success is NA


# Create combined column age_outcome with age_group and pregnancy_success
IVF_metadata_combined <- IVF_metadata_status_age %>%
  mutate(age_outcome = if_else(is.na(age_group) | is.na(pregnancy_success), 
                               NA_character_, 
                               paste(age_group, pregnancy_success, sep = "_")))

#Saving
write_csv(IVF_metadata_combined, file = "IVF_metadata_combined.csv")
write_tsv(IVF_metadata_combined, file = "IVF_metadata_combined.tsv")