#load
library(tidyverse)
library(phyloseq)
library(microbiome)

library(ggVennDiagram)

#### Load data ####
load("ivf_phyloseq.RData") #unrarafied object, analysis take vary seq depth into consideration already

# Convert raw read counts to relative abundance. counts -> percentage, so more comparable
phyloseq_RA <- transform_sample_counts(ivf_phyloseq, fun=function(x) x/sum(x))

#### "core" microbiome of outcome (successful or unsuccesful) ####

# Filter dataset by outcome
phyloseq_successful_outcome <- subset_samples(phyloseq_RA, `outcome`=="successful")
phyloseq_unsuccesful_outcome <- subset_samples(phyloseq_RA, `outcome`=="unsuccessful")

# core microbiome calculation
  # detection: ASV must have relative abundance greater than 0; must be present to be considered
  # prevalence: ASV must be present in at least xx% of the sample
  #tried that the higehst prevalence is 0.4
successful_ASVs <- core_members(phyloseq_RA, detection=0, prevalence = 0.1)
unsuccessful_ASVs <- core_members(phyloseq_RA, detection=0, prevalence = 0.1)

# retrieve taxonomic classifications of the ASVs identified as core members
tax_table(prune_taxa(successful_ASVs, ivf_phyloseq))
tax_table(prune_taxa(unsuccessful_ASVs, ivf_phyloseq))


# can plot those ASVs' relative abundance
prune_taxa(unsuccessful_ASVs,phyloseq_RA) %>% 
  plot_bar(fill="Genus")
#  facet_wrap(.~`outcome`, scales ="free") #x-axis don't depend on each other
#less groups from antibiotic group

# Notice that in this dataset, there are very few CORE microbiome members. This is common
### What if we wanted to make a Venn diagram of all the ASVs that showed up in each treatment?
 #detection more tha 0.001 (0.1%)
successful_list <- core_members(phyloseq_successful_outcome, detection=0.001, prevalence = 0.1)
unsuccessful_list <- core_members(phyloseq_unsuccesful_outcome, detection=0.001, prevalence = 0.1)

outcome_list_full <- list(S= successful_list, U= unsuccessful_list)


# Create a Venn diagram using all the ASVs 
first_venn_outcome <- ggVennDiagram(x = outcome_list_full)

ggsave("venn_outcome", first_venn)


# Get the taxonomy table from your original phyloseq object
taxonomy <- as.data.frame(tax_table(ivf_phyloseq))

# Add ASV names as rownames (makes subsetting easier)
taxonomy$ASV <- rownames(taxonomy)

# Get unique ASVs for successful outcome
unique_successful_ASVs <- setdiff(successful_list, unsuccessful_list)

# Get unique ASVs for unsuccessful outcome
unique_unsuccessful_ASVs <- setdiff(unsuccessful_list, successful_list)

# Create tables of unique ASVs and their taxonomy
successful_taxa <- taxonomy[taxonomy$ASV %in% unique_successful_ASVs, ]
unsuccessful_taxa <- taxonomy[taxonomy$ASV %in% unique_unsuccessful_ASVs, ]


# Save successful_taxa to a CSV file
write.csv(successful_taxa, file = "successful_taxa.csv", row.names = FALSE)

# Save unsuccessful_taxa to a CSV file
write.csv(unsuccessful_taxa, file = "unsuccessful_taxa.csv", row.names = FALSE)



#### "core" microbiome of disease ####

# Filter dataset by disease
phyloseq_no_pregnancy <- subset_samples(phyloseq_RA, `disease`=="No pregnancy")
phyloseq_ongoing_preg <- subset_samples(phyloseq_RA, `disease`=="Ongoing pregnancy")
phyloseq_live_birth <- subset_samples(phyloseq_RA, `disease`=="Live birth")
phyloseq_biochem_preg <- subset_samples(phyloseq_RA, `disease`=="Biochemical pregnancy")
phyloseq_clinical_miscarriage <- subset_samples(phyloseq_RA, `disease`=="Clinical miscarriage")

# core microbiome calculation
# detection: ASV must have relative abundance greater than 0; must be present to be considered
# prevalence: ASV must be present in at least xx% of the sample
#tried that the higehst prevalence is 0.4
no_pregnancy_ASVs <- core_members(phyloseq_no_pregnancy, detection=0, prevalence = 0.1)
ongoing_preg_ASVs <- core_members(phyloseq_ongoing_preg, detection=0, prevalence = 0.1)
live_birth_ASVs <- core_members(phyloseq_live_birth, detection=0, prevalence = 0.1)
biochem_preg_ASVs <- core_members(phyloseq_biochem_preg, detection=0, prevalence = 0.1)
clinical_miscarriage_ASVs <- core_members(phyloseq_clinical_miscarriage, detection=0, prevalence = 0.1)


# retrieve taxonomic classifications of the ASVs identified as core members
tax_table(prune_taxa(no_pregnancy_ASVs, ivf_phyloseq))
tax_table(prune_taxa(ongoing_preg_ASVs, ivf_phyloseq))
tax_table(prune_taxa(live_birth_ASVs, ivf_phyloseq))
tax_table(prune_taxa(biochem_preg_ASVs, ivf_phyloseq))
tax_table(prune_taxa(clinical_miscarriage_ASVs, ivf_phyloseq))


# Notice that in this dataset, there are very few CORE microbiome members. This is common
### What if we wanted to make a Venn diagram of all the ASVs that showed up in each treatment?
#detection more tha 0.001 (0.1%)
no_pregnancy_list <- core_members(phyloseq_no_pregnancy, detection=0.001, prevalence = 0.1)
on_going_list <- core_members(phyloseq_ongoing_preg, detection=0.001, prevalence = 0.1)
live_birth_list <- core_members(phyloseq_live_birth, detection=0.001, prevalence = 0.1)
biochem_preg_list <- core_members(phyloseq_biochem_preg, detection=0.001, prevalence = 0.1)
clinical_miscarriage_list <- core_members(phyloseq_clinical_miscarriage, detection=0.001, prevalence = 0.1)

outcome_list_full <- list(no_preg = no_pregnancy_list, 
                          on_going = on_going_list,
                          live_birth = live_birth_list,
                          biochem_preg = biochem_preg_list,
                          clinical_miscarriage = clinical_miscarriage_list)


# Create a Venn diagram using all the ASVs shared and unique to antibiotic users and non users
first_venn <- ggVennDiagram(x = outcome_list_full)

ggsave("venn_outcome", first_venn)


# Get all core member lists
all_lists <- list(no_pregnancy_list, 
                  on_going_list, 
                  live_birth_list, 
                  biochem_preg_list, 
                  clinical_miscarriage_list)
names(all_lists) <- c("no_pregnancy", "ongoing_pregnancy", "live_birth", "biochem_pregnancy", "clinical_miscarriage")

# Create a list to store the unique ASVs for each group
unique_asvs_per_group <- list()

# Loop through each group
for (group_name in names(all_lists)) {
  # Get the core members for the current group
  current_group_list <- all_lists[[group_name]]
  
  # Get the core members for all OTHER groups
  other_groups_lists <- all_lists[names(all_lists) != group_name]
  other_groups_combined <- unlist(other_groups_lists)
  
  # Find the unique ASVs for the current group
  unique_asvs <- setdiff(current_group_list, other_groups_combined)
  
  # Store the unique ASVs in the list
  unique_asvs_per_group[[group_name]] <- unique_asvs
}

# Example: Print the unique ASVs for "No pregnancy"
print(unique_asvs_per_group$no_pregnancy)

# Get the taxonomy table from your original phyloseq object
taxonomy <- as.data.frame(tax_table(ivf_phyloseq))

# Add ASV names as rownames (makes subsetting easier)
taxonomy$ASV <- rownames(taxonomy)

# Loop through the groups and create the tables
taxa_tables_list <- list()

for (group_name in names(unique_asvs_per_group)) {
  unique_asvs <- unique_asvs_per_group[[group_name]]
  taxa_table <- taxonomy[taxonomy$ASV %in% unique_asvs, ]
  taxa_tables_list[[group_name]] <- taxa_table # Store the table in the list
  write.csv(taxa_table, paste0("taxa_table_", group_name, ".csv")) # Save to CSV (optional)
}

# Access the tables from the list
View(taxa_tables_list$no_pregnancy)
View(taxa_tables_list$ongoing_pregnancy)
View(taxa_tables_list$live_birth)
View(taxa_tables_list$biochem_pregnancy)
View(taxa_tables_list$clinical_miscarriage)


#### "core" microbiome of agegroup #### subset to successful outcome

# 1. Define Age Groups 
age_groups <- c("26-30", "31-35", "36-40", "41-45", "46-50")

#     Run this and CHECK the output CAREFULLY.
table(sample_data(phyloseq_RA)$age_group)


# Filter dataset by age group (creating separate phyloseq objects)

s_phyloseq_26_30 <- subset_samples(phyloseq_successful_outcome, age_group == "26-30")
s_phyloseq_31_35 <- subset_samples(phyloseq_successful_outcome, age_group == "31-35")
s_phyloseq_36_40 <- subset_samples(phyloseq_successful_outcome, age_group == "36-40")
s_phyloseq_41_45 <- subset_samples(phyloseq_successful_outcome, age_group == "41-45")
s_phyloseq_46_50 <- subset_samples(phyloseq_successful_outcome, age_group == "46-50")

# Calculate core microbiome for each age group (higher prevalence)
s_age_26_30_ASVs <- core_members(s_phyloseq_26_30, detection = 0, prevalence = 0.4)
s_age_31_35_ASVs <- core_members(s_phyloseq_31_35, detection = 0, prevalence = 0.4)
s_age_36_40_ASVs <- core_members(s_phyloseq_36_40, detection = 0, prevalence = 0.4)
s_age_41_45_ASVs <- core_members(s_phyloseq_41_45, detection = 0, prevalence = 0.4)
s_age_46_50_ASVs <- core_members(s_phyloseq_46_50, detection = 0, prevalence = 0.4)


# Calculate core microbiome for each age group (lower prevalence - for Venn diagram)
s_age_26_30_list <- core_members(s_phyloseq_26_30, detection = 0.001, prevalence = 0.1)
s_age_31_35_list <- core_members(s_phyloseq_31_35, detection = 0.001, prevalence = 0.1)
s_age_36_40_list <- core_members(s_phyloseq_36_40, detection = 0.001, prevalence = 0.1)
s_age_41_45_list <- core_members(s_phyloseq_41_45, detection = 0.001, prevalence = 0.1)
s_age_46_50_list <- core_members(s_phyloseq_46_50, detection = 0.001, prevalence = 0.1)

# Create a list for the Venn diagram
s_age_list_full <- list("26-30" = s_age_26_30_list,
                      "31-35" = s_age_31_35_list,
                      "36-40" = s_age_36_40_list,
                      "41-45" = s_age_41_45_list,
                      "46-50" = s_age_46_50_list)

# Create and save the Venn diagram
s_age_venn <- ggVennDiagram(x = s_age_list_full)
ggsave("venn_age.png", age_venn) # Or .pdf


# Get all core member lists
s_all_age_lists <- list(s_age_26_30_list, s_age_31_35_list, s_age_36_40_list, s_age_41_45_list, s_age_46_50_list)
names(s_all_age_lists) <- age_groups # Use the defined age_groups vector

# Create a list to store the unique ASVs
s_unique_asvs_per_age_group <- list()

# Loop through each group
for (group_name in names(s_all_age_lists)) {
  s_current_group_list <- s_all_age_lists[[group_name]]
  s_other_groups_lists <- s_all_age_lists[names(s_all_age_lists) != group_name]
  s_other_groups_combined <- unlist(s_other_groups_lists)
  s_unique_asvs <- setdiff(s_current_group_list, s_other_groups_combined)
  s_unique_asvs_per_age_group[[group_name]] <- s_unique_asvs
}


# Get the taxonomy table
s_taxonomy <- as.data.frame(tax_table(ivf_phyloseq))
s_taxonomy$ASV <- rownames(taxonomy)

# Create an empty list to store the tables
s_taxa_tables_list <- list()

# Loop and create tables
for (group_name in names(s_unique_asvs_per_age_group)) {
  s_unique_asvs <- s_unique_asvs_per_age_group[[group_name]]
  s_taxa_table <- s_taxonomy[s_taxonomy$ASV %in% s_unique_asvs, ]
  s_taxa_tables_list[[group_name]] <- s_taxa_table
  write.csv(s_taxa_table, paste0("s_taxa_table_", group_name, ".csv")) # Save to CSV (optional)
}


# View the tables 
View(s_taxa_tables_list$`26-30`)
View(s_taxa_tables_list$`31-35`)
View(s_taxa_tables_list$`36-40`)
View(s_taxa_tables_list$`41-45`)
View(s_taxa_tables_list$`46-50`)


#### "core" microbiome of agegroup #### subset to UNsuccessful outcome

# 1. Define Age Groups 
age_groups <- c("26-30", "31-35", "36-40", "41-45", "46-50")

#     Run this and CHECK the output CAREFULLY.
table(sample_data(phyloseq_RA)$age_group)


# Filter dataset by age group (creating separate phyloseq objects)

u_phyloseq_26_30 <- subset_samples(phyloseq_unsuccesful_outcome, age_group == "26-30")
u_phyloseq_31_35 <- subset_samples(phyloseq_unsuccesful_outcome, age_group == "31-35")
u_phyloseq_36_40 <- subset_samples(phyloseq_unsuccesful_outcome, age_group == "36-40")
u_phyloseq_41_45 <- subset_samples(phyloseq_unsuccesful_outcome, age_group == "41-45")
u_phyloseq_46_50 <- subset_samples(phyloseq_unsuccesful_outcome, age_group == "46-50")

# Calculate core microbiome for each age group (higher prevalence)
u_age_26_30_ASVs <- core_members(u_phyloseq_26_30, detection = 0, prevalence = 0.4)
u_age_31_35_ASVs <- core_members(u_phyloseq_31_35, detection = 0, prevalence = 0.4)
u_age_36_40_ASVs <- core_members(u_phyloseq_36_40, detection = 0, prevalence = 0.4)
u_age_41_45_ASVs <- core_members(u_phyloseq_41_45, detection = 0, prevalence = 0.4)
u_age_46_50_ASVs <- core_members(u_phyloseq_46_50, detection = 0, prevalence = 0.4)


# Calculate core microbiome for each age group (lower prevalence - for Venn diagram)
u_age_26_30_list <- core_members(u_phyloseq_26_30, detection = 0.001, prevalence = 0.1)
u_age_31_35_list <- core_members(u_phyloseq_31_35, detection = 0.001, prevalence = 0.1)
u_age_36_40_list <- core_members(u_phyloseq_36_40, detection = 0.001, prevalence = 0.1)
u_age_41_45_list <- core_members(u_phyloseq_41_45, detection = 0.001, prevalence = 0.1)
u_age_46_50_list <- core_members(u_phyloseq_46_50, detection = 0.001, prevalence = 0.1)

# Create a list for the Venn diagram
u_age_list_full <- list("26-30" = u_age_26_30_list,
                        "31-35" = u_age_31_35_list,
                        "36-40" = u_age_36_40_list,
                        "41-45" = u_age_41_45_list,
                        "46-50" = u_age_46_50_list)

# Create and save the Venn diagram
u_age_venn <- ggVennDiagram(x = u_age_list_full)
ggsave("venn_age.png", age_venn) # Or .pdf


# Get all core member lists
u_all_age_lists <- list(u_age_26_30_list, u_age_31_35_list, u_age_36_40_list, u_age_41_45_list, u_age_46_50_list)
names(u_all_age_lists) <- age_groups # Use the defined age_groups vector

# Create a list to store the unique ASVs
u_unique_asvs_per_age_group <- list()

# Loop through each group
for (group_name in names(u_all_age_lists)) {
  u_current_group_list <- u_all_age_lists[[group_name]]
  u_other_groups_lists <- u_all_age_lists[names(u_all_age_lists) != group_name]
  u_other_groups_combined <- unlist(u_other_groups_lists)
  u_unique_asvs <- setdiff(u_current_group_list, u_other_groups_combined)
  u_unique_asvs_per_age_group[[group_name]] <- u_unique_asvs
}


# Get the taxonomy table
u_taxonomy <- as.data.frame(tax_table(ivf_phyloseq))
u_taxonomy$ASV <- rownames(taxonomy)

# Create an empty list to store the tables
u_taxa_tables_list <- list()

# Loop and create tables
for (group_name in names(u_unique_asvs_per_age_group)) {
  u_unique_asvs <- u_unique_asvs_per_age_group[[group_name]]
  u_taxa_table <- u_taxonomy[u_taxonomy$ASV %in% u_unique_asvs, ]
  u_taxa_tables_list[[group_name]] <- u_taxa_table
  write.csv(u_taxa_table, paste0("u_taxa_table_", group_name, ".csv")) # Save to CSV (optional)
}


# View the tables 
View(u_taxa_tables_list$`26-30`)
View(u_taxa_tables_list$`31-35`)
View(u_taxa_tables_list$`36-40`)
View(u_taxa_tables_list$`41-45`)
View(u_taxa_tables_list$`46-50`)


#### "core" microbiome of agegroup + outcome ####


#     Run this and CHECK the output CAREFULLY.
table(sample_data(phyloseq_RA)$age_outcome)

# 2. Define age_outcome groups (or create dynamically)
# OPTION A: Manually define (if you know all the combinations)
age_outcome_groups <- c("26-30_successful", "26-30_unsuccessful",
                        "31-35_successful", "31-35_unsuccessful",
                        "36-40_successful", "36-40_unsuccessful",
                        "41-45_successful", "41-45_unsuccessful",
                        "46-50_successful", "46-50_unsuccessful")


#Create empty list for phyloseq objects
phyloseq_objects <- list()

#Create a phyloseq object for each group
for (group in age_outcome_groups){
  phyloseq_objects[[group]] <- subset_samples(phyloseq_RA, age_outcome == group)
}

# Calculate core microbiome for each age-outcome group (higher prevalence)
core_asvs_high_prevalence <- list()
for (group in age_outcome_groups){
  core_asvs_high_prevalence[[group]] <- core_members(phyloseq_objects[[group]], detection = 0, prevalence = 0.4)
}

# Retrieve taxonomic classifications (optional - for checking)
for (group in age_outcome_groups) {
  tax_table(prune_taxa(core_asvs_high_prevalence[[group]], ivf_phyloseq))
}

# Calculate core microbiome for each age-outcome group (lower prevalence - for Venn diagram)
core_asvs_low_prevalence <- list()
for (group in age_outcome_groups){
  core_asvs_low_prevalence[[group]] <- core_members(phyloseq_objects[[group]], detection = 0.001, prevalence = 0.1)
}

# Create a list for the Venn diagram
# You *might* not want a Venn diagram with ALL age-outcome groups (too many!)
# Consider creating Venn diagrams for *subsets* of groups (e.g., within each age group)


# Get all core member lists
all_age_outcome_lists <- core_asvs_low_prevalence # Use the low-prevalence core ASVs
names(all_age_outcome_lists) <- age_outcome_groups

# Create a list to store the unique ASVs
unique_asvs_per_age_outcome_group <- list()

# Loop through each group
for (group_name in names(all_age_outcome_lists)) {
  current_group_list <- all_age_outcome_lists[[group_name]]
  other_groups_lists <- all_age_outcome_lists[names(all_age_outcome_lists) != group_name]
  other_groups_combined <- unlist(other_groups_lists)
  unique_asvs <- setdiff(current_group_list, other_groups_combined)
  unique_asvs_per_age_outcome_group[[group_name]] <- unique_asvs
}


# Get the taxonomy table
taxonomy <- as.data.frame(tax_table(ivf_phyloseq))
taxonomy$ASV <- rownames(taxonomy)

# Create an empty list to store the tables
taxa_tables_list <- list()

# Loop and create tables
for (group_name in names(unique_asvs_per_age_outcome_group)) {
  unique_asvs <- unique_asvs_per_age_outcome_group[[group_name]]
  taxa_table <- taxonomy[taxonomy$ASV %in% unique_asvs, ]
  taxa_tables_list[[group_name]] <- taxa_table
  write.csv(taxa_table, paste0("taxa_table_", group_name, ".csv")) # Save to CSV (optional)
  
}


# View the tables
View(taxa_tables_list$`26-30_successful`)
View(taxa_tables_list$`26-30_unsuccessful`)
View(taxa_tables_list$`31-35_successful`)
View(taxa_tables_list$`31-35_unsuccessful`)
View(taxa_tables_list$`36-40_successful`)
View(taxa_tables_list$`36-40_unsuccessful`)
View(taxa_tables_list$`41-45_successful`)
View(taxa_tables_list$`41-45_unsuccessful`)
View(taxa_tables_list$`46-50_successful`)
View(taxa_tables_list$`46-50_unsuccessful`)


