# Goal: Aim1 -  Categorize the endometrial microbiota and microbial diversity in relation to 
# age groups and their corresponding reproductive success

# Load libraries
library(tidyverse)
library(phyloseq)
library(ggplot2)
library(vegan)
library(DESeq2)
library(ape)
library(picante)
library(lme4)
library(ggeffects)


#### Binning Reproductive outcome and age ####

# Load metadata
ivf_metaFP <- "../data_files/IVF_metadata.tsv"
ivf_meta <- read_delim(ivf_metaFP)

# Update metadate file to only include: sample-id, AGE, disease columns
ivf_meta_updated <- ivf_meta %>%
  select(`sample-id`, AGE, disease)

# Create new column named "outcome" 
# filter out NA in "disease" column
# successful if "disease" = Live birth, Ongoing pregnancy 
# unsuccessful if "disease" = Clinical miscarriage, Biochemical pregnancy, No pregnancy

ivf_meta_updated <- ivf_meta_updated %>%
  filter(disease != "NA: Not Applicable") %>%
  mutate(outcome = ifelse(disease %in% c("Live birth", 
                                         "Ongoing pregnancy"),
                          "successful",
                          ifelse(disease %in% c("Clinical miscarriage",
                                                "Biochemical pregnancy",
                                                "No pregnancy", 
                                                "Ectopic pregnancy"),
                                 "unsuccessful", NA)))

# In AGE column filter out AGE <= 25
# Bin samples into the following age groups: 26-30, 31-35, 36-40, 41-45, 46-50
ivf_meta_updated <- ivf_meta_updated %>%
  filter(AGE > 25) %>%
  mutate(age_group = cut(AGE,
                         breaks = c(25, 30, 35, 40, 45, 50),
                         labels = c("26-30", "31-35", "36-40", "41-45", "46-50"),
                         right = TRUE)) # specifies that the right age boundary should be included


# Create a new metadata column that combines age_group and outcome in age_outcome
ivf_meta_updated <- ivf_meta_updated %>%
  mutate(age_outcome = paste(age_group, outcome, sep = "_"))

#### Creating phyloseq object ####
# Load feature table, taxonomy, tree 
otufp <- "../data_files/ivf_export/ivf-table_export/feature-table.txt"
ivf_otu <- read_delim(file = otufp, delim = "\t", skip = 1)

taxfp <- "../data_files/ivf_export/ivf_taxonomy_export/taxonomy.tsv"
ivf_tax <- read_delim(taxfp)

phylotreefp <- "../data_files/ivf_export/ivf-rooted-tree_export/tree.nwk"
ivf_phylotree <- read.tree(phylotreefp)

## Format OTU table
# with rownames and colnames as OTUs and sampleIDs, respectively
ivf_otu_mat <- as.matrix(ivf_otu[,-1])
# Make the first column (#OTU ID) the rownames of the new matrix
rownames(ivf_otu_mat) <- ivf_otu$'#OTU ID'
OTU <- otu_table(ivf_otu_mat, taxa_are_rows = TRUE)
class(OTU)

## Format ivf_meta_updated
# Save everything except sampleid as new data frame
samp_df <- as.data.frame(ivf_meta_updated[,-1])
# Make sampleids the rownames
rownames(samp_df) <- ivf_meta_updated$'sample-id'
# Make phyloseq sample data with sample_data() function
META <- sample_data(samp_df)
class(META)

## Format taxonomy
# Conver taxon strings to a table with separate taxa rank columns
tax_mat <- ivf_tax %>% select(-Confidence) %>%
  separate(col=Taxon, sep="; "
           , into = c("Domain","Phylum","Class","Order","Family","Genus","Species")) %>%
  as.matrix()
tax_mat <- tax_mat[,-1]
# Make sampleids the rownames
rownames(tax_mat) <- ivf_tax$'Feature ID'
# Make taxa table
TAX <- tax_table(tax_mat)
class(TAX)

## Create phyloseq object
ivf_phyloseq <- phyloseq(OTU, META, TAX, ivf_phylotree)

## Filtering and Rarefying phyloseq object
# Remove non-bacterial sequences, if any
ivf_final <- subset_taxa(ivf_phyloseq, Domain == "d__Bacteria"
                                 & Class!="c__Chloroplast"
                                 & Family !="f_Mitochondria")

# Adjust plot margins to make sure there is enough space for the plot
par(mar = c(4, 4, 2, 2))  # Adjust margins (bottom, left, top, right)

# Rarefy samples
rarecurve(t(as.data.frame(otu_table(ivf_final))), cex=0.1)
ivf_rare <- rarefy_even_depth(ivf_final, rngseed = 1, sample.size = 2500)
# Rarefaction removes 498 samples

#### Alpha Diversity #### 
## Shannon's Diversity
# Extract information 
alphadiv <- estimate_richness(ivf_rare)
samp_dat <- sample_data(ivf_rare)
samp_dat_wdiv <- data.frame(samp_dat, alphadiv)

# Add Shannon diversity to the metadata table
sample_data(ivf_rare)$Shannon <- alphadiv$Shannon

data <- samp_dat_wdiv

# Convert 'outcome' to a factor (if it's not already)
data$outcome <- factor(data$outcome, levels = c("successful", "unsuccessful"), 
                       labels = c("Successful", "Unsuccessful"))


# Fit the Linear Mixed Effects Model (LME)
lme_model <- lmer(Shannon ~ age_group + (1 | outcome), data = data)

# Create a new dataset with model predictions
data$predicted <- predict(lme_model, re.form = ~ (1 | outcome))  # Include random effects

# Print model summary
summary(lme_model)

# Plot: Regression lines for each outcome
shannon_lr <- ggplot(data, aes(x = AGE, y = Shannon, color = outcome)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = TRUE, aes(fill = outcome), alpha = 0.2) + 
  labs(title = "Shannon Diversity Across Age for Different IVF Outcomes",
       x = "Age",
       y = "Shannon Diversity", 
       color = "Outcome",
       fill = "Outcome") + 
  theme_minimal()

## Statistical Analysis Shannon's Diversity
# convert 'age_group' to numeric
data$age_group <- as.numeric(data$age_group)

# Spearman's correlation for each outcome separately
cor_results_shannon <- data %>%
  group_by(outcome) %>%
  summarise(spearman_cor = cor.test(age_group, Shannon, method = "spearman", exact = FALSE)$estimate,
            p_value = cor.test(age_group, Shannon, method = "spearman", exact = FALSE)$p.value)

# Print correlation results per outcome
print(cor_results_shannon)

ggsave("Shannon_linear_reg.png",
       shannon_lr,
       height = 4,
       width = 6)


## Faith's PD
phylo_dist <- pd(t(otu_table(ivf_rare)), phy_tree(ivf_rare),
                 include.root=F) 

# add PD to metadata table
sample_data(ivf_rare)$PD <- phylo_dist$PD

# Extract information 
alphadiv <- estimate_richness(ivf_rare)
samp_dat <- sample_data(ivf_rare)
samp_dat_wdiv <- data.frame(samp_dat, alphadiv)

data <- samp_dat_wdiv

# Convert 'outcome' to a factor (if it's not already)
data$outcome <- factor(data$outcome, levels = c("successful", "unsuccessful"), 
                       labels = c("Successful", "Unsuccessful"))

# Fit the Linear Mixed Effects Model (LME)
lme_model <- lmer(PD ~ age_group + (1 | outcome), data = data)

# Create a new dataset with model predictions
data$predicted <- predict(lme_model, re.form = ~ (1 | outcome))  # Include random effects

# Print model summary
summary(lme_model)

# Plot: Regression lines for each outcome
faith_pd_lr <- ggplot(data, aes(x = AGE, y = PD, color = outcome)) +
  geom_point(alpha = 0.6) +  # Scatterplot
  geom_smooth(method = "lm", se = TRUE, aes(fill = outcome), alpha = 0.2) +  # Best fit line + confidence ribbon
  labs(title = "Faith’s PD Across Age for Different IVF Outcomes",
       x = "Age",
       y = "Faith's PD",
       color = "Outcome",
       fill = "Outcome") +
  theme_minimal()

## Statistical Analysis Faith's PD
# convert 'age_group' to numeric
data$age_group <- as.numeric(data$age_group)

# Spearman's correlation for each outcome separately
cor_results_fpd <- data %>%
  group_by(outcome) %>%
  summarise(spearman_cor = cor.test(age_group, PD, method = "spearman", exact = FALSE)$estimate,
            p_value = cor.test(age_group, PD, method = "spearman", exact = FALSE)$p.value)

# Print correlation results per outcome
print(cor_results_fpd)

ggsave("Faith_PD_linear_reg.png",
       faith_pd_lr,
       height = 4,
       width = 6)

#### Taxonomic Analysis ####
# Plot bar plot of taxonomy
plot_bar(ivf_rare, fill = "Genus")

# Convert absolute number to relative abundance 
ivf_RA <- transform_sample_counts(ivf_rare, function(x) x/sum(x))

# To remove black bars, "glom" by Genus first 
ivf_genus <- tax_glom(ivf_RA, taxrank = "Genus", NArm = FALSE)

# Ensure 'age_outcome' metadata column is still present in metadata after tax_glom
sample_data(ivf_species)$age_outcome <- sample_data(ivf_rare)$age_outcome
sample_data(ivf_genus)$outcome <- factor(sample_data(ivf_genus)$outcome, 
                                         levels = c("successful", "unsuccessful"),
                                         labels = c("Successful", "Unsuccessful"))

# Plot bar plot
tax_bar_plot <- plot_bar(ivf_genus, fill = "Genus") +
  facet_wrap(outcome ~ age_group, nrow = 2, ncol = 5, scales = "free_x") +  
  labs(x = "Samples", y = "Relative Abundance", title = "Taxonomic Composition by Age & Outcome") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 12))

ggsave("tax_composition_genus.png",
       tax_bar_plot,
       height = 10,
       width = 12)






