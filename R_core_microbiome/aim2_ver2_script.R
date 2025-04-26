# Goal: Aim2 -  Categorize the endometrial microbiota and microbial diversity in relation to 
# age groups and their corresponding reproductive success


# Load libraries
library(tidyverse)
library(phyloseq)
library(ggplot2)
library(vegan)
library(DESeq2)
library(ape)
library(indicspecies)

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


save(ivf_final, file="ivf_final.RData")

#### Indicator Speices Analysis/Taxa Analysis ####

# group OTUs to the genus level
#group data based on specific taxanomic rank: Genus, don't want to remove NA
mpt_genus <- tax_glom(ivf_phyloseq, "Genus", NArm = FALSE)
#convert counts from otu table from absolute to relative
mpt_genus_RA <- transform_sample_counts(mpt_genus, fun=function(x) x/sum(x))

##ISA based on agegroup + outcome
#tanspose otu table, cluster is predictor
isa_mpt_age_outcome <- multipatt(t(otu_table(mpt_genus_RA)), cluster = sample_data(mpt_genus_RA)$`age_outcome`)
summary(isa_mpt_age_outcome)
#stat closer to 1 means its a better indicator
taxtable_age_outcome <- tax_table(ivf_phyloseq) %>% as.data.frame() %>% rownames_to_column(var="ASV")

# consider that your table is only going to be resolved up to the genus level, be wary of 
# anything beyond the glomed taxa level
#at p=0.05
isa_mpt_age_outcome$sign %>%
  rownames_to_column(var="ASV") %>%
  left_join(taxtable_age_outcome) %>%
  filter(p.value<0.05) %>% View()

#at p=0.1
isa_mpt_age_outcome$sign %>%
  rownames_to_column(var="ASV") %>%
  left_join(taxtable_age_outcome) %>%
  filter(p.value<0.1) %>% View()

