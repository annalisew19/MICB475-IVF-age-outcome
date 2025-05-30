# Random Forest Model 

# Based on this guide: https://www.r-bloggers.com/2021/04/random-forest-in-r/

# Load library
library(randomForest)
library(caret)
library(readr)
library(tidyverse)
library(dplyr)


# Load in data that has the ASVs as columns with their counts and the metadata column of interest per sample but remove sample_id

IVF_meta <- read.delim("IVF_metadata_combined.tsv", sep="\t", row.names=1)
IVF_OTU <- read.delim("feature-table.txt", sep="\t", skip=1, row.names=1)

t_IVF_OTU <- t(IVF_OTU)

IVF_meta_df <- as.data.frame(IVF_meta)
IVF_OTU_df <- as.data.frame(t_IVF_OTU)
# Merge metadata and OTU
IVF_merged <- merge(IVF_OTU_df, IVF_meta_df, by = "row.names", all.x=FALSE)

IVF_filtered <- IVF_merged[, !colnames(IVF_merged) %in% c("Row.names", "AGE", "disease", "age_group", "age_outcome")]
IVF_filtered <- IVF_filtered[!is.na(IVF_filtered$pregnancy_success), ]

# Check sample size across pregnancy success
IVF_filtered$pregnancy_success <- as.factor(IVF_filtered$pregnancy_success)
table(IVF_filtered$pregnancy_success)

# Randomforest has an issue where column names cannot start with numbers when using formula (dependent ~ .) 
# https://stackoverflow.com/questions/61639109/error-in-evalpredvars-data-env-object-example-not-found-in-randomforest
pregnancy_col <- which(colnames(IVF_filtered) == "pregnancy_success")
colnames(IVF_filtered)[-pregnancy_col] <- paste0( "X", colnames(IVF_filtered)[-pregnancy_col])

# Lets start with random seed so the outcome will be repeatable and store train and test data.
set.seed(222)
ind <- sample(2, nrow(IVF_filtered), replace = TRUE, prob = c(0.7, 0.3))
train <- IVF_filtered[ind==1,]
test <- IVF_filtered[ind==2,]

# Run the random forest code on the training data
rf <- randomForest(pregnancy_success ~ ., data = train, proximity=TRUE)
print(rf)

## Plotting the model
plot(rf) 
MDSplot(rf, train$pregnancy_success)

#Variable Importance Plot
png("Top5_VariableImportance.png", width = 800, height = 600)
varImpPlot(rf, n.var = 5, main = "Top 5 - Variable Importance") #Top 5 Variable Importance plot
dev.off()

#Model Accuracy
# Prediction and confusion matrix on training data
p1 <- predict(rf, train)
confusionMatrix(p1, train$ pregnancy_success)

# Run prediction and confusion on the test data now
p2 <- predict(rf, test)
confusionMatrix(p2, test$ pregnancy_success)



