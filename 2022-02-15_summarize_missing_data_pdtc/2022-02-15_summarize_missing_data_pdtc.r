library(tidyverse)

pdtc_metadata <- read_csv('../2022-02-09_pdxnet_pdtc_pdmr_portal_ids/2022-02-15_pdtc_metadata_cleaned.csv')

nrow(pdtc_metadata)
head(pdtc_metadata)

# Define function to summarize missing fields

summarize_missing_fields <- function(df) {
    colSums((is.na(df) | (df == 'Unknown') | (df == '?'))) %>% as.data.frame()
}

# How many NAs are there for the most important columns?


summarize_missing_fields(pdtc_metadata)

# Break down by PDTC

pdtc_metadata <- pdtc_metadata %>% group_split(contributor)
names(pdtc_metadata) <- c('BCM', 'HCI', 'MDACC', 'UC Davis', 'WISTAR', 'WUSTL')

pdtc_metadata %>% map(head, 1)

# Break down the missing metadata fields by PDTC

pdtc_metadata %>% map(summarize_missing_fields)

# Load the original metadata

original_pdtc_metadata <- read_csv('../2022-02-09_pdxnet_pdtc_pdmr_portal_ids/2022-02-09_pdxnet_portal_pdtc_files.csv')

nrow(original_pdtc_metadata)
head(original_pdtc_metadata)

# NAs and missing data originally?

summarize_missing_fields(original_pdtc_metadata)

# Break down by PDTC

original_pdtc_metadata <- original_pdtc_metadata %>% group_split(contributor)
names(original_pdtc_metadata) <- c('BCM', 'HCI', 'MDACC', 'UC Davis', 'WISTAR', 'WUSTL')

original_pdtc_metadata %>% map(head, 1)

# Get the number of missing metadata fields from the original metadata

original_pdtc_metadata %>% map(summarize_missing_fields)


