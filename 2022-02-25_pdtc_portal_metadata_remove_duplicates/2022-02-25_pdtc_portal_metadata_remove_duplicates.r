library(tidyverse)
library(readxl)

pdtc_metadata <- read_csv('../2022-02-16_integrate_tmz_bcm_hci_metadata/2022-02-17_pdtc_metadata_cleaned.csv')

head(pdtc_metadata)

duplicates <- (pdtc_metadata$file_name %>% table() %>% as.data.frame() %>% filter(Freq > 1))[, 1] %>% as.character()

length(duplicates)
duplicates

# Look at the metadata for only the duplicates externally

duplicates <- filter(pdtc_metadata, file_name %in% duplicates) %>% arrange(file_name)

write_csv(duplicates, '2022-02-25_pdtc_duplicate_files.csv')

# Verify that all HCI duplicates have a 1 in paired_end and NA in capture_assembly

# How many HCI duplicates?
duplicates %>% filter(contributor == 'HCI') %>% nrow()

# Half of those should have NA in the capture assembly
duplicates %>% filter(contributor == 'HCI' & is.na(capture_assembly)) %>% nrow()
(duplicates %>% filter(contributor == 'HCI') %>% nrow()) * 0.5 == (duplicates %>% filter(contributor == 'HCI' & is.na(capture_assembly)) %>% nrow())

# Create a list of HCI files to remove

hci_to_remove <- duplicates %>% filter(contributor == 'HCI' & is.na(capture_assembly))

# Remove HCI duplicates by their unique index

pdtc_metadata_cleaned <- pdtc_metadata %>% filter(!(...1 %in% hci_to_remove$...1))
nrow(pdtc_metadata_cleaned)

# Check the number of remaining files makes sense
nrow(pdtc_metadata_cleaned) == (nrow(pdtc_metadata) - 46)

# Load the tmz metadata

tmz_metadata <- read_excel('../2022-02-16_integrate_tmz_bcm_hci_metadata/TMZ_SequenceMetadata.xlsx')

tmz_metadata

# Create a list of wistar files that are duplicates

wistar_duplicates <- duplicates %>% filter(contributor == 'WISTAR')

wistar_duplicates %>% write_csv('2022-02-25_wistar_duplicates.csv')

wistar_duplicates

# Create new index column

pdtc_metadata_cleaned$...1 <- seq(1, nrow(pdtc_metadata_cleaned))

# Export

pdtc_metadata_cleaned %>% write_csv('2022-02-25_pdtc_metadata_duplicated_wistar.csv')

# read in the edited metadata

wistar_deduplicated <- read_excel('2022-02-25_wistar_deduplicated.xlsx')

wistar_deduplicated

# Remove the rows that are for these duplicated file names from the cleaned version

pdtc_metadata_cleaned <- pdtc_metadata_cleaned %>% filter(!(file_name %in% wistar_deduplicated$file_name))

# Does that match the expected number of files
2146 - nrow(pdtc_metadata_cleaned) == 24

# Put the wistar rows back in, sort, and generate a new index

pdtc_metadata_cleaned <- bind_rows(pdtc_metadata_cleaned, mutate(wistar_deduplicated, is_ffpe = as.character(is_ffpe))) %>%
    arrange(contributor, file_name)

pdtc_metadata_cleaned$...1 <- seq(1, nrow(pdtc_metadata_cleaned))

nrow(pdtc_metadata_cleaned)
head(pdtc_metadata_cleaned)

# Check that all filenames are present exactly once

pdtc_metadata_cleaned$file_name %>% table() %>% as.data.frame() %>% filter(Freq > 1)

# Make sure all the indices are unique

pdtc_metadata_cleaned$...1 %>% table() %>% as.data.frame() %>% filter(Freq > 1)

# Rename the index column before export

colnames(pdtc_metadata_cleaned)[[1]] <- ''

# Export

pdtc_metadata_cleaned %>% write_csv('2022-02-25_pdtc_portal_metadata_deduplicated.csv')
