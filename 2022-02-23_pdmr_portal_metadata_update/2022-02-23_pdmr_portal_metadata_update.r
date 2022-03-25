library(tidyverse)

pdmr_metadata <- read_csv('2021-04-01_pdxnet-pdmr_datapool-fastq-manifest.csv') %>% bind_rows(read_csv('2021-08-16_pdxnet_pdmr_datapool_uploaded_fastq-manifest.csv'))

nrow(pdmr_metadata)
head(pdmr_metadata)

# Remove unnecessary columns

pdmr_metadata <- pdmr_metadata %>% select(-id, -project)

# Create index and contributor columns

pdmr_metadata$index <- seq(1, nrow(pdmr_metadata))
pdmr_metadata$contributor <- 'NCI'

head(pdmr_metadata)

colnames(pdmr_metadata)

# Reorder columns

pdmr_metadata <- pdmr_metadata %>% select(index, contributor, name, `Patient ID`, `PDMR Sample ID`, `PDMR Version`, `Is PDMR Version 2`, sample_id, case_id, `Specimen ID`, sample_type, `Has Matched Normal`,
                         experimental_strategy, gender, age_at_diagnosis, paired_end, `Capture kit`, `Capture assembly`, platform, Passage, disease_type)

colnames(pdmr_metadata)
colnames(pdmr_metadata) %>% length()

colnames(pdmr_metadata) <- c('', 'contributor', 'file_name', 'patient_id', 'pdmr_sample_id', 'pdmr_version', 'is_pdmr_version_2', 'sample_id', 'case_id', 'specimen_id', 'sample_type',
                             'has_matched_normal', 'experimental_strategy', 'gender', 'age_at_diagnosis', 'paired_end', 'capture_kit', 'capture_assembly', 'platform', 'passage',
                             'disease_type')

colnames(pdmr_metadata)
colnames(pdmr_metadata) %>% length()

# Export

write_csv(pdmr_metadata, '2022-02-23_pdmr_portal_metadata.csv')
