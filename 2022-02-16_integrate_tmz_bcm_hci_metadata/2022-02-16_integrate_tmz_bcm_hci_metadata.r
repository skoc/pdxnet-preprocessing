library(tidyverse)
library(readxl)

pdtc_models <- read_csv('../2022-02-09_pdxnet_pdtc_pdmr_portal_ids/2022-02-09_pdxnet_portal_pdtc_models.csv')

head(pdtc_models)

pdtc_metadata <- read_csv('../2022-02-09_pdxnet_pdtc_pdmr_portal_ids/2022-02-15_pdtc_metadata_cleaned.csv')

head(pdtc_metadata)

tmz_metadata <- read_excel('TMZ_SequenceMetadata.xlsx') %>% mutate_if(is.character, str_trim) # Need to strip whitespace

head(tmz_metadata)

bcm_metadata <- read_csv('BCM_FullMetaData_Breast_collection_models.csv')

head(bcm_metadata)

# Define function to summarize missing fields

summarize_missing_fields <- function(df) {
    colSums((is.na(df) | (df == 'Unknown') | (df == '?'))) %>% as.data.frame()
}

# How many NAs are there for the most important columns?


summarize_missing_fields(pdtc_metadata)

# How many don't conform to HCI-# and BCM-# with the dash
# Note: a few models don't follow this convention and have a different prefix entirely

# How many models are there from BCM?
nrow(pdtc_metadata %>% filter(contributor == 'BCM'))

# How many start with this?
pdtc_metadata %>% filter(contributor == 'BCM' & startsWith(model_id, 'BCM-')) %>% nrow()

# Show the ones that don't
pdtc_metadata %>% filter(contributor == 'BCM' & !startsWith(model_id, 'BCM-'))

# Do the same for HCI

# How many models are there from HCI?
nrow(pdtc_metadata %>% filter(contributor == 'HCI'))

# How many start with this?
pdtc_metadata %>% filter(contributor == 'HCI' & startsWith(patient_id, 'HCI-')) %>% nrow()

# How many don't
pdtc_metadata %>% filter(contributor == 'HCI' & !startsWith(patient_id, 'HCI-')) %>% nrow()

# For the sequencing metadata ensure that all are HCI-#

pdtc_metadata_cleaned <- pdtc_metadata %>% mutate(patient_id = ifelse((contributor == 'HCI' & startsWith(patient_id, 'HCI')), str_replace(patient_id, 'HCI', 'HCI-'), patient_id))

# Check
pdtc_metadata_cleaned %>% filter(contributor == 'HCI' & startsWith(patient_id, 'HCI-')) %>% nrow()

# Which ones don't?
pdtc_metadata_cleaned %>% filter(contributor == 'HCI' & !startsWith(patient_id, 'HCI-'))

# Bring the patient_id over to the model ID field

pdtc_metadata_cleaned <- pdtc_metadata_cleaned %>% mutate(model_id = ifelse((contributor == 'HCI' & is.na(model_id) & sample_type == 'PDX'), patient_id, model_id))

# Check the numner of missing model IDs here and what the sample_types of those are
pdtc_metadata_cleaned %>% filter(contributor == 'HCI' & is.na(model_id)) %>% nrow()
(pdtc_metadata_cleaned %>% filter(contributor == 'HCI' & is.na(model_id)))$sample_type %>% unique()

# How many PDX samples don't have a model_id now?

pdtc_metadata_cleaned %>% filter(sample_type == 'PDX' & is.na(model_id)) 

# Start with the BCM models

pdtc_metadata_cleaned <- pdtc_metadata_cleaned %>% left_join(select(bcm_metadata, `Model ID`, `Pathology Diagnosis`, `Patient ID`), by = c('model_id' = 'Model ID')) %>%
    mutate(
        disease_type = ifelse(disease_type == 'Unknown' & !is.na(`Pathology Diagnosis`), `Pathology Diagnosis`, disease_type),
        body_location = ifelse(body_location == 'Unknown' & !is.na(`Pathology Diagnosis`), 'Breast', body_location),
        patient_id = ifelse(is.na(patient_id) & !is.na(`Pathology Diagnosis`), `Patient ID`, patient_id)
        ) %>%
    select(-`Pathology Diagnosis`, -`Patient ID`)

nrow(pdtc_metadata_cleaned)
head(pdtc_metadata_cleaned)

# Add using the tmz data sheet
# I don't need to add passages, as these are already present for all of these samples in the pdtc metadata

pdtc_metadata_cleaned <- pdtc_metadata_cleaned %>% left_join(
        distinct(select(tmz_metadata, Sex, `Primary Tumor Origin`, `Model ID`, `Tumor ID`, `Tissue Histology`, `Patient ID`, `Specimen Tissue`, `Is FFPE`)), by = c('model_id' = 'Model ID')) %>%
    mutate(
        gender = ifelse((is.na(gender) | gender == 'Unknown') & !is.na(Sex), Sex, gender),
        patient_id = ifelse(is.na(patient_id) & !is.na(`Patient ID`), `Patient ID`, patient_id),
        tumor_id = ifelse(is.na(tumor_id) & !is.na(`Tumor ID`), `Tumor ID`, tumor_id),
        is_ffpe = ifelse(is_ffpe == 'Unknown' & !is.na(`Is FFPE`), `Is FFPE`, is_ffpe),
        body_location = ifelse(body_location == 'Unknown' & !is.na(`Specimen Tissue`), `Specimen Tissue`, body_location),  # Since some of these were collected other than at their primary tumor origin, this is going to differ from disease_type in those situations
        disease_type = ifelse((disease_type == 'Unknown' | is.na(disease_type)) & !is.na(`Primary Tumor Origin`), str_c(`Primary Tumor Origin`, ' - ', `Tissue Histology`), disease_type)
        ) %>% 
    mutate(disease_type = ifelse(startsWith(disease_type, 'Bladder'), str_replace(disease_type, '^Bladder - ', ''), disease_type)) %>%  # Fix the disease type so it doesn't have bladder in there twice
    mutate(is_ffpe = ifelse(is_ffpe == 'No', FALSE, is_ffpe)) %>%  # Match TRUE/FALSE formatting with existing files
    select(-`Primary Tumor Origin`, -`Tumor ID`, -`Tissue Histology`, -`Patient ID`, -`Specimen Tissue`, -`Is FFPE`, -Sex) # Remove columns from the TMZ metadata

head(pdtc_metadata_cleaned)

# How many NAs are there now?

summarize_missing_fields(pdtc_metadata_cleaned)

# How many PDXs are there without a model_id?

pdtc_metadata_cleaned %>% filter(sample_type == 'PDX' & is.na(model_id)) %>% nrow()

# Export

pdtc_metadata_cleaned %>% write_csv('2022-02-17_pdtc_metadata_cleaned.csv')
