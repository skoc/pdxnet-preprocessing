library(tidyverse)
library(readxl)

pdtc_models <- read_csv('2022-02-09_pdxnet_portal_pdtc_models.csv')
colnames(pdtc_models)[1] <- 'Index'
nrow(pdtc_models)
colnames(pdtc_models)

head(pdtc_models)

pdtc_files <- read_csv('2022-02-09_pdxnet_portal_pdtc_files.csv')
colnames(pdtc_files)[1] <- 'Index'
nrow(pdtc_files)
colnames(pdtc_files)

head(pdtc_files)

pdmr_files <- read_csv('2022-02-09_pdxnet_portal_pdmr_files.csv')
colnames(pdmr_files)[1] <- 'Index'
nrow(pdmr_files)
colnames(pdmr_files)

head(pdmr_files)

pdtc_files_missing_gender <- pdtc_files %>% filter(is.na(gender))
pdtc_files_missing_gender %>% nrow()

pdtc_files_missing_paired_end <- pdtc_files %>% filter(is.na(paired_end))
pdtc_files_missing_paired_end %>% nrow()

pdtc_files_missing_capture_kit <- pdtc_files %>% filter((is.na(capture_kit) | (capture_kit == 'Unknown')))
pdtc_files_missing_capture_kit %>% nrow()

pdtc_files_missing_capture_assembly <- pdtc_files %>% filter(is.na(capture_assembly))
pdtc_files_missing_capture_assembly %>% nrow()

pdtc_files_missing_access_level <- pdtc_files %>% filter(is.na(access_level))
pdtc_files_missing_access_level %>% nrow()

pdtc_files_missing_created_datetime <- pdtc_files %>% filter(is.na(created_datetime))
pdtc_files_missing_created_datetime %>% nrow()

pdtc_files_missing_data_category <- pdtc_files %>% filter(is.na(data_category))
pdtc_files_missing_data_category %>% nrow()

pdtc_files_missing_data_format <- pdtc_files %>% filter(is.na(data_format))
pdtc_files_missing_data_format %>% nrow()

pdtc_files_missing_platform <- pdtc_files %>% filter(is.na(platform))
pdtc_files_missing_platform %>% nrow()

pdtc_files_missing_ffpe <- pdtc_files %>% filter(is.na(is_ffpe))
pdtc_files_missing_ffpe %>% nrow()

pdtc_files_missing_file_size <- pdtc_files %>% filter(is.na(file_size))
pdtc_files_missing_file_size %>% nrow()

pdtc_files_missing_passage <- pdtc_files %>% filter((is.na(passage) | (passage == '?')))
pdtc_files_missing_passage %>% nrow()

pdtc_files_missing_disease_type <- pdtc_files %>% filter((is.na(disease_type) | (disease_type == 'Unknown')))
pdtc_files_missing_disease_type %>% nrow()

pdtc_files_missing_disease_detail <- pdtc_files %>% filter((is.na(disease_detail) | (disease_detail == 'Unknown')))
pdtc_files_missing_disease_detail %>% nrow()

# There are some CRAM files in here for no good reason
# I thought these had been removed?

pdtc_cram_files <- pdtc_files %>% filter(data_format == 'cram')

pdtc_cram_files

# Create a small table to replace these with fastqs by first loading in a small manifest

wustl_cram_converted_fastqs <- read_csv('manifest_20220209_155256_wustl_converted_cram_fastq.csv') %>% mutate(name = basename(name))

wustl_cram_converted_fastqs

pdtc_cram_files_converted_fastq <- pdtc_cram_files[c(1, 1, 2, 2, 3, 3), ]

pdtc_cram_files_converted_fastq

# Replace with the fastq files

pdtc_cram_files_converted_fastq$file_name <- wustl_cram_converted_fastqs$name
pdtc_cram_files_converted_fastq$file_size <- wustl_cram_converted_fastqs$size

pdtc_cram_files_converted_fastq$data_category <- 'Raw Sequencing Data'
pdtc_cram_files_converted_fastq$data_format <- 'fastq'
pdtc_cram_files_converted_fastq$data_type <- 'Raw Reads'

pdtc_cram_files_converted_fastq$paired_end <- c(1, 2, 1, 2, 1, 2)

pdtc_cram_files_converted_fastq

# Export

pdtc_cram_files_converted_fastq %>% write_csv('2022-02-09_wustl_cram_fastq_converted_for_portal.csv')

# Put these converted cram files back in

edited_pdtc_files <- pdtc_files %>% filter((data_format != 'cram') | is.na(data_format))

edited_pdtc_files <- bind_rows(edited_pdtc_files, pdtc_cram_files_converted_fastq) %>% arrange('index')

edited_pdtc_files %>% nrow()
head(edited_pdtc_files)

# Read in the non-concatenated data

non_concat_lanes <- read_excel('Individual_Lane_Files_for_Removal.xlsx')

nrow(non_concat_lanes)
head(non_concat_lanes)

# How many unique sample_ids are there?

non_concat_lanes$sample_id %>% unique()

# Load in the manifest of the concatenated versions

hci_concat_fastqs <- read_csv('manifest_20220209_165026_hci_concatenated_fastqs.csv') %>% mutate(name = basename(name))

nrow(hci_concat_fastqs)
head(hci_concat_fastqs)

# Create a table of the rest of the metadata that exists for the non-concat fastqs

edited_hci_concat_lanes <- non_concat_lanes[c(1, 1, 25, 25, 53, 53, 75, 75, 117, 117, 157, 157, 183, 183, 209, 209, 233, 233, 253, 253, 273, 273, 295, 295, 327, 327, 351, 351, 381, 381,
                                              413, 413, 439, 439, 469, 469, 503, 503, 533, 533, 563, 563, 580, 580, 616, 616, 654, 654), ]

nrow(edited_hci_concat_lanes)
head(edited_hci_concat_lanes)

# Also need the index column

edited_hci_concat_lanes <- edited_hci_concat_lanes %>% left_join(select(pdtc_files, Index, file_name)) %>% select(Index, contributor:disease_detail)

nrow(edited_hci_concat_lanes)
head(edited_hci_concat_lanes)

# Double check that they're all in the same order

all(hci_concat_fastqs$sample_id == edited_hci_concat_lanes$sample_id)

# Merge

edited_hci_concat_lanes$file_name <- hci_concat_fastqs$name
edited_hci_concat_lanes$file_size <- hci_concat_fastqs$size

edited_hci_concat_lanes$data_category <- 'Raw Sequencing Data'
edited_hci_concat_lanes$data_format <- 'fastq.gz'
edited_hci_concat_lanes$data_type <- 'Raw Reads'

nrow(edited_hci_concat_lanes)
head(edited_hci_concat_lanes)

# Export

edited_hci_concat_lanes %>% write_csv('2022-02-09_hci_concat_lanes_for_portal.csv')

# Add them back into the full dataset

# First remove the existing ones

edited_pdtc_files <- edited_pdtc_files %>% filter(!(file_name %in% non_concat_lanes$file_name))
edited_pdtc_files %>% nrow()

# This number of rows, plus the number of ones which needed removal should add up to 2825
# Which was the orginal amount of rows

2142 + 683

# Put these in and rearrange the rows based on the index

edited_pdtc_files <- bind_rows(edited_pdtc_files, edited_hci_concat_lanes) %>% arrange('index')

edited_pdtc_files %>% nrow()
head(edited_pdtc_files)

# That number of rows should be 2142 + 48

2142 + 48

edited_pdtc_files %>% write_csv('2022-02-09_pdxnet_portal_pdtc_files_removed_cram_nonconcat_lanes.csv')

edited_pdtc_files_by_contributor <- edited_pdtc_files %>% group_split(contributor)

edited_pdtc_files_by_contributor %>% map(head, 1)

# Name the dataframes

names(edited_pdtc_files_by_contributor) <- c('BCM', 'HCI', 'MDACC', 'UC Davis', 'WISTAR', 'WUSTL')

edited_pdtc_files_by_contributor %>% map(head, 5)

# Export these as-is so I can take a look at them more easily

for (i in seq_along(edited_pdtc_files_by_contributor)) {
    write_csv(edited_pdtc_files_by_contributor[[i]], str_c('2022-02-10_', names(edited_pdtc_files_by_contributor)[[i]], '_files_pre_edit.csv'))
    }

# The model IDs have extra crap in the IDs that don't match for BCM some model IDs have a space and some stuff in parenthesis
# This can be removed to match based on model ID

pdtc_models_cleaned <- pdtc_models %>% mutate(ContributorPDX.ID_edit = str_replace(ContributorPDX.ID, '\\s.*$', ''))

nrow(pdtc_models_cleaned)
head(pdtc_models_cleaned)
colnames(pdtc_models_cleaned)

edited_pdtc_files_by_contributor[['BCM']] %>% nrow()
edited_pdtc_files_by_contributor[['BCM']] %>% head()
colnames(edited_pdtc_files_by_contributor[['BCM']])

# How many NAs are there for the most important columns?

colSums((is.na(edited_pdtc_files_by_contributor[['BCM']]) | (edited_pdtc_files_by_contributor[['BCM']] == 'Unknown'))) %>% as.data.frame()

# Merge BCM with the models

edited_bcm_files <- edited_pdtc_files_by_contributor[['BCM']] %>%
    left_join(select(pdtc_models_cleaned, ContributorPDX.ID_edit, Gender, CTEP.SDCDescription, Disease.BodyLocation),
              by = c('model_id' = 'ContributorPDX.ID_edit')
              )

edited_bcm_files %>% head()

# Replace the disease detail if the models info exists
# Fill in gender
# Set the capture kit

edited_bcm_files <- edited_bcm_files %>% mutate(
    disease_detail = ifelse(!is.na(CTEP.SDCDescription), CTEP.SDCDescription, disease_detail),
    disease_type = ifelse(!is.na(Disease.BodyLocation), Disease.BodyLocation, disease_type),
    gender = ifelse(!is.na(Gender), Gender, gender),
    capture_kit = ifelse(experimental_strategy == 'WES', 'SureSelect Human All Exon V6', capture_kit),
    platform = 'Illumina',
    ) %>%
    select(-Gender, -CTEP.SDCDescription, -Disease.BodyLocation)

head(edited_bcm_files)

# data format to lowercase for consistency

edited_bcm_files <- edited_bcm_files %>%
    mutate(data_format = tolower(data_format))

# How'd we do fixing these?

colSums((is.na(edited_bcm_files) | (edited_bcm_files == 'Unknown'))) %>% as.data.frame()

# Export BCM only

write_csv(edited_bcm_files, '2022-02-11_BCM_files_edited.csv')

edited_pdtc_files_by_contributor[['HCI']] %>% nrow()
edited_pdtc_files_by_contributor[['HCI']] %>% head()
colnames(edited_pdtc_files_by_contributor[['HCI']])

# How many NAs are there for the most important columns?

colSums((is.na(edited_pdtc_files_by_contributor[['HCI']]) | (edited_pdtc_files_by_contributor[['HCI']] == 'Unknown') | (edited_pdtc_files_by_contributor[['HCI']] == '?'))) %>% as.data.frame()

# Start cleaning HCI

edited_hci_files <- edited_pdtc_files_by_contributor[['HCI']] %>% mutate(
    paired_end = ifelse(str_detect(file_name, 'file1'), 1, ifelse(str_detect(file_name, 'file2'), 2, paired_end)),
    passage = ifelse(sample_type == 'Normal', NA, passage)
    )

# Continue fixing passage

edited_hci_files <- edited_hci_files %>% mutate(
    passage = ifelse(passage == '?', 'Unknown', passage)
)

head(edited_hci_files)

# Add the data_category, data_format, and data_type
# Remove files that are not fastq.gz or txt.gz, because there are a few in the middle here

edited_hci_files <- edited_hci_files %>% mutate(
    data_category = 'Raw Sequencing Data',
    data_format = 'fastq.gz',
    data_type = 'Raw Reads'
    ) %>% filter((endsWith(edited_hci_files$file_name, 'gz')))

nrow(edited_hci_files)
head(edited_hci_files)

# Export HCI only

write_csv(edited_bcm_files, '2022-02-14_HCI_files_edited.csv')

edited_pdtc_files_by_contributor[['MDACC']] %>% nrow()
edited_pdtc_files_by_contributor[['MDACC']] %>% head()
colnames(edited_pdtc_files_by_contributor[['MDACC']])

# How many NAs are there for the most important columns?

colSums((is.na(edited_pdtc_files_by_contributor[['MDACC']]) | (edited_pdtc_files_by_contributor[['MDACC']] == 'Unknown') | (edited_pdtc_files_by_contributor[['MDACC']] == '?'))) %>% as.data.frame()

# How many of patient_id	model_id	tumor_id	case_id are in the models information?

any(c(edited_pdtc_files_by_contributor[['MDACC']]$patient_id, edited_pdtc_files_by_contributor[['MDACC']]$model_id, edited_pdtc_files_by_contributor[['MDACC']]$tumor_id, edited_pdtc_files_by_contributor[['MDACC']]$case_id) %in% pdtc_models_cleaned$ContributorPDX.ID)

# Add some of the missing metadata in

edited_mdacc_files <- edited_pdtc_files_by_contributor[['MDACC']] %>%
    mutate(data_category = 'Raw Sequencing Data',
           data_format = 'fastq.gz',
           data_type = 'Raw Reads'
           )

nrow(edited_mdacc_files)
head(edited_mdacc_files)

# Export MDACC only

write_csv(edited_mdacc_files, '2022-02-14_MDACC_files_edited.csv')

edited_pdtc_files_by_contributor[['UC Davis']] %>% nrow()
edited_pdtc_files_by_contributor[['UC Davis']] %>% head()
colnames(edited_pdtc_files_by_contributor[['UC Davis']])

# Add some of the missing metadata in

edited_ucd_files <- edited_pdtc_files_by_contributor[['UC Davis']] %>%
    mutate(data_category = 'Raw Sequencing Data',
           data_format = ifelse(endsWith(file_name, 'gz'), 'fastq.gz', 'fastq'),
           data_type = 'Raw Reads'
           )

nrow(edited_ucd_files)
head(edited_ucd_files)

# Export UCD only

write_csv(edited_ucd_files, '2022-02-14_UCDavis_files_edited.csv')

edited_pdtc_files_by_contributor[['WISTAR']] %>% nrow()
edited_pdtc_files_by_contributor[['WISTAR']] %>% head()
colnames(edited_pdtc_files_by_contributor[['WISTAR']])

# How many NAs are there for the most important columns?

colSums((is.na(edited_pdtc_files_by_contributor[['WISTAR']]) | (edited_pdtc_files_by_contributor[['WISTAR']] == 'Unknown') | (edited_pdtc_files_by_contributor[['WISTAR']] == '?'))) %>% as.data.frame()

# Load i nthe subject phenotype info from the dbGaP submission to fill over to this

wistar_subject_phenotypes <- read_excel('wistar_subjectphenotype_DS_v2.xlsx')

head(wistar_subject_phenotypes)

# Are all these subject Ids found in the portal metadata?

(edited_pdtc_files_by_contributor[['WISTAR']]$patient_id %>% unique()) %in% wistar_subject_phenotypes$SUBJECT_ID %>% table()

edited_pdtc_files_by_contributor[['WISTAR']] %>% filter(!(patient_id %in% wistar_subject_phenotypes$SUBJECT_ID))

# Start with the easy stuff

edited_wistar_files <- edited_pdtc_files_by_contributor[['WISTAR']] %>%
    mutate(data_category = 'Raw Sequencing Data',
           data_format = ifelse(endsWith(file_name, 'gz'), 'fastq.gz', 'fastq'),
           data_type = 'Raw Reads'
           )

# Fill in the disease info for all the others, and make the rest of the fields more consistent

edited_wistar_files <- edited_wistar_files %>% left_join(select(wistar_subject_phenotypes, SUBJECT_ID, SEX, DISEASE_DESCRIPTION), by = c('patient_id' = 'SUBJECT_ID')) %>%
    mutate(gender = str_replace(str_replace(str_replace(SEX, 'UNK', 'Unknown'), '1', 'Male'), '2', 'Female'),
           disease_detail = ifelse(!is.na(DISEASE_DESCRIPTION), DISEASE_DESCRIPTION, 'Unknown'),
           passage = ifelse(((passage == 'Tumor') | (passage == 'tumor')), NA, passage)) %>%
    mutate(disease_type = ifelse(disease_detail == 'Unknown', 'Unknown', 'Skin')) %>%
    select(-SEX, -DISEASE_DESCRIPTION) 

nrow(edited_wistar_files)
head(edited_wistar_files)

# One more small detail, turn NAs in gender into Unknown

edited_wistar_files <- edited_wistar_files %>% mutate(gender = ifelse(is.na(gender), 'Unknown', gender))

# Export wistar only

write_csv(edited_wistar_files, '2022-02-14_Wistar_files_edited.csv')

edited_pdtc_files_by_contributor[['WUSTL']] %>% nrow()
edited_pdtc_files_by_contributor[['WUSTL']] %>% head()
colnames(edited_pdtc_files_by_contributor[['WUSTL']])

# How many NAs are there for the most important columns?

colSums((is.na(edited_pdtc_files_by_contributor[['WUSTL']]) | (edited_pdtc_files_by_contributor[['WUSTL']] == 'Unknown') | (edited_pdtc_files_by_contributor[['WUSTL']] == '?'))) %>% as.data.frame()

# Read in the PDMR patient info

pdmr_patient_info <- read_csv('pdmr_patientinformation.csv')

nrow(pdmr_patient_info)
head(pdmr_patient_info)

# Reformat the wustl data so it can be merged with this patient info to fill those ones in

edited_wustl_files <- edited_pdtc_files_by_contributor[['WUSTL']] %>% mutate(pdmr_patient = ifelse(startsWith(patient_id, 'PDMR'), str_replace(patient_id, 'PDMR-', ''), NA))

nrow(edited_wustl_files)
head(edited_wustl_files)

# Add the pdmr info to the data frame

edited_wustl_files <- edited_wustl_files %>% left_join(select(pdmr_patient_info, -View, -'STR ProfileAvail', -Gender), by = c('pdmr_patient' = 'Patient ID'))

nrow(edited_wustl_files)
head(edited_wustl_files)

# Fix the disease_type and disease_detail, as well as paired end status

edited_wustl_files <- edited_wustl_files %>% mutate(
    disease_type = ifelse(!is.na(`Disease BodyLocation`), `Disease BodyLocation`, disease_type),
    disease_detail = ifelse(!is.na(`CTEP SDCDescription`), `CTEP SDCDescription`, disease_detail),
    paired_end = ifelse(((paired_end == 'Unknown') | is.na(paired_end)), ifelse(endsWith(file_name, 'r1.fastq.gz'), 1, 2), paired_end)
    ) %>%
    select(Index:disease_detail)

nrow(edited_wustl_files)
head(edited_wustl_files)

# How'd we do fixing these?

colSums((is.na(edited_wustl_files) | (edited_wustl_files == 'Unknown') | (edited_wustl_files == '?'))) %>% as.data.frame()

# Export wustl only

write_csv(edited_wustl_files, '2022-02-15_wustl_files_edited.csv')

# Bind rows into a single dataframe

merged_edited_pdtc_files <- bind_rows(edited_bcm_files, edited_hci_files, edited_mdacc_files, edited_ucd_files, edited_wistar_files, edited_wustl_files)

nrow(merged_edited_pdtc_files)
head(merged_edited_pdtc_files)

# How'd we do fixing these?

colSums((is.na(merged_edited_pdtc_files) | (merged_edited_pdtc_files == 'Unknown') | (merged_edited_pdtc_files == '?'))) %>% as.data.frame()

# Give them all a new index

merged_edited_pdtc_files$Index <- seq(1, 2180)

head(merged_edited_pdtc_files)

# In the passage column it should be "unknown" if sample type is PDX, rather than NA
# Tumors and normals should be NA because that column does not apply

merged_edited_pdtc_files <- merged_edited_pdtc_files %>% mutate(passage = ifelse(((sample_type == 'PDX') & is.na(passage)), 'Unknown', passage))

nrow(merged_edited_pdtc_files)
head(merged_edited_pdtc_files)

# Remove columns that are not useful

merged_edited_pdtc_files <- merged_edited_pdtc_files %>% select(-file_size, -created_datetime, -investigation)

nrow(merged_edited_pdtc_files)
head(merged_edited_pdtc_files)

# Rename the disease_type to and change disease_detail to disease_type for consistency

merged_edited_pdtc_files <- merged_edited_pdtc_files %>% rename('body_location' = 'disease_type') %>% rename('disease_type' = 'disease_detail')

head(merged_edited_pdtc_files)

# Do some final cleanup of the columns to turn things into unknown vs just NA

merged_edited_pdtc_files <- merged_edited_pdtc_files %>% mutate(
    is_ffpe = ifelse(is.na(is_ffpe), 'Unknown', is_ffpe),
    access_level = ifelse(is.na(access_level), 'Controlled', access_level),
    public = ifelse(is.na(public), 'No', public),
    gender = ifelse(is.na(gender), 'Unknown', gender)
    )

head(merged_edited_pdtc_files)

# Load the manifest

hci_manifest_pdx_tumor <- read_csv('hci_manifest_20220215_160645.csv') %>%
    mutate(name = basename(name)) %>%
    select(name, capture_kit) %>%
    rename('manifest_capture_kit' = 'capture_kit')

head(hci_manifest_pdx_tumor)

# Merge with all the other files and fix capture_kit

merged_edited_pdtc_files <- merged_edited_pdtc_files %>% left_join(hci_manifest_pdx_tumor, by = c('file_name' = 'name')) %>%
    mutate(capture_kit = ifelse(((is.na(capture_kit) | (capture_kit == 'Unknown')) & !is.na(manifest_capture_kit)), manifest_capture_kit, capture_kit)) %>%
    mutate(capture_kit = ifelse(is.na(capture_kit), 'Unknown', capture_kit)) %>%
    select(-manifest_capture_kit)

head(merged_edited_pdtc_files)

# Reorder a few of the columns at the end because it makes more sense to put the body location and disease type columns together

merged_edited_pdtc_files <- merged_edited_pdtc_files %>% select(Index:body_location, disease_type, comment)

# Remove the title of the first column for consistency with the existing data

colnames(merged_edited_pdtc_files)[1] <- ''

merged_edited_pdtc_files %>% head()

# Export this as a file

merged_edited_pdtc_files %>% write_csv('2022-02-15_pdtc_metadata_cleaned.csv')
