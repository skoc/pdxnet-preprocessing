library(tidyverse)
library(sevenbridges)

# Load the dbgap metadata for the file list

cds_files <- read_csv('HCI_dbGaP_SampleMapping_SampleAttributes_DS_v1.csv')$FILENAME

length(cds_files)

# Load the CGC metadata to get the file IDS

hci_files <- read_csv('2022-02-23_hci_fastq_manifest_20220223_150616.csv') %>% select(id, name) %>% mutate(name = basename(name))

head(hci_files)

# Filter the hci files on the cgc for the list of files from dbGaP

cds_files_manifest <- hci_files %>% filter(name %in% cds_files)

nrow(cds_files_manifest)
head(cds_files_manifest)

# Connect to the platform

sbg_auth <- Auth(from = 'file', profile_name = 'cgc')
sbg_proj <- sbg_auth$project(id = 'pdxnet/pdxnet-datapool')

# Get the folder object for the staging area

staging_folder <- sbg_proj$get_root_folder()$list_folder_contents(complete = TRUE)[[6]]$list_folder_contents(complete = TRUE)[[1]]

staging_folder
staging_folder$id

# Copy files to the staging area

for (i in seq_along(cds_files_manifest$id)) {
    
    # Pause for 5 mins every 200 iterations to avoid API call limits
    if (i %% 200 == 0) {
        Sys.sleep(300)
    }
    
    # Copy files
    sbg_proj$file(id = cds_files_manifest$id[i])$copy_to_folder(staging_folder$id)
}

for (i in seq_along(cds_files_manifest$id)) {
    
    # Pause for 5 mins every 200 iterations to avoid API call limits
    if (i %% 200 == 0) {
        Sys.sleep(300)
    }
    
    # Set metadata
    sbg_proj$file(id = cds_files_manifest$id[i])$set_meta(cds_status = 'uploaded', dbgap_accession = 'phs002479.v1.p1')
}
