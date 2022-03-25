library(tidyverse)
library(sevenbridges)

existing_wustl_files <- read_csv('2022-02-22_wustl_datapool_existing_rnaseq_wes_manifest.csv') %>%
    mutate(name = basename(name))

head(existing_wustl_files)

cds_wustl_files <- read_csv('2022-02-22_wustl_datapool_cds_rnaseq_wes_manifest.csv') %>%
    mutate(name = basename(name))

head(cds_wustl_files)

# Files not already in the datapool have not been analyzed
# Create a separate list

new_wustl_files <- cds_wustl_files %>% filter(!(name %in% existing_wustl_files$name))

nrow(new_wustl_files)
head(new_wustl_files)

# Connect to the platform

sbg_auth <- Auth(from = 'file', profile_name = 'cgc')
sbg_proj <- sbg_auth$project(id = 'pdxnet/pdxnet-datapool')

# Get the folder object for the analysis_incomplete folder

analysis_incomplete_folder <- sbg_proj$get_root_folder()$list_folder_contents(complete = TRUE)[[5]]$list_folder_contents(complete = TRUE)[[6]]$list_folder_contents(complete = TRUE)[[1]]

analysis_incomplete_folder
analysis_incomplete_folder$id

# Test folder operations with the first file

# Test setting metadata
sbg_proj$file(id = new_wustl_files$id[1])$set_meta(cds_status = 'uploaded', dbgap_accession = 'phs002432.v1.p1')

# Test moving file
sbg_proj$file(id = new_wustl_files$id[1])$move_to_folder(analysis_incomplete_folder$id)

# Get file info
sbg_proj$file(id = new_wustl_files$id[1])
sbg_proj$file(id = new_wustl_files$id[1])$get_parent_folder()

# Loop to move the files which haven't been analyzed yet and set metadata fields

for (i in seq_along(new_wustl_files$id)) {
    
    # Pause for 5 mins every 200 iterations to avoid API call limits
    if (i %% 200 == 0) {
        Sys.sleep(300)
    }
    
    # Set metadata and move files
    sbg_proj$file(id = new_wustl_files$id[i])$set_meta(cds_status = 'uploaded', dbgap_accession = 'phs002432.v1.p1')
    sbg_proj$file(id = new_wustl_files$id[i])$move_to_folder(analysis_incomplete_folder$id)
}

# Filter the manifest for files which were submitted to CDS

existing_files_cds_uploaded <- existing_wustl_files %>% filter(name %in% cds_wustl_files$name)

nrow(existing_files_cds_uploaded)
head(existing_files_cds_uploaded)

# Add the metadata to the files in-place

for (i in seq_along(existing_files_cds_uploaded$id)) {
    
    # Pause for 5 mins every 200 iterations to avoid API call limits
    if (i %% 200 == 0) {
        Sys.sleep(300)
    }
    
    # Set metadata and move files
    sbg_proj$file(id = existing_files_cds_uploaded$id[i])$set_meta(cds_status = 'uploaded', dbgap_accession = 'phs002432.v1.p1')
}
