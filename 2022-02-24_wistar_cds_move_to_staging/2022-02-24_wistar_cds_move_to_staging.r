library(tidyverse)
library(readxl)
library(sevenbridges)

# Load the files for cds upload

cds_files <- read_excel('wistar_files_sampleattributes_DS.xlsx')$FILENAME

length(cds_files)
head(cds_files)

# Load the cgc metadata

cgc_files <- read_csv('2022-02-24_wistar_rnaseq_wes_manifest_20220224_095810.csv')

head(cgc_files)

# I don't need all the column, so just get the filename and id from the cgc files

cgc_files <- cgc_files %>% select(id, name)

# basename for the name field

cgc_files <- cgc_files %>% mutate(name = basename(name))

head(cgc_files)
nrow(cgc_files)

all(cds_files %in% cgc_files$name)

# Filter the cgc files for only the cds files

cds_files_manifest <- cgc_files %>% filter(name %in% cds_files)

nrow(cds_files_manifest)
head(cds_files_manifest)

# Connect to the platform

sbg_auth <- Auth(from = 'file', profile_name = 'cgc')
sbg_proj <- sbg_auth$project(id = 'pdxnet/pdxnet-datapool')

# Get the folder object for the staging area

staging_folder <- sbg_proj$get_root_folder()$list_folder_contents(complete = TRUE)[[11]]$list_folder_contents(complete = TRUE)[[1]]

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
    sbg_proj$file(id = cds_files_manifest$id[i])$set_meta(cds_status = 'uploaded', dbgap_accession = 'phs002432.v1.p1')
}
