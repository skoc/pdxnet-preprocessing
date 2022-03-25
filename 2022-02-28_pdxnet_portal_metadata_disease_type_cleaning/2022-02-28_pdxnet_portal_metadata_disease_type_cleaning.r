library(tidyverse)

pdtc_metadata <- read_csv('../2022-02-25_pdtc_portal_metadata_remove_duplicates/2022-02-25_pdtc_portal_metadata_deduplicated.csv') %>%
    rename('index' = '...1')

head(pdtc_metadata)

# What are the unique entries in that column, and how many are there?

disease_types <- pdtc_metadata$disease_type %>% table() %>% as_tibble() %>% rename('disease_type' = '.', 'freq' = 'n')

disease_types

# Replace some of the disease_type replacements that we can do quickly here
# In general, reformat all to have tissue/location - disease - other info

pdtc_metadata_cleaned <- pdtc_metadata %>%
    # colon
    mutate(disease_type = str_replace(disease_type, '(Adenocarcinoma - colon)|(Colon adenocarcinoma)', 'Colon - Adenocarcinoma')) %>%
    mutate(disease_type = str_replace(disease_type, '(Gastrointestinal Neoplasm, Colon - Colon Adenocarcinoma \\(10009951\\))|(Gastrointestinal Neoplasm, Colon - Adenocarcinoma \\(10009951\\))', 'Colon - Gastrointestinal Neoplasm - Adenocarcinoma (10009951)')) %>%
    mutate(disease_type = str_replace(disease_type, 'Gastrointestinal Neoplasm, Colorectal cancer, NOS \\(10010029\\)', 'Colon - Gastrointestinal Neoplasm - Colorectal cancer - NOS (10010029)')) %>%
    # bladder
    mutate(disease_type = str_replace(disease_type, '(Urothelial Bladder Cancer)|(urothelial\\/bladder ca)|(Urothelial/bladder cancer, NOS)', 'Bladder - Urothelial Cancer - NOS')) %>%
    mutate(disease_type = str_replace(disease_type, '(bladder urothelial carcinoma)|(Urothelial carcinoma)', 'Bladder - Urothelial Carcinoma')) %>%
    mutate(disease_type = str_replace(disease_type, '(Urothelial Tract Neoplasm, Urothelial/bladder cancer, NOS \\(10018192\\))|(Urothelial Tract Neoplasm, Bladder - Urothelial Cancer - NOS \\(10018192\\))', 'Bladder - Urothelial Tract Neoplasm - Urothelial Cancer - NOS (10018192)')) %>%
    mutate(disease_type = str_replace(disease_type, 'Myoinvasive bladder cancer', 'Bladder - Myoinvasive Cancer')) %>%
    mutate(disease_type = str_replace(disease_type, 'Transitional cell carcinoma', 'Bladder - Transitional Cell Carcinoma')) %>%
    mutate(disease_type = str_replace(disease_type, 'Urothelial Tract Neoplasm, Transitional cell car. - uroth. \\(10044409\\)', 'Bladder - Urothelial Tract Neoplasm - Transitional Cell Carcinoma (10044409)')) %>%
    # bone
    mutate(disease_type = str_replace(disease_type, 'Bone Neoplasm, Chondrosarcoma \\(10008737\\)', 'Bone - Neoplasm - Chondrosarcoma (10008737)')) %>%
    # breast
    mutate(disease_type = str_replace(disease_type, 'Breast Ascites', 'Breast - Ascites')) %>%
    mutate(disease_type = str_replace(disease_type, '^Invasive breast carcinoma$', 'Breast - Invasive Carcinoma')) %>%
    mutate(disease_type = str_replace(disease_type, '^Invasive ductal carcinoma$', 'Breast - Invasive Ductal Carcinoma')) %>%
    mutate(disease_type = str_replace(disease_type, '^Breast cancer, NOS$', 'Breast - Breast Cancer - NOS')) %>%
    mutate(disease_type = str_replace(disease_type, '^Breast Invasive', 'Breast - Invasive')) %>%
    mutate(disease_type = str_replace(disease_type, 'Breast Neoplasm, Invasive breast carcinoma \\(10006190\\)', 'Breast - Neoplasm - Invasive breast carcinoma (10006190)')) %>%
    mutate(disease_type = str_replace(disease_type, 'Breast Pleural Effusion', 'Breast - Pleural Effusion')) %>%
    mutate(disease_type = str_replace(disease_type, 'Breast Poorly Differentiated', 'Breast - Poorly Differentiated')) %>%
    mutate(disease_type = str_replace(disease_type, 'Infiltrating duct carcinoma, NOS', 'Breast - Infiltrating duct carcinoma - NOS')) %>%
    # pancreas
    mutate(disease_type = str_replace(disease_type, 'Gastrointestinal Neoplasm, Adenocarcinoma - pancreas \\(10052747\\)', 'Pancreas - Gastrointestinal Neoplasm - Adenocarcinoma (10052747)')) %>%
    # rectum
    mutate(disease_type = str_replace(disease_type, 'Gastrointestinal Neoplasm, Adenocarcinoma - rectum \\(10038045\\)', 'Rectum - Gastrointestinal Neoplasm - Adenocarcinoma (10038045)')) %>%
    # gall bladder
    mutate(disease_type = str_replace(disease_type, 'Gastrointestinal Neoplasm, Gall bladder carcinoma \\(adeno\\) \\(10017618\\)', 'Gall Bladder - Gastrointestinal Neoplasm - Adenocarcinoma (10017618)')) %>%
    # gastrointestinal
    mutate(disease_type = str_replace(disease_type, 'Gastrointestinal Neoplasm, Gastrointestinal cancer, NOS \\(10017986\\)', 'Gastrointestinal - Neoplasm - Gastrointestinal cancer - NOS (10017986)')) %>%
    # head and neck
    mutate(disease_type = str_replace(disease_type, 'Head and Neck Neoplasm, H & N squamous cell car., NOS \\(10060121\\)', 'Head and Neck - Neoplasm - Suamous Cell Carcinoma - NOS (10060121)')) %>%
    mutate(disease_type = str_replace(disease_type, 'Head and Neck Neoplasm, Pharyngeal squam. cell carcinoma \\(10034819\\)', 'Head and Neck - Neoplasm - Pharyngeal Squamous Cell Carcinoma (10034819)')) %>%
    # kidney
    mutate(disease_type = str_replace(disease_type, 'Kidney Neoplasm, RCC, clear cell adenocarcinoma \\(10009251\\)', 'Kidney - Neoplasm RCC - Clear Cell Adenocarcinoma (10009251)')) %>%
    mutate(disease_type = str_replace(disease_type, 'Kidney Neoplasm, Renal cell carcinoma, NOS \\(10038415\\)', 'Kidney - Neoplasm - Renal Cell Carcinoma - NOS (10038415)')) %>%
    # lung
    mutate(disease_type = str_replace(disease_type, '(^Lung )|(^Lung, )', 'Lung - ')) %>%
    mutate(disease_type = str_replace(disease_type, ', Lung', ' -')) %>%
    mutate(disease_type = str_replace(disease_type, 'adenocarcinoma \\(10025032\\)', 'Adenocarcinoma (10025032)')) %>%
    mutate(disease_type = str_replace(disease_type, 'cancer, NOS \\(10025065\\)', 'NOS (10025065')) %>%
    mutate(disease_type = str_replace(disease_type, 'cell lung cancer', 'Cell Cancer')) %>%
    mutate(disease_type = str_replace(disease_type, 'Squamous cell lung carcinoma', 'Squamous Cell Carcinoma')) %>%
    mutate(disease_type = str_replace(disease_type, '^Neuroendocrinal$', 'Lung - Neuroendocrinal')) %>%  # Consistent with MDACC models of the same type
    # skin
    mutate(disease_type = str_replace(disease_type, 'Melanoma', 'Skin - Melanoma')) %>%
    mutate(disease_type = str_replace(disease_type, 'Skin Neoplasm, Skin - Melanoma \\(10053571\\)', 'Skin - Neoplasm - Melanoma (10053571)')) %>%
    mutate(disease_type = str_replace(disease_type, 'Skin Neoplasm, Merkel cell tumor \\(10029266\\)', 'Skin - Neoplasm - Merkel Cell Tumor (10029266)')) %>%
    # ovary
    mutate(disease_type = str_replace(disease_type, 'Reproductive System Neoplasm, Female, Ovarian cancer, NOS \\(10033272\\)', 'Ovary - Reproductive System Neoplasm - Female - Ovarian Cancer - NOS (10033272)'))
    
    
    

pdtc_metadata_cleaned$disease_type %>% table() %>% as_tibble() %>% rename('disease_type' = '.', 'freq' = 'n')

# Fix a few more that aren't as standardized

pdtc_metadata_cleaned <- pdtc_metadata_cleaned %>% mutate(disease_type = str_replace(disease_type, 'Soft Tissue Neoplasm, Malig. periph. nerve sheath tum. \\(10026667\\)', 'Soft Tissue - Neoplasm - Malignant Peripheral Nerve Sheath Tumor (10026667)')) %>%
    mutate(disease_type = str_replace(disease_type, 'Soft Tissue Neoplasm, Non-Rhabdo. soft tissue sarcoma \\(10039494\\)', 'Soft Tissue - Neoplasm - Non-Rhabdomyosarcoma Soft Tissue Sarcoma (10039494)')) %>%
    mutate(disease_type = str_replace(disease_type, 'Soft Tissue Neoplasm, Rhabdomyosarcoma, NOS \\(10039024\\)', 'Soft Tissue - Neoplasm - Rhabdomyosarcoma - NOS (10039024)')) %>%
    mutate(disease_type = str_replace(disease_type, 'Soft Tissue Neoplasm, Synovial sarcoma \\(10042866\\)', 'Soft Tissue - Neoplasm - Synovial Sarcoma (10042866)'))


pdtc_metadata_cleaned$disease_type %>% table() %>% as_tibble() %>% rename('disease_type' = '.', 'freq' = 'n')

# Fix body location for those two MDACC samples

pdtc_metadata_cleaned <- pdtc_metadata_cleaned %>% mutate(body_location = ifelse(disease_type == 'Lung - Neuroendocrinal', 'Lung', body_location))

# Normals and blood normals should have disease type == NA

pdtc_metadata_cleaned <- pdtc_metadata_cleaned %>% mutate(disease_type = ifelse(sample_type == 'Normal', NA, disease_type))

pdtc_metadata_cleaned <- pdtc_metadata_cleaned %>% rename('disease_detail' = 'disease_type') %>%
    separate(disease_detail, into = c('disease_type'), sep = ' - ', remove = FALSE, extra = 'drop') %>%
    mutate(disease_type = str_c(disease_type, ' Cancer'))

head(pdtc_metadata_cleaned)

# Some disease names are wrong, because English

pdtc_metadata_cleaned$disease_type %>% table()

# Clean these for grammatical and readability reasons

pdtc_metadata_cleaned <- pdtc_metadata_cleaned %>% mutate(disease_type = str_replace(disease_type, 'Pancreas Cancer', 'Pancreatic Cancer')) %>%
    mutate(disease_type = str_replace(disease_type, 'Rectum Cancer', 'Rectal Cancer')) %>%
    mutate(disease_type = str_replace(disease_type, 'Unknown Cancer', 'Unknown')) %>%
    mutate(disease_type = str_replace(disease_type, 'Ovary Cancer', 'Ovarian Cancer'))

pdtc_metadata_cleaned$disease_type %>% table()

# Fix index name

colnames(pdtc_metadata_cleaned)[1] <- ''

# Export

write_csv(pdtc_metadata_cleaned, '2022-02-28_pdtc_files_metadata_cleaned.csv')

# Let's just check really quick that there are no duplicate files

pdtc_metadata_cleaned$file_name %>% table() %>% as_tibble() %>% filter(n > 1)
