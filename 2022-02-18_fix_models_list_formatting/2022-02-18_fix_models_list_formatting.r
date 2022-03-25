library(tidyverse)

pdtc_models <- read_csv('2022-02-09_pdxnet_portal_pdtc_models.csv')

head(pdtc_models)

pdtc_models <- pdtc_models %>% mutate(ContributorPDX.ID = ifelse(Contributor == 'HCI', str_replace(ContributorPDX.ID, '^(?!.*HCI-)HCI', 'HCI-'), ContributorPDX.ID))

# Check the HCI ones for formatting
pdtc_models %>% filter(Contributor == 'HCI')

# Export
colnames(pdtc_models)[1] <- ''
write_csv(pdtc_models, '2022-02-18_pdxnet_portal_pdtc_models.csv')


