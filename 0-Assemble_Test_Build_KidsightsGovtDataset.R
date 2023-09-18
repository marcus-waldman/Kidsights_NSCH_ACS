rm(list = ls())

require(haven)
require(tidyverse)
require(readr)
require(usethis)
require(tibble)
require(dataMeta)

OneDrive_Rpkgs_root = "C:/Users/marcu/OneDrive - University of Nebraska Medical Center"
OneDrive_Rpkgs_path = paste0(OneDrive_Rpkgs_root, "/R Packages")

Github_Repo_root = "C:/Users/marcu/Dropbox/GitKraken/white-rhino/"
Github_Repo_path = paste0(Github_Repo_root, "/repositories/Kidsights_NSCH_ACS/")


#### (If necessary) Step 1: Update sysdata.rda (Data that will automatically be loaded when KidsightsGovtDatasets loaded) ####

# Source datasets that need to automatically be loaded into KidsightsGovtDatasets
cahmi_2019_2020 = haven::read_dta(paste0(OneDrive_Rpkgs_path,"/KidsightsGovtDatasets/data-files/source-data/NSCH/2019-2020 NSCH_Topical_CAHMI_DRC.dta"))
cahmi_2017_2018 = haven::read_dta(paste0(OneDrive_Rpkgs_path,"/KidsightsGovtDatasets/data-files/source-data/NSCH/2017-2018 NSCH_topical_DRC_Dec 2019.dta"))
cahmi_2016 = haven::read_dta(paste0(OneDrive_Rpkgs_path,"/KidsightsGovtDatasets/data-files/source-data/NSCH/2016NSCHTopical_DRCv3__Stata13_Sep2018.dta"))

# Constructed datasets that need to be automatically loaded into KidsightsGovtDatasets
vars_df = readxl::read_excel(path=paste0(OneDrive_Rpkgs_path,"/KidsightsGovtDatasets/data-files/constructed-data/gameplan-combined-acs-nsch.xlsx"),
                             sheet = "vars")


usethis::use_data(cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars_df,
                  internal = TRUE, overwrite = TRUE, compress = "gzip")

#### Step 2: Check Package ####
#   Check Package:             'Ctrl + Shift + E'


#### Step 3: Install Package ####
#   Install Package:           'Ctrl + Shift + B'

#### Step 4: Check Package ###
library(KidsightsGovtDatasets)
nsch_dat = KidsightsGovtDatasets::recode_cahmi() %>% tibble::as_tibble()

variable_labels = sapply(1:ncol(nsch_dat),
                         FUN = function(j){
                           sjlabelled::get_label(nsch_dat[,j])
                           }
                         )
variable_labels

dict = dataMeta::build_dict(nsch_dat, linker = build_linker(nsch_dat))
