# Title: Setup DB
# Author: Natalie Goulett, Brandon Edward Rose
# Date edited: 2025/01/17


# Objective -----
# Set up and update the database object using REDCapSync (was RosyREDCap) & API

library("RosyREDCap")
# would these now be under
library("REDCapSync") # ?
library("RosyDB")

projects <- RosyDB::get_projects()

#DB <- load_RosyREDCap("PRAC")
DB <-setup_RosyREDCap(
  short_name = "PRAC",
  dir_path = getwd() %>% normalizePath(),
  # for security, the API token is defined in the .Renviron file
  token_name = "PRAC_token",
  redcap_base = "https://redcap.fiu.edu/",
  merge_form_name = "patient"
)
DB <- update_RosyREDCap(DB)
DB$metadata %>% RosyUtils::add_list_to_global()
#DB <- add_forms_transformation(DB,forms_transformation =  default_forms_transformation(DB))
DB <- clean_DB(DB)
DB$data %>% RosyUtils::add_list_to_global()

