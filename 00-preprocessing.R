
library(rhomis)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)

# Preprocessing State
base_path <- "inst/projects/tree-aid-example/rhomis-2/"
raw_data_path <- "raw-data/raw-data.csv"
raw_data <- readr::read_csv(paste0(base_path,raw_data_path))

#identify Repeat Columns

raw_data <- raw_data %>% rename(language_survey="survey_grp/language")



clean_tree_aid_columns <- clean_column_names(colnames(raw_data))

clean_tree_aid_columns[duplicated(tolower(clean_tree_aid_columns))]

colnames(raw_data) <- tolower(clean_tree_aid_columns)

write_csv(raw_data, paste0(base_path,"preprocessed_data.csv"))


