library(tidyverse)
library(jsonlite)
library(here)

# Functions ----------------------------------------------------------------------------------------
parse_labels <- function(json_object) {
  external_id <- json_object[["External ID"]]
  molecule <- json_object[["Label"]]$molecule
  concentration <- json_object[["Label"]]$concentration
  absorption <- json_object[["Label"]]$absorption
  transfer <- json_object[["Label"]]$transfer
  agreement <- json_object[["Agreement"]]
  activity_business_key <- str_split(json_object[["External ID"]], "_", simplify = TRUE)[1]
  user_business_key <- json_object[["External ID"]] %>% 
    str_remove(., ".png") %>% 
    str_split(., "_", simplify = TRUE) %>% 
    .[2]
  bind_cols(external_id = external_id, 
            molecule = molecule, 
            concentration = concentration, 
            absorption = absorption, 
            transfer = transfer, 
            agreement = agreement, 
            activity_business_key = activity_business_key, 
            user_business_key = user_business_key)
}

# Load raw data ------------------------------------------------------------------------------------
json <- read_json("00_data/input/export-2019-07-08T20_14_21.969Z.json")
lem <- read_csv(here::here("00_data/input/clickstream_23.csv"))

# Join data ----------------------------------------------------------------------------------------
labels <- 
  json %>% 
  purrr::map_dfr(parse_labels) %>% 
  distinct()

molecule_raw <- inner_join(labels, lem, by = "user_business_key")
names(molecule_raw) <- gsub(pattern = "body_result_extensions_", replacement = "", x = names(molecule_raw))
names(molecule_raw) <- gsub(pattern = "body_context_extensions_", replacement = "", x = names(molecule_raw))

# Feature Transformations --------------------------------------------------------------------------
concentration_values <- "absent|low|medium|high"
gas_values           <- "absent|carbon_dioxide|methane|nitrogen_dioxide|sulfur_dioxide"

molecule_derived <- 
  molecule_raw %>% 
  mutate_at(13:18, ~if_else(is.na(.), "absent", .)) %>% 
  mutate(
    concentration_left = str_extract(left_gas, concentration_values),
    concentration_left = factor(concentration_left, levels = c("absent", "low", "medium", "high"), 
                                ordered = TRUE),
    concentration_right = str_extract(right_gas, concentration_values),
    concentration_right = factor(concentration_right, levels = c("absent", "low", "medium", "high"), 
                                 ordered = TRUE),
    concentration_correct = ifelse(concentration_left < concentration_right, 1, 0), 
    concentration_correct = ifelse(left_gas == "absent" & 
                                     right_gas == "absent", 2, concentration_correct),
    
    gas_left = str_extract(left_gas, gas_values),
    gas_right = str_extract(right_gas, gas_values),
    
    gas_correct = case_when(gas_left == "absent" & gas_right == "absent" ~ FALSE,
                            gas_left %in% c("nitrogen_dioxide", "sulfur_dioxide") | 
                              gas_right %in% c("nitrogen_dioxide", "sulfur_dioxide") ~ FALSE,
                            gas_left == "carbon_dioxide" & gas_right == "methane" ~ FALSE,
                            gas_left == "methane" & gas_right == "carbon_dioxide" ~ FALSE,
                            TRUE ~ TRUE),
    
    gas_absent = case_when(gas_left == "absent" & gas_right == "absent" ~ TRUE,
                           TRUE ~ FALSE),
    
    absorbed_levels_left = str_extract(left_absorbed_energy, concentration_values),
    absorbed_levels_right = str_extract(right_absorbed_energy, concentration_values),
    absorbed_levels_left = factor(absorbed_levels_left,
                                  levels = c("absent", "low", "medium", "high"), ordered = TRUE),
    absorbed_levels_right = factor(absorbed_levels_right,
                                   levels = c("absent", "low", "medium", "high"), ordered = TRUE),
    absorbed_compare_correct = ifelse(absorbed_levels_left < absorbed_levels_right, 1, 0),
    absorbed_compare_correct = ifelse(absorbed_levels_left == "absent" & 
                                        absorbed_levels_right == "absent", 2, absorbed_compare_correct))


remove(json, labels)
