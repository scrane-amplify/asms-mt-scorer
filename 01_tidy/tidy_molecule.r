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
json <- read_json(here::here("00_data", "input", "export-2019-07-08T20_14_21.969Z.json"))
lem <- read_csv(here::here("00_data", "input", "clickstream_23.csv"))

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
    left_absorbed_energy = factor(left_absorbed_energy), 
    left_gas = factor(left_gas), 
    left_transferred_energy = factor(left_transferred_energy), 
    right_absorbed_energy = factor(right_absorbed_energy),
    right_gas = factor(right_gas),
    right_transferred_energy = factor(right_transferred_energy),
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
    gas_left = factor(gas_left),
    gas_right = str_extract(right_gas, gas_values),
    gas_right = factor(gas_right),
    
    gas_correct = case_when(gas_left == "absent" & gas_right == "absent" ~ FALSE,
                            gas_left %in% c("nitrogen_dioxide", "sulfur_dioxide") | 
                              gas_right %in% c("nitrogen_dioxide", "sulfur_dioxide") ~ FALSE,
                            gas_left == "carbon_dioxide" & gas_right == "methane" ~ FALSE,
                            gas_left == "methane" & gas_right == "carbon_dioxide" ~ FALSE,
                            TRUE ~ TRUE),
    gas_correct = factor(gas_correct),
    
    gas_absent = case_when(gas_left == "absent" & gas_right == "absent" ~ TRUE,
                           TRUE ~ FALSE),
    gas_absent = factor(gas_absent),
    
    absorbed_levels_left = str_extract(left_absorbed_energy, concentration_values),
    absorbed_levels_right = str_extract(right_absorbed_energy, concentration_values),
    absorbed_levels_left = factor(absorbed_levels_left,
                                  levels = c("absent", "low", "medium", "high"), ordered = TRUE),
    absorbed_levels_right = factor(absorbed_levels_right,
                                   levels = c("absent", "low", "medium", "high"), ordered = TRUE),
    absorbed_compare_correct = ifelse(absorbed_levels_left < absorbed_levels_right, 1, 0),
    absorbed_compare_correct = ifelse(absorbed_levels_left == "absent" & 
                                        absorbed_levels_right == "absent", 2, absorbed_compare_correct),
    
    molecule = factor(molecule),
    concentration = factor(concentration),
    absorption = factor(absorption),
    transfer = factor(transfer))

# matched$gas_left <- as.factor(matched$gas_left)
# matched$gas_right <- as.factor(matched$gas_right)
# matched$gas_correct <- as.factor(matched$gas_correct)
# matched$gas_absent <- as.factor(matched$gas_absent)
# train_data$molecule<-as.factor(train_data$molecule)

remove(json, labels, concentration_values, gas_values, lem, molecule_raw)
