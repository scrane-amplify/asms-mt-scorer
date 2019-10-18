library(tidyverse)
library(jsonlite)
library(here)

# Functions ----------------------------------------------------------------------------------------
parse_labels <- function(json_object) {
  external_id <- json_object[["External ID"]]
  transfer <- json_object[["Label"]]["transfer"]
  temperature <- json_object[["Label"]]["temperature"]
  absorption <- json_object[["Label"]]["absorption"]
  ice <- json_object[["Label"]]["ice"]
  `human_activity_+_molecule` <- json_object[["Label"]]["human_activity_+_molecule"]
  molecule_concentration <- json_object[["Label"]]["molecule_concentration"]
  human_activity_concentration <- json_object[["Label"]]["human_activity_concentration"]
  agreement <- json_object[["Agreement"]]
  activity_business_key <- str_split(json_object[["External ID"]], "_", simplify = TRUE)[1]
  user_business_key <- json_object[["External ID"]] %>% 
    str_remove(., ".png") %>% 
    str_split(., "_", simplify = TRUE) %>% 
    .[2]
  bind_cols(external_id = external_id, 
            transfer = transfer,      
            temperature = temperature, 
            absorption = absorption, 
            ice = ice, 
            `human_activity_+_molecule` = `human_activity_+_molecule`, 
            molecule_concentration = molecule_concentration, 
            human_activity_concentration = human_activity_concentration, 
            agreement = agreement, 
            activity_business_key = activity_business_key, 
            user_business_key = user_business_key)
}


# Load raw data ------------------------------------------------------------------------------------
json <- read_json(here::here("00_data", "input", "export-2019-10-16T23_08_49.218Z.json"))
lem <- read_csv(here::here("00_data", "input", "clickstream_31.csv"))  %>% 
  set_names(~ str_to_lower(.) %>%
              str_replace_all("body result extensions ", "") %>% 
              str_replace_all(" ", "_") %>%
              str_replace_all("bad", "good")) %>% 
  group_by(body_actor_account_name) %>% 
  mutate(max_time = ifelse(request_time_time == max(request_time_time), 1, 0)) %>% 
  filter(max_time == 1) %>% 
  select(-max_time) %>% 
  ungroup() %>% 
  rename(user_business_key = body_actor_account_name) %>% 
  select(user_business_key, 
         submission_id, 
         left_transferred_energy, 
         right_transferred_energy, 
         left_gas, 
         right_gas, 
         left_human_factor, 
         right_human_factor,
         left_temperature, 
         right_temperature, 
         left_absorbed_energy, 
         right_absorbed_energy, 
         left_ice, 
         right_ice) 


# Feature Transformations --------------------------------------------------------------------------
human_impacts_raw <- 
  lem %>% 
  mutate(left_gas_level = str_extract(left_gas, "high|medium|low"), 
         left_gas = str_remove(left_gas, "_high|_medium|_low"), 
         left_transferred_energy = str_remove(left_transferred_energy, "_energy"),
         left_temperature = str_remove(left_temperature, "temperature_"),
         left_absorbed_energy = str_remove(left_absorbed_energy, "absorbed_energy_"),
         left_ice = str_remove(left_ice, "ice_"),
         left_human_factor_level = str_extract(left_human_factor, "high|medium|low"), 
         left_human_factor = str_remove(left_human_factor, "_high|_medium|_low"), 
         right_gas_level = str_extract(right_gas, "high|medium|low"), 
         right_gas = str_remove(right_gas, "_high|_medium|_low"), 
         right_transferred_energy = str_remove(right_transferred_energy, "_energy"), 
         right_temperature = str_remove(right_temperature, "temperature_"), 
         right_absorbed_energy = str_remove(right_absorbed_energy, "absorbed_energy_"), 
         right_ice = str_remove(right_ice, "ice_"), 
         right_human_factor_level = str_extract(right_human_factor, "high|medium|low"), 
         right_human_factor = str_remove(right_human_factor, "_high|_medium|_low")) %>% 
  mutate_all(~case_when(. == "low" ~ "1", 
                        . == "medium" ~ "2", 
                        . == "high" ~ "3", 
                        is.na(.) ~ "0",
                        TRUE ~ .)) %>% 
  mutate(transfer_correct = ifelse((left_transferred_energy == "more_out" | 
                                      left_transferred_energy == "equal_in") & 
                                     right_transferred_energy == "more_in", 
                                   "accept", "reject"), 
         left_gas = ifelse(left_gas == "0" & right_gas != "0", right_gas, left_gas), 
         right_gas = ifelse(right_gas == "0" & left_gas != "0", left_gas, right_gas), 
         molecule_concentration_correct = case_when(
           (left_gas == right_gas) & (left_gas_level < right_gas_level) ~ "accept", 
           TRUE ~ "reject"), 
         left_human_factor = ifelse(left_human_factor == "0" & 
                                      right_human_factor != "0", 
                                    right_human_factor, left_human_factor), 
         right_human_factor = ifelse(right_human_factor == "0" & 
                                       left_human_factor != "0", 
                                     left_human_factor, right_human_factor), 
         human_activity_concentration_correct = case_when(
           (left_human_factor == right_human_factor) & 
             (right_human_factor == "combustion" | right_human_factor == "livestock") & 
             (left_human_factor_level < right_human_factor_level)  ~ "accept", 
           (left_human_factor == right_human_factor) & 
             (left_human_factor == "forest_cover" | left_human_factor == "gas_capture") &
             (left_human_factor_level > right_human_factor_level) ~ "accept", 
           TRUE ~ "reject"),
         human_activity_molecule_correct = case_when(left_human_factor == "combustion" & 
                                            left_gas == "carbon_dioxide" & 
                                            right_human_factor == "combustion" & 
                                            right_gas == "carbon_dioxide" ~ "accept", 
                                          left_human_factor == "forest_cover" & 
                                            left_gas == "carbon_dioxide" & 
                                            right_human_factor == "forest_cover" & 
                                            right_gas == "carbon_dioxide" ~ "accept", 
                                          left_human_factor == "livestock" & 
                                            left_gas == "methane" & 
                                            right_human_factor == "livestock" & 
                                            right_gas == "methane" ~ "accept", 
                                          left_human_factor == "gas_capture" & 
                                            (left_gas == "carbon_dioxide" | 
                                               left_gas == "methane") & 
                                            right_human_factor == left_human_factor & 
                                            right_gas == left_gas ~ "accept",
                                          TRUE ~ "reject"), 
         left_temperature = ifelse(left_temperature == "0", NA, left_temperature), 
         right_temperature = ifelse(right_temperature == "0", NA, right_temperature), 
         temperature_correct = ifelse(left_temperature < right_temperature, "accept", "reject"), 
         temperature_correct = ifelse(is.na(temperature_correct), "reject", temperature_correct), 
         left_temperature = ifelse(is.na(left_temperature), "0", left_temperature), 
         right_temperature = ifelse(is.na(right_temperature), "0", right_temperature), 
         absorption_correct = 
           ifelse(left_absorbed_energy < right_absorbed_energy, "accept", "reject"), 
         ice_correct = ifelse((left_ice > right_ice), "accept", "reject"), 
         ice_correct = ifelse(left_ice == "0" & right_ice  == "0", "accept", ice_correct))


# Join data ----------------------------------------------------------------------------------------
labels <- 
  json %>% 
  purrr::map_dfr(parse_labels) %>% 
  distinct() %>% 
  filter(!is.na(transfer)) %>% 
  rename(human_activity_molecule = `human_activity_+_molecule`)

human_impacts_derived <- inner_join(labels, human_impacts_raw, by = "user_business_key") %>% 
  mutate_if(is.character, str_replace, "ignore", "reject") %>% 
  mutate_if(is.character, factor) 

remove(json, labels, lem, human_impacts_raw)
