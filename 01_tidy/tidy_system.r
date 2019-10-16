library(tidyverse)
library(jsonlite)
library(here)
library(lubridate)
source("01_tidy/helper_system.R")
options(digits.secs = 3)

# Import & clean data ------------------------------------------------------------------------------
lem <- read_csv(here::here("00_data", "input", "clickstream_27.csv")) %>% 
  filter(body_result_extensions_panel == "right") %>% 
  select(-contains("left"), # We want to ignore any student work on the left panels
         -body_verb_id,     
         -body_object_id,
         -body_context_extensions_levelname, 
         -body_result_extensions_panel,
         -body_result_extensions_view, # The rest of these dropped variables are for the molecule level
         -body_result_extensions_right_absorbed_energy, 
         -body_result_extensions_right_gas, 
         -body_result_extensions_right_ice, 
         -body_result_extensions_right_temperature, 
         -body_result_extensions_right_transferred_energy) %>% 
  distinct() %>% 
  is_latest_submission() %>% 
  filter(is_latest_submission == TRUE) %>% 
  set_names(~str_replace_all(., "body_result_extensions_", ""))

json <- read_json(here::here("00_data", "input", "export-2019-08-07T18_09_49.724Z.json"))

labels <- 
  json %>% 
  purrr::map_dfr(parse_labels) %>% 
  distinct()

# Join data ----------------------------------------------------------------------------------------
system_raw <- inner_join(labels, lem, by = c("user_business_key" = "body_actor_account_name"))

# Feature Transformations --------------------------------------------------------------------------
system <- 
  system_raw %>% 
  mutate(head_x     = head_x - 385,
         head_y     = 617 - head_y,
         tail_x     = tail_x - 385,
         tail_y     = 617 - tail_y, 
         molecule_x = molecule_x - 385, 
         molecule_y = 617 - molecule_y,
         id         = row_number()) %>% 
  select(user_business_key, 
         submission_id, 
         id, 
         head_x, 
         head_y, 
         tail_x, 
         tail_y, 
         molecule_x, 
         molecule_y, 
         object_id) %>% 
  gather(key = "item_type", 
         value = "value", 
         -id, 
         -submission_id, 
         -user_business_key, 
         -object_id) %>% 
  filter(!is.na(value)) %>% 
  separate(item_type, c("item", "axis"), "_") %>% 
  spread(axis, value) %>% 
  rename(x_axis = x, 
         y_axis = y) 


system_features <- 
  system %>% 
  group_by(submission_id) %>% 
  mutate(pt = row_number()) %>% 
  nest() %>% 
  mutate(location_group = map(data, identify_groups)) %>% 
  unnest(cols = c(data, location_group)) %>% 
  # TODO: path objects get a 0 here for 'correct_molecule' but that should maybe be an NA
  mutate(correct_molecule = ifelse(str_detect(object_id, "carbon_dioxide|methane"), 1, 0)) %>% 
  group_by(submission_id, location_group) %>% 
  mutate(nh = sum(item == "head"), 
         nt = sum(item == "tail"),
         nm = sum(item == "molecule"),
         cm = max(correct_molecule), 
         has_htm = ifelse(nh >= 1 & nt >= 1 & nm >= 1 & cm >= 1, 1, 0), 
         has_ht = ifelse(nh >= 1 & nt >= 1 & nm == 0, 1, 0)) %>% 
  group_by(submission_id) %>% 
  nest() %>% 
  mutate(n_htm = map(data, count_htm)) %>% 
  unnest(cols = c(data, n_htm)) %>% 
  nest() %>% 
  mutate(n_ht = map(data, count_ht)) %>% 
  unnest(cols = c(data, n_ht)) %>% 
  group_by(submission_id) %>% 
  nest() %>% 
  mutate(values = map(data, has_downward_deflection)) %>% 
  unnest(data) %>% 
  unnest_wider(values) %>% 
  mutate(has_htm = max(has_htm),
         has_ht = max(has_ht)) %>% 
  group_by(submission_id) %>% 
  nest() %>% 
  mutate(diff = map(data, count_distance)) %>% 
  unnest(data) %>% 
  unnest_wider(diff) 

remove(json, labels, lem, system_raw)