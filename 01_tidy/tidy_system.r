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


system_groups <- 
  system %>% 
  group_by(submission_id) %>% 
  mutate(pt = row_number()) %>% 
  nest() %>% 
  mutate(location_group = map(data, identify_groups2)) %>% 
  unnest(cols = c(data, location_group)) %>% 
  ungroup() %>% 
  select(user_business_key, submission_id, pt, location_group, everything()) %>% 
  arrange(user_business_key, submission_id, pt)
  


get_edist <- function(i) {
  #i <- system$submission_id[1]
  # get euclidean distances    
  # t_subset <- filter(system, submission_id == i) %>% 
  #   rowid_to_column("pt") 
  # dist_eucl <- dist(t_subset[ , 7:8], method = "euclidean")
  # # t_distances is a matrix (as a data frame) of the distances between objects within a single submission
  # t_distances <- as.data.frame(round(as.matrix(dist_eucl)[1:nrow(t_subset), 1:nrow(t_subset)], 1)) 
  # 
  # # find sets 
  # t_groups <- 
  #   t_distances %>% 
  #   rowid_to_column("pt") %>% 
  #   gather(k, v, -pt) %>% 
  #   arrange(pt, k) %>%
  #   mutate(pt = as.numeric(pt), 
  #          k = as.numeric(k), 
  #          flag = ifelse(v < 50, 1, 0)) %>% 
  #   filter(flag == 1) %>% 
  #   select(-flag, -v) %>% 
  #   group_by(pt) %>% 
  #   summarise(location_group = paste(k, collapse = ", ")) %>% 
  #   # nest(k, .key = "location_group") %>% 
  #   # nest(location_group = c(k)) %>% 
  #   # mutate(location_group = as.character(location_group)) %>% 
  #   ungroup() 
  # 
  # # This puts the location_group variable back into the original submission data frame
  # df3 <- left_join(t_subset, t_groups, by = "pt")
  
  df4 <- 
    df3 %>% 
    select(pt, user_business_key, submission_id, item, location_group, object_id) %>% 
    # TODO: path objects get a 0 here for 'correct_molecule' but that should probably be an NA
    mutate(correct_molecule = ifelse(str_detect(object_id, "carbon_dioxide|methane"), 1, 0)) %>% 
    group_by(location_group) %>% 
    mutate(nh = sum(item == "head"), 
           nt = sum(item == "tail"),
           nm = sum(item == "molecule"),
           cm = max(correct_molecule), 
           has_htm = ifelse(nh >= 1 & nt >= 1 & nm >= 1 & cm >= 1, 1, 0), 
           has_ht = ifelse(nh >= 1 & nt >= 1 & nm == 0, 1, 0)) %>% 
    ungroup() 
  
  ## get info on whether groups have (head, tail, molecule), (head, tail)     
  df5 <- 
    df4 %>% 
    group_by(user_business_key, submission_id, location_group, has_htm, has_ht) %>% 
    summarise(n_htm = max(has_htm),  
              n_ht = max(has_ht)) %>% 
    group_by(user_business_key, submission_id) %>% 
    summarise(has_htm = max(has_htm), 
              has_ht = max(has_ht), 
              n_htm = sum(n_htm),  
              n_ht = sum(n_ht)) %>% 
    ungroup()
  
  ## downward deflection: 
  # get info on whether line from a (head, tail, molecule) group is pointing down
  df6 <- left_join(df4, df, by = c("user_business_key", "submission_id", "pt", "item", "object_id")) 
  df7 <- 
    df6 %>% 
    select(user_business_key, submission_id, id, 
           location_group, has_htm, item, x_axis, y_axis) %>% 
    mutate(head_with_molecule = ifelse(item == "head" & has_htm == 1, 1, 0)) %>% 
    group_by(id) %>% 
    mutate(tail_y_axis = ifelse(item == "tail", y_axis, 0), 
           tail_y_axis = max(tail_y_axis), 
           ht_diff = ifelse(head_with_molecule == 1, (y_axis - tail_y_axis), 0), 
           htm_ht_diff_pos = ifelse(ht_diff > 0, 1, 0)) %>% 
    ungroup() %>% 
    group_by(user_business_key, submission_id) %>% 
    summarise(has_htm_ht_diff_pos = max(htm_ht_diff_pos, na.rm = TRUE), 
              n_htm_ht_diff_pos = sum(htm_ht_diff_pos, na.rm = TRUE))
  
  df8 <- left_join(df5, df7, by = c("user_business_key", "submission_id"))
  df8
}

subs <- select(system, submission_id, user_business_key) %>% distinct()
ed <- map_dfr(subs$submission_id, get_edist)

## feature generation based on difference between head and tail
# ht_difference <- 
#   cs_exs %>% 
#   select(body_actor_account_name, submission_id, id, item, y_axis) %>% 
#   spread(item, y_axis) %>% 
#   mutate(ht_diff = (head - tail), 
#          flag_positive = ifelse(ht_diff > 0, 1, 0), 
#          flag_negative = ifelse(ht_diff < 0, 1, 0)) %>% 
#   group_by(body_actor_account_name, submission_id) %>% 
#   summarise(n_ht_diff_pos = sum(flag_positive, na.rm = TRUE), 
#             n_ht_diff_neg = sum(flag_negative, na.rm = TRUE)) %>% 
#   ungroup()
# 
# 
# level_27_features <- left_join(ed, ht_difference, 
#                                by = c("body_actor_account_name",
#                                       "submission_id")) 

remove()
