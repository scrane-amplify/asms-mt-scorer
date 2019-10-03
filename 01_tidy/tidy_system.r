library(tidyverse)
library(jsonlite)
library(here)
library(lubridate)
options(digits.secs = 3)

# Functions ----------------------------------------------------------------------------------------
is_latest_submission <- function(data) {
  # There are multiple submissions per user, and each submission has multiple rows associated with
  # the submission that span a range of time. To get the latest submission per user we will first
  # get the most recent time stamp per submission ID, then get the rows associated with the latest
  # time per user. This returns the input data frame with the addition of a new boolean variable 
  # that indicates if the row is part of the latest submission for all rows.
  data %>% 
    mutate(request_time = ymd_hms(request_time)) %>% 
    group_by(body_result_extensions_submission_id, body_actor_account_name) %>%
    mutate(new_max = max(request_time)) %>% 
    group_by(body_actor_account_name) %>% 
    mutate(is_latest_submission = case_when(new_max == max(new_max) ~ TRUE,
                                            TRUE ~ FALSE)) %>% 
    ungroup()
}

parse_labels <- function(json_object) {
  external_id <- json_object[["External ID"]]
  absorption_and_emission <- json_object[["Label"]]$`absorption_&_emission_`
  deflection <- json_object[["Label"]]$deflection
  activity_business_key <- str_split(json_object[["External ID"]], "_", simplify = TRUE)[1]
  user_business_key <- json_object[["External ID"]] %>% 
    str_remove(., ".png") %>% 
    str_split(., "_", simplify = TRUE) %>% 
    .[2]
  bind_cols(external_id = external_id, 
            absorption_and_emission = absorption_and_emission, 
            deflection = deflection, 
            activity_business_key = activity_business_key, 
            user_business_key = user_business_key)
}

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

get_edist <- function(i) {
  i <- system$submission_id[1]
  # get euclidean distances    
  df <- filter(system, submission_id == i) %>% 
    rowid_to_column("pt") 
  dist_eucl <- dist(df[ , 7:8], method = "euclidean")
  df1 <- as.data.frame(round(as.matrix(dist_eucl)[1:nrow(df), 1:nrow(df)], 1)) 
  
  # find sets 
  df2 <- 
    df1 %>% 
    rowid_to_column("pt") %>% 
    gather(k, v, -pt) %>% 
    arrange(pt, k) %>%
    mutate(pt = as.numeric(pt), 
           k = as.numeric(k), 
           flag = ifelse(v < 50, 1, 0)) %>% 
    filter(flag == 1) %>% 
    select(-flag, -v) %>% 
    group_by(pt) %>% 
    nest(k, .key = "location_group") %>% 
    #nest(location_group = c(k)) %>% 
    mutate(location_group = as.character(location_group)) %>% 
    ungroup() 
  
  df3 <- left_join(df, df2, by = "pt")
  
  df4 <- 
    df3 %>% 
    select(pt, body_actor_account_name, submission_id, item, location_group, object_id) %>% 
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
    group_by(body_actor_account_name, submission_id, location_group, has_htm, has_ht) %>% 
    summarise(n_htm = max(has_htm),  
              n_ht = max(has_ht)) %>% 
    group_by(body_actor_account_name, submission_id) %>% 
    summarise(has_htm = max(has_htm), 
              has_ht = max(has_ht), 
              n_htm = sum(n_htm),  
              n_ht = sum(n_ht)) %>% 
    ungroup()
  
  ## downward deflection: 
  # get info on whether line from a (head, tail, molecule) group is pointing down
  df6 <- left_join(df4, df, by = c("body_actor_account_name", "submission_id", "pt", "item", "object_id")) 
  df7 <- 
    df6 %>% 
    select(body_actor_account_name, submission_id, id, 
           location_group, has_htm, item, x_axis, y_axis) %>% 
    mutate(head_with_molecule = ifelse(item == "head" & has_htm == 1, 1, 0)) %>% 
    group_by(id) %>% 
    mutate(tail_y_axis = ifelse(item == "tail", y_axis, 0), 
           tail_y_axis = max(tail_y_axis), 
           ht_diff = ifelse(head_with_molecule == 1, (y_axis - tail_y_axis), 0), 
           htm_ht_diff_pos = ifelse(ht_diff > 0, 1, 0)) %>% 
    ungroup() %>% 
    group_by(body_actor_account_name, submission_id) %>% 
    summarise(has_htm_ht_diff_pos = max(htm_ht_diff_pos, na.rm = TRUE), 
              n_htm_ht_diff_pos = sum(htm_ht_diff_pos, na.rm = TRUE))
  
  df8 <- left_join(df5, df7, by = c("body_actor_account_name", "submission_id"))
  df8
}

subs <- select(system, submission_id, body_actor_account_name) %>% distinct()
ed <- map_dfr(subs$submission_id, get_edist)

remove()
