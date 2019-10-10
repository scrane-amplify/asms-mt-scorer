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