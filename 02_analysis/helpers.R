split_data <- function(data, split_ratio, seed) {
  set.seed(seed)
  
  split_size <- round(nrow(data) * split_ratio)
  
  in_train   <- sample(1:nrow(data), size = split_size)
  
  data$set <- "NA"
  data$set[ in_train]  <- "train"
  data$set[ -in_train] <- "test"
  
  data <- 
    data %>% 
    group_by(set) %>%
    nest()
  
  return(data)
}