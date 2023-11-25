rm_dupl_empty_noneng <- function(dataset) {
  # call helper function that removes account handles, RT headers, turns all 
  # to lowercase
  dataset$cleantext <- clean_tweet(dataset$vader_preprocessed_text)
  
 # dataset <- dataset %>% distinct()
  
  # dataset <- dataset %>%
  #   group_by(author.id, cleantext) %>%
  #   arrange(cleantext) %>%
  #   filter(row_number() == 1)
  
  # if i keep dataset <- dataset %>% ...%>% filter(row_number() == 1) in 
  # rm_dupl_empty_noneng.R 33099 rows deleted-- for now, comment it out, if there 
  # is time come back and investigate further if it correctly removes the records
  # within a given author.id for which cleantext is duplicated and keeps only one
  # instance of the duplicated cleantext per author, while retaining it if it's 
  # used by other authors
  
  
  
 ## dataset$cleantext <- sapply(dataset$cleantext, 
 ##                             function(x) gsub("[^\x01-\x7F]", "", x))
  
  # get rec ids for tweets with only empty strings after removing 
  # foreign characters (but still retaining rows where vader_compound!=0 to keep
  # emojis)
  rec_ids_empty <- unlist(
    dataset[dataset$cleantext == "" & dataset$vader_compound == 0, "record_id"])
  dataset <- dataset[!(dataset$record_id %in% rec_ids_empty), ]
  dataset <- dataset %>% arrange(tweeted_at, author.id)
  
  # test if working test <- tibble(hit = c(rep("a", 4), rep("b", 5)), 
  # indx = c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  # hit_start = c(7,7, 105, 131, 4, 7, 56, 64, 7), 
  # hit_end = c(112, 112, 126, 152, 82, 34, 83, 81, 112), 
  # stamp_score = c(NA, NA, 9.30, 9.49, NA, NA, NA, 8.16, NA), 
  # bit_score = c(76.2, 76.2, NA, NA, 84.7, 8.3, 0.3, NA, 76.2) 
  # )
  
  return(dataset)

}
