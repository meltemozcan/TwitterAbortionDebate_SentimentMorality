
processTwitterData2 <- function(ds) {
 
  # create new indicator variables for time windows surrounding the leak and 
  # overturn
  ds <- leak_overturn_52daywindows(ds)
  
  # format existing geo variables of interest, recode fields using existing info 
  # where possible to consolidate/clean up
  ds <- processTwitterGeoFields(ds)
  # remove records where the tweet was empty or only contained account handle(s),
  # perform other minor formatting
  ds <- clean_tweet(ds)
   # read in the list of variables to drop
  drop_vars <- unlist(read.csv('Data/external/drops.csv'))
  # drop previously defined variables that are not of interest
  ds <- dplyr::select(ds, -all_of(drop_vars))
  # reorder cols
  ds <- ds %>% dplyr::select(id, author.id, everything())
  
  # take subset of tweets in English
  
  ds <- ds %>% filter(lang %in% c('en', 'und', 'qme', 'qst'))
 
  # if not already in string form, convert to string
  # ds$author.id <- ifelse(is.character(ds$author.id), 
  #                        ds$author.id, as.character(ds$author.id))
  # ds$id <- ifelse(is.character(ds$id), ds$id, as.character(ds$id))
  
  # convert to datetime object
  ds$created_at <- as_datetime(ds$created_at)
  
  # create new variable for the date without the time stamp
  ds <- ds %>% mutate(date_of_tweet = date(created_at))
  
  # sequence a counter variable for a user's tweets on a day
  ds <- ds %>% 
    arrange(author.id, date_of_tweet, id) %>% 
    group_by(author.id, date_of_tweet) %>% 
    mutate(nth_tweet_on_date = as.numeric(factor(id)))%>%
    mutate(total_tweets_on_date = max(nth_tweet_on_date))
  
  ds <- ds %>% arrange(author.id) %>% group_by(author.id) %>% 
    mutate(total_days_tweeted = n_distinct(date_of_tweet),
           count_tweets = as.numeric(factor(id)))
  
  # add calendar info
  ds$month <- month(ds$created_at)
  ds$dayofweek <- wday(ds$created_at)
  ds$wknd <- ifelse(ds$dayofweek %in% c(6,7), 1, 0)
  
  ds <- ds %>%  dplyr::select("created_at", "author.id",  "state", "city",
                      everything()) #`Unnamed..0.1`
  
  if ("X" %in% colnames(ds)) {
    ds <- ds %>% select(-c(X))
  }
  
#  ds <- ds %>% select(- c(referenced_tweets.replied_to.id, conversation_id,
#                          author.id.string, id_str))
  
  ds <- ds %>% arrange(created_at, author.id, state, city) 
  
  
  # indicator variables
  ds$positive <- as.integer(ds$sentiment=="positive")
  ds$neutral <- as.integer(ds$sentiment=="neutral")
  ds$negative <- as.integer(ds$sentiment=="negative")
  
  ds <- ds[ds$prolife_prochoice != "throw_out",]
  
  ds$choice <- as.integer(ds$prolife_prochoice=="choice")
  ds$ab_neutral <- as.integer(ds$prolife_prochoice=="neutral")
  ds$life <- as.integer(ds$prolife_prochoice=="life")
  
  return(ds)
}
