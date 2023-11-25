
processTwitterData <- function(datasetString) {
  library(maps)
  cit <- us.cities
  cit[,1] <- sub('...$', "", cit[,1])
  # read in the list of variables to drop
  drop_vars <- unlist(read.csv('external/drops.csv'))
  # read in, format state name and abbreviations
  st <- read.csv("external/states.csv")
  states <- st[,1]
  states_abbr <- st[,2]
  states_cap <- toupper(st[,1])
  
  # read in the Twitter dataset
  dt <- read.csv(datasetString)
  
  # rename variables as needed
  dt <- dt %>% 
    rename(like_count = public_metrics.like_count,
           reply_count = public_metrics.reply_count, 
           retweet_count = public_metrics.retweet_count,
           auth_follower_count = author.public_metrics.followers_count,
           auth_following_count = author.public_metrics.following_count,
           auth_listed_count = author.public_metrics.listed_count,
           auth_tweet_count = author.public_metrics.tweet_count)
  
  # drop previously defined variables that are not of interest
  dt <- select(dt, -all_of(drop_vars))
  dt %>% select(conversation_id, author.id, geo.full_name, 
                  geo.name, geo.geo.bbox, everything())
  dt$author.id <- as.character(dt$author.id)
  dt$conversation_id <- as.character(dt$conversation_id)
  # retrieve the state abbr following the comma in full geo name
  dt$state <- sapply(strsplit(dt$geo.full_name, ', '), `[`, 2)
  #reorder
  dt <- dt %>% select(state,geo.name, geo.full_name, everything())
  #temp vec of location before the comma
  dt$temp.st <- sapply(strsplit(dt$geo.full_name, ', '), `[`, 1)
  
  # create new variable, city, which takes the value in geo.name where state != USA
  # and the location before the comma can be found in the vector of states
  # where state == USA and the location before the comma can be found in 'states',
  # replace with the state abbr. 
  dt <- dt %>%
    mutate(city = ifelse(state !="USA" & !(temp.st %in% states), 
                         geo.name, NA)) %>%
    mutate(state = ifelse(state =="USA" & temp.st %in% states, 
                          states_abbr[match(temp.st, states)], state))
  
    # mismatching locations, if known city, replace with state
    mism <- unique(dt$state)[!unique(dt$state) %in% st[,2]]
    dt <- dt %>%
      mutate(state = ifelse(state %in% mism & state %in%cit[,1], 
                            states_abbr[match(state, st[,2])], state))%>%
      mutate(state = ifelse(state %in% mism & state %in% st[,1], 
                            states_abbr[match(state, st[,1])], state))
    dt <- dt %>% mutate(state=ifelse(state=="USA",NA, state))
    dt$state <- ifelse(dt$state %in% c("New York City", "Brooklyn", "Queens", 
                                           "New York County"), "NY", dt$state)
    # for now also removing Virgin Islands and Puerto Rico as I don't have the
    # external data for these

    unknownst <- unique(dt$state)[!unique(dt$state) %in% st[,2]]
    dt <- dt %>% mutate(state = ifelse(state %in% unknownst, NA, state))
  
  
  # geo full name refers to either state, USA or city, state -- too much noise.
  # drop temp and noisy location vars
  dt <- dt %>% select(-c(temp.st, geo.name, geo.full_name, text))
  
  # make new vars for coordinates of bbox
  # drop '[ , ]'
  dt$temp <- dt$geo.geo.bbox
  dt$temp <- sub('.', "",dt$temp) # drop first letter
  dt$temp <- sub('.$', "",dt$temp) # last
  temp <- unlist(strsplit(dt$temp, ", "))
  dt$LONG_min <- as.numeric(temp[seq(2, length(temp), 4)])
  dt$LAT_min <- as.numeric(temp[seq(1, length(temp), 4)])
  dt$LONG_max <- as.numeric(temp[seq(4, length(temp), 4)])
  dt$LAT_max <- as.numeric(temp[seq(3, length(temp), 4)])
  
  dt$tweeted_at <- as_datetime(dt$created_at)
  dt$TwitterAccountCreatedAt <- as_datetime(dt$author.created_at)
  dt <- dt %>% mutate(date_of_tweet = date(tweeted_at))
  
  dt <- dt %>% select("tweeted_at", "author.id",  "state", "city", 
                      "LAT_min", "LONG_min", "LAT_max", "LONG_max", 
                      everything(),
                      -c(X, created_at, author.created_at, `Unnamed..0.1`))
  dt <- dt %>% arrange(tweeted_at, author.id, state, city) 
  
  return(dt)
}
