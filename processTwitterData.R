
processTwitterData <- function(datasetString) {
  
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
                          states_abbr[match(temp.st, states)], state))%>%
    select(state, city, everything()) %>%
    arrange(state, city)
  
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
  
  dt <- dt %>% select("state", "city", "LAT_min", "LONG_min", "LAT_max", 
                          "LONG_max", everything(), -c(X))
  
  return(dt)
}
