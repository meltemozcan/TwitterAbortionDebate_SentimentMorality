
# helper to make minor adjustments/formatting changes to some geo fields
processTwitterGeoFields <- function(ds) {
  library(maps)
  cit <- us.cities
  # cit[,1] <- sub('...$', "", cit[,1])
  
  
  # read in, format state name and abbreviations
  st <- read.csv("Data/external/states.csv")
  states <- st[,1]
  states_abbr <- st[,2]
  states_cap <- toupper(states)
  
  # retrieve the state abbr following the comma in full geo name
  ds$state <- sapply(strsplit(ds$geo.full_name, ', '), `[`, 2)
  #reorder
  ds <- ds %>%  dplyr::select(state,geo.name, geo.full_name, everything())
  #temp vec of location before the comma
  ds$temp.st <- sapply(strsplit(ds$geo.full_name, ', '), `[`, 1)
  
  # create new variable, city, which takes the value in geo.name where state != USA
  # and the location before the comma can be found in the vector of states
  # where state == USA and the location before the comma can be found in 'states',
  # replace with the state abbr. 
  ds <- ds %>%
    mutate(city = ifelse(state !="USA" & !(temp.st %in% states), 
                         geo.name, NA)) %>%
    mutate(state = ifelse(state =="USA" & temp.st %in% states, 
                          states_abbr[match(temp.st, states)], state))
  
  # mismatching locations, if matches a known city/state, replace
  mism <- unique(ds$state)[!unique(ds$state) %in% st[,2]]
  
  ds <- ds %>%
    mutate(state = ifelse(state %in% mism & state %in% cit[,1], 
                          states_abbr[match(state, st[,2])], state))%>%
    mutate(state = ifelse(state %in% mism & state %in% st[,1], 
                          states_abbr[match(state, st[,1])], state))
  # if just USA, update state to NA
  ds <- ds %>% mutate(state = ifelse(state == "USA", NA, state))
  # hard code some known locations
  ds$state <- ifelse(ds$state %in% c("New York City", "Brooklyn", "Queens", 
                                     "New York County"), "NY", ds$state)
  # for now also removing Virgin Islands and Puerto Rico as I don't have the
  # external data for these
  
  unknownst <- unique(ds$state)[!unique(ds$state) %in% st[,2]]
  ds <- ds %>% mutate(state = ifelse(state %in% unknownst, NA, state))
  
  # geo full name refers to either state, USA or city, state -- too much noise.
  # drop temp and noisy location vars
  ds <- ds %>%  dplyr::select(-c(temp.st, geo.name, geo.full_name))
  
  # make new vars for coordinates of bbox
  # drop '[ , ]'
  ds$temp <- ds$geo.geo.bbox
  ds$temp <- sub('.', "",ds$temp) # drop first letter
  ds$temp <- sub('.$', "",ds$temp) # last
  temp <- unlist(strsplit(ds$temp, ", "))
  ds$LONG_min <- as.numeric(temp[seq(2, length(temp), 4)])
  ds$LAT_min <- as.numeric(temp[seq(1, length(temp), 4)])
  ds$LONG_max <- as.numeric(temp[seq(4, length(temp), 4)])
  ds$LAT_max <- as.numeric(temp[seq(3, length(temp), 4)])
  ds <- ds %>% dplyr::select(-c(temp))
  
  return(ds)
}