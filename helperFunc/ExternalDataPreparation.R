# Compile external data sources
## Add new datasets at the end as needed

library(dplyr)
st <- read.csv("Data/external/states.csv")
colnames(st) <- c("state", "abbr")
st <- as.data.frame(st)
states <- st[,1]
states_abbr <- st[,2]
states_cap <- toupper(st[,1])

# State level abortion information & provider counts
abortion <- read.csv("external/providers_clean_state_lev.csv") # 51 x 13
#abortion <- abortion %>% select(-X)

# 2020 election votes by state 
# https://www.presidency.ucsb.edu/statistics/elections/2020
votes2020 <- readxl::read_xlsx("external/AmericanPresidencyProject2020Votes.xlsx")
votes2020 <- votes2020 %>% filter(STATE %in% states)
# substitute state names w abbr
votes2020$state <- states_abbr[match(votes2020$STATE, states)]
votes2020 <- votes2020 %>% select(state, BidenPerc,TrumpPerc, OtherPerc)
votes2020 <- votes2020 %>% 
  rename(BidenPerc2020 = BidenPerc,
         TrumpPerc2020 = TrumpPerc, 
         OtherPerc2020 = OtherPerc) # 51 x 4

us_regions <- read.csv("external/us_census_bureau_regions_and_divisions.csv")
us_regions <- us_regions %>% rename(state = State.Code, region = Region, 
                                    division = Division) # 51 x 4

# American Community Survey 2021
censusdatw <- read.csv('external/ACS_2021_censusData.csv') %>% select(-X) # 51 x 49

# Party Affiliation Pew 2014
partyaff2014 <- readxl::read_xlsx('external/PartyAffiliationByState_Pew2014.xlsx')

ideology2018 <- readxl::read_xlsx('external/politicalIdeology2018Gallup.xlsx')
ideology2018 <- left_join(st, ideology2018, by = join_by(state))
ideology2018 <- ideology2018 %>% select(-c(state, N))
colnames(ideology2018)[1] <- "state"

politics2018 <- readxl::read_xlsx('external/politicalAffiliation2018_Gallup.xlsx')
politics2018 <- left_join(st, politics2018, by = join_by(state))
politics2018 <- politics2018 %>% select(-state)
colnames(politics2018)[1] <- "state"

religious2017 <- readxl::read_xlsx('external/religiousPercentagesState2017_statista.xlsx')
religious2017 <- left_join(st, religious2017, by = join_by(state))
religious2017 <- religious2017 %>% select(-state)
religious2017[,2:4] <- religious2017[,2:4] / 100
colnames(religious2017)[1] <- "state"

#https://www.statista.com/statistics/660661/abortion-rate-united-states-by-state/
abortionRate2020 <- readxl::read_xlsx('external/abortionRatesByState2020_CDC.xlsx')
abortionRate2020 <- left_join(st, abortionRate2020, by = join_by(state))
abortionRate2020 <- abortionRate2020 %>% select(-state)
colnames(abortionRate2020)[1] <- "state"  

# https://www.statista.com/statistics/1307644/share-of-reported-legal-abortions-out-of-state-residents-in-the-us-by-state/
outofstate2020percent <- readxl::read_xlsx('external/legalAbortionsOutOfState2020_CDC.xlsx')
outofstate2020percent <- left_join(st, outofstate2020percent, by = join_by(state))
outofstate2020percent <- outofstate2020percent %>% select(-state)
colnames(outofstate2020percent)[1] <- "state"


# Merge data
state_level <- abortion %>% 
  left_join(abortionRate2020, by = join_by(state))%>%
  left_join(outofstate2020percent, by = join_by(state))%>%
  left_join(us_regions, by = join_by(state)) %>% 
  left_join(censusdatw, by = join_by(state)) %>%
  left_join(votes2020, by = join_by(state)) %>%
  left_join(ideology2018, by = join_by(state)) %>%
  left_join(politics2018, by = join_by(state))%>%
  left_join(religious2017, by = join_by(state))

state_level <- state_level %>% select(-X)
  # 51 x 85
#colnames(state_level)

write.csv(state_level, 'external/StateLevelExternalData2021.csv')
