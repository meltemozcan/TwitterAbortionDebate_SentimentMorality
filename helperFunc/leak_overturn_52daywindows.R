# Add three new variables to denote the borders of the 52 day windows before, 
# between, and after the leak and the overturn

leak_overturn_52daywindows <- function(ds) {
  # overturn: taking npr https://www.npr.org/2022/06/24/1102305878/supreme-court-abortion-roe-v-wade-decision-overturn as first reporting, 3:43 UTC
  
  # leak: https://www.politico.com/news/2022/05/02/supreme-court-abortion-draft-opinion-00029473
  # 08:32 PM EDT on the 2nd is 1:32AM UTC on the 3rd
  
  # 52 days between leak and overturn, find counts bef and after
  ds$created_at <- as_datetime(ds$created_at)
  
  overturn_timestamp <- as_datetime("2022-06-24 03:43:00")
  leak_timestamp <- as_datetime("2022-05-03 01:32:00")
  
  ds <- ds %>% 
    arrange(created_at) %>%
    mutate(btwn52_Dobbs_Overturn = 
             ifelse(created_at > leak_timestamp & created_at < leak_timestamp, 1, 0),
           pre52_Dobbs = 
             ifelse(created_at > (leak_timestamp - days(52)) & created_at < leak_timestamp, 1, 0),
           post52_Overturn = 
             ifelse(created_at < (overturn_timestamp + days(52)) & created_at > leak_timestamp, 1, 0))
  ds <-  ds %>% 
    arrange(created_at) %>%
    mutate(beforeOverturn = 
             as.integer(created_at < overturn_timestamp),
           afterOverturn = 
             as.integer(created_at >= overturn_timestamp),
           beforeLeak = 
             as.integer(created_at < leak_timestamp),
           afterLeak =
             as.integer(created_at >= leak_timestamp)
    )
  return(ds)
}
