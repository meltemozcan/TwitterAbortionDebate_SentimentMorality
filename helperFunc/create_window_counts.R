
# create subset of data pertaining to the three windows only, then add new 
#variables for total counts of particular types of tweets daily per author

create_window_counts <- function(dt) {

    ds <- dt[dt$pre52_Dobbs==1 | 
                  dt$btwn52_Dobbs_Overturn == 1 | 
                  dt$post52_Overturn == 1, ]

    # Create the daily counts of tweets per author reflecting a particular abortion stance
    dslife <- ds %>% 
      filter(prolife_prochoice=="life") %>%
      arrange(author.id, date_of_tweet, id) %>% 
      group_by(author.id, date_of_tweet) %>% 
      mutate(nth_lifetweet_on_date = as.numeric(factor(id)))%>%
      mutate(life_total_on_date = max(nth_lifetweet_on_date)) %>% ungroup()
    
    dslife_ids <- dslife[, c("author.id", "id", "date_of_tweet", "life_total_on_date")]
    ds <-left_join(ds, dslife_ids, by = c("author.id", "id", "date_of_tweet"))
    ds$life_total_on_date <- tidyr::replace_na(ds$life_total_on_date, 0)
    
    dschoice <- ds %>% 
      filter(prolife_prochoice=="choice") %>%
      arrange(author.id, date_of_tweet, id) %>% 
      group_by(author.id, date_of_tweet) %>% 
      mutate(nth_choicetweet_on_date = as.numeric(factor(id)))%>%
      mutate(choice_total_on_date = max(nth_choicetweet_on_date)) %>% ungroup()
    
    dschoice_ids <- dschoice[, c("author.id", "id", "date_of_tweet", "choice_total_on_date")]
    ds <-left_join(ds, dschoice_ids, by = c("author.id", "id", "date_of_tweet"))
    ds$choice_total_on_date <- tidyr::replace_na(ds$choice_total_on_date, 0)
    #any(is.na(ds$choice_total_on_date))
    sum(table(ds$choice_total_on_date))
    
    dsneut <- ds %>% 
      filter(prolife_prochoice=="neutral") %>%
      arrange(author.id, date_of_tweet, id) %>% 
      group_by(author.id, date_of_tweet) %>% 
      mutate(nth_neuttweet_on_date = as.numeric(factor(id)))%>%
      mutate(neu_total_on_date = max(nth_neuttweet_on_date)) %>% ungroup()
    
    dsneut_ids <- dsneut[, c("author.id", "id", "date_of_tweet", "neu_total_on_date")]
    ds <- left_join(ds, dsneut_ids, by = c("author.id", "id", "date_of_tweet"))
    ds$neu_total_on_date <- tidyr::replace_na(ds$neu_total_on_date, 0)
    
    # examine if correctly created
    check <- ds[, c("author.id", "id", "date_of_tweet", "nth_tweet_on_date",
                      "total_tweets_on_date", "choice_total_on_date",
                      "neu_total_on_date", "life_total_on_date")]
    
    
    
    # Create the daily counts of tweets per author reflecting a particular sentiment
    dsneg <- ds %>% 
      filter(sentiment=="negative") %>%
      arrange(author.id, date_of_tweet, id) %>% 
      group_by(author.id, date_of_tweet) %>% 
      mutate(nth_negtweet_on_date = as.numeric(factor(id)))%>%
      mutate(neg_total_on_date = max(nth_negtweet_on_date)) %>% ungroup()
    
    dsneg_ids <- dsneg[, c("author.id", "id", "date_of_tweet", "neg_total_on_date")]
    ds <-left_join(ds, dsneg_ids, by = c("author.id", "id", "date_of_tweet"))
    ds$neg_total_on_date <- tidyr::replace_na(ds$neg_total_on_date, 0)
    
    dspos <- ds %>% 
      filter(sentiment=="positive") %>%
      arrange(author.id, date_of_tweet, id) %>% 
      group_by(author.id, date_of_tweet) %>% 
      mutate(nth_postweet_on_date = as.numeric(factor(id)))%>%
      mutate(pos_total_on_date = max(nth_postweet_on_date)) %>% ungroup()
    
    dspos_ids <- dspos[, c("author.id", "id", "date_of_tweet", "pos_total_on_date")]
    ds <-left_join(ds, dspos_ids, by = c("author.id", "id", "date_of_tweet"))
    ds$pos_total_on_date <- tidyr::replace_na(ds$pos_total_on_date, 0)
   # sum(table(ds$pos_total_on_date))
    
    dssentneut <- ds %>% 
      filter(sentiment=="neutral") %>%
      arrange(author.id, date_of_tweet, id) %>% 
      group_by(author.id, date_of_tweet) %>% 
      mutate(nth_sentneuttweet_on_date = as.numeric(factor(id)))%>%
      mutate(sentneu_total_on_date = max(nth_sentneuttweet_on_date)) %>% ungroup()
    
    dssentneut_ids <- dssentneut[, c("author.id", "id", "date_of_tweet", "sentneu_total_on_date")]
    ds <- left_join(ds, dssentneut_ids, by = c("author.id", "id", "date_of_tweet"))
    ds$sentneu_total_on_date <- tidyr::replace_na(ds$sentneu_total_on_date, 0)
    
    # examine if correctly created
    check <- ds[, c("author.id", "id", "date_of_tweet", "nth_tweet_on_date",
                      "total_tweets_on_date", "pos_total_on_date",
                      "sentneu_total_on_date", "neg_total_on_date")]
    
    #moral nonmoral
    dsmoral <- ds %>% 
      filter(binary_morality=="moral") %>%
      arrange(author.id, date_of_tweet, id) %>% 
      group_by(author.id, date_of_tweet) %>% 
      mutate(nth_moraltweet_on_date = as.numeric(factor(id)))%>%
      mutate(moral_total_on_date = max(nth_moraltweet_on_date)) %>% ungroup()
    
    dsmoral_ids <- dsmoral[, c("author.id", "id", "date_of_tweet", "moral_total_on_date")]
    ds <-left_join(ds, dsmoral_ids, by = c("author.id", "id", "date_of_tweet"))
    ds$moral_total_on_date <- tidyr::replace_na(ds$moral_total_on_date, 0)
    #sum(table(ds$moral_total_on_date))
    
    dsnonmoral <- ds %>% 
      filter(binary_morality=="non-moral") %>%
      arrange(author.id, date_of_tweet, id) %>% 
      group_by(author.id, date_of_tweet) %>% 
      mutate(nth_nonmoraltweet_on_date = as.numeric(factor(id)))%>%
      mutate(nonmoral_total_on_date = max(nth_nonmoraltweet_on_date)) %>% ungroup()
    
    dsnonmoral_ids <- dsnonmoral[, c("author.id", "id", "date_of_tweet", "nonmoral_total_on_date")]
    ds <-left_join(ds, dsnonmoral_ids, by = c("author.id", "id", "date_of_tweet"))
    ds$nonmoral_total_on_date <- tidyr::replace_na(ds$nonmoral_total_on_date, 0)
    #sum(table(ds$nonmoral_total_on_date))
    # examine if correctly created
    check <- ds[, c("author.id", "id", "date_of_tweet", "nth_tweet_on_date",
                      "total_tweets_on_date", 
                      "moral_total_on_date", "nonmoral_total_on_date")]
    
    return(ds)
}