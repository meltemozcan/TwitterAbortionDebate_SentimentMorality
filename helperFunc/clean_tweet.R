clean_tweet <- function(dataset) {
  
  minor_tweet_format <- function(unclean_tweet) {
    library(stringr)
    #https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r
    # clean_tweet = gsub("&amp", "", unclean_tweet)
    # clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
    # clean_tweet = gsub("@\\w+", "", clean_tweet)
    # clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
    # clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
    # clean_tweet = gsub("http\\w+", "", clean_tweet)
    # clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
    # clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
    #ref: ( Hicks , 2014)
    #get rid of unnecessary spaces
    # clean_tweet <- str_replace_all(clean_tweet," ","")
    # Take out retweet header
    # clean_tweet <- str_replace(unclean_tweet,"RT @[a-z,A-Z]*: ","")
    # Get rid of references to other screennames
    clean_tweet <- str_replace_all(unclean_tweet,"@[a-z,A-Z]*","")   
    clean_tweet <- str_replace_all(clean_tweet," ","") 
    clean_tweet <- tolower(clean_tweet)
    #ref: (Stanton 2013)
    return(clean_tweet)
  }
  
  # call helper function that removes account handles, RT headers, turns all 
  # to lowercase
  dataset$cleantext <- minor_tweet_format(dataset$vader_preprocessed_text)
  
  # get rec ids for tweets with only empty strings after removing 
  # foreign characters (but still retaining rows where vader_compound!=0 to keep
  # emojis)
  rec_ids_empty <- unlist(
    dataset[dataset$cleantext == "" & dataset$vader_compound == 0, "id"])
  dataset <- dataset[!(dataset$id %in% rec_ids_empty), ]
  dataset <- dataset %>% arrange(created_at, author.id)
  return(dataset)

}
