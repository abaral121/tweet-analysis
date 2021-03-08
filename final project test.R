library(tidyverse)
library(rtweet)
library(maps)

app_name            = "Jbear"
consumer_key        = "1px1kEwDL9kFgcwBHIzNGzQaM"
consumer_secret     = "zrQO5TvGyoGGO6WxvF0yItPKkgC6TCMfEN5iaiWlIpSzeGLhYT"
access_token        = "1220144645574942721-LRXaphq3bmQW2OCoVUperdYe35Tue2"
access_secret       = "crevqY7fwhifEHbYQaRb5dlJUYiUrW2ztSMCKu5hheeVc"

twitter_token = create_token(
  app             = app_name,
  consumer_key    = consumer_key,
  consumer_secret = consumer_secret,
  access_token    = access_token,
  access_secret   = access_secret,
  set_renv        = TRUE
)


covid_charlotte = search_tweets(query = "#corona", 
                          n = 50, 
                          include_rts = FALSE,
                          geocode = "35.22,-80.84,10mi"
                          )

covid_durham = search_tweets(query = "#COVID19", 
                                n = 50, 
                                include_rts = FALSE,
                                geocode = "35.77,-78.63,10mi"
)


covid_greensboro = search_tweets(query = "#COVID19", 
                             n = 50, 
                             include_rts = FALSE,
                             geocode = "36.07,-79.79,10mi"
)

