### libraries
###############################################################################################################################################
library(tidyverse)
library(tidytext)
library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)
library(stringi)
library(magrittr)
library(rtweet)
library(textdata)

### loading data 
###############################################################################################################################################
df = read.csv("C:\\Users\\abhis\\Documents\\Duke University\\IDS 703 Text Analysis\\tweets_sd_illinois.csv")
df2 = read.csv("C:\\Users\\abhis\\Documents\\Duke University\\IDS 703 Text Analysis\\tweets_sd_illinois_2.csv")

df = rbind(df,df2)

copy = df

### data wrangling/viz
###############################################################################################################################################

#df$created_at =  format(as.Date(df$created_at), "%m-%y")

## Number of tweets
table1 = df %>%
  select(created_at) %>%
  mutate(
    created_at = as.character(created_at)
  ) %>%
  group_by(created_at) %>%
  dplyr::summarize(
    n = n()
  )
colnames(table1) = c("Date", "Number_of_tweets")

ggplot(table1, aes(x = Date, y = Number_of_tweets)) +
  geom_line(group = 1, color = "blue") +
  ggtitle("") +
  xlab("") +
  ylab("")

### tweets using ts_plot

ts_plot(df, "24 hours") +
  ggtitle("") +
  xlab("") +
  ylab("")

###############################################################################################################################################

### popular hashtags
table2 = df %>%
  select(hashtags) %>% na.omit() 

storage = NULL

for(i in table2$hashtags){
  if(str_detect(i, "\\(") == T){
    i = stri_extract_all_regex(i, '(?<=").*?(?=")')
    i = unlist(i)
    storage = append(storage,i)
  }
  else{
    storage = append(storage,i)
  }
}

storage = as.data.frame(storage)
colnames(storage) = "hashtag"

storage = storage %>% 
  filter(hashtag != ", ") %>%
  group_by(hashtag) %>%
  dplyr::summarise(
    Count = n()
  ) 

storage %<>% arrange(desc(Count)) %>% head(10)

ggplot(storage, aes(x = reorder(hashtag,Count), y = Count)) +
  geom_col(fill = "blue") +
  ggtitle("Top 10 hashtags") +
  xlab("Hashtag") +
  ylab("Count") +
  coord_flip()

###############################################################################################################################################

### location of twitter users
geo_code = lat_lng(df,  coords = c("coords_coords", "bbox_coords", "geo_coords"))
par(mar = c(0, 0, 0, 0))
maps::map("state","illinois", lwd = .25)
with(geo_code, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

###############################################################################################################################################

### tweet analysis
tweets.covid = df %>% dplyr::select(screen_name, text)
tweets.covid$stripped_text1 = gsub("https\\S+", "", tweets.covid$text)

tweets.covd_stem = tweets.covid %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1) 

cleaned_tweets.covid = tweets.covd_stem %>%
  anti_join(stop_words)

head(cleaned_tweets.covid)

# Top 10 words
cleaned_tweets.covid %>% 
  count(word, sort = T) %>%
  top_n(10) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique Words",
       title = "Unique word counts found in Illinois")

###############################################################################################################################################

# doing sentiment analysis
bing_covid = cleaned_tweets.covid %>%
  inner_join((get_sentiments("bing"))) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_covid %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(
    word = reorder(word,n)
  ) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

###############################################################################################################################################

# sentiment analysis but per tweet, this takes a while to finish

sentiment_twt = function(twt){
  twt_tbl = tibble(text = twt) %>%
    mutate(
      stripped_text = gsub("http\\S+", "", text)
    ) %>%
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup() %>%
    mutate(
      score = case_when(
        sentiment == 'negative' ~ n*(-1),
        sentiment == 'positive' ~ n * 1)
    )
  
  
  sent.score = case_when(
    nrow(twt_tbl) == 0~0,
    nrow(twt_tbl) > 0 ~ sum(twt_tbl$score))
  
  zero.type = case_when(
    nrow(twt_tbl) == 0~ "Type 1",
    nrow(twt_tbl) > 0 ~ "Type 2"
  )
  
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
  
}

tweet_sent = lapply(df$text, function(x){sentiment_twt(x)})


tweet_sent2 = bind_rows(
  tibble(
    name = "COVID19",
    score = unlist(purrr::map(tweet_sent, "score")),
    type = unlist(purrr::map(tweet_sent, "type"))
  )
)

ggplot(tweet_sent2, aes(x = score, fill = name)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of sentiments of twitter users",
    x = 'Individual score',
    y = "Count"
  )
