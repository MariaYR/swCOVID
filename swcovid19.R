#swcovid19
library(dplyr)
library(tm)
library(ggplot2)
library(RWeka)
library(gplots)
library(corrplot)
library(RColorBrewer)
library(stm)
library(streamR)
library (tidyverse)
library(tidytext)
library(stminsights)
library(stringr)
library (Rtsne)
library(rsvd)
library(geometry)
library(igraph)
library(rtweet)

## store api keys (these are fake example values; replace with your own keys)
api_key <- "zmUkEB27bLv5NytcPVsfc0Wa1"
api_secret_key <- "riv4j7lwHIVIs8mhH9pqQe4APp8ncV8YvNevaxh26iiVFwuwbz"
access_token <- "235262823-O6sLg6iMkAH7whP5Oj6IHM8K9WuWzY3eqw7swEjb"
access_token_secret <- "WFlkUA7MlkmgrchIrvuUYj3KDPSNcIqwZbM3gTcFkYS8H"

## authenticate via web browser
my_token <- create_token(
  app = "JumpingJellies",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

my_data <- search_tweets(q = "maga",n = 500,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
              retryonratelimit = TRUE,verbose = TRUE)
names(my_data)
swcovid_tweets <- search_tweets(q = "swthrucovid",n = 500000,type = "recent",include_rts = TRUE, parse = TRUE,token = my_token,
                             retryonratelimit = TRUE,verbose = TRUE)
#only 131 tweets

swcovid_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWthruCOVID Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## create lat/lng variables using all available tweet and profile geo-location data
covid_latlong <- lat_lng(swcovid_tweets)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(covid_latlong, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

# view column with screen names - top 6
head(swcovid_tweets$screen_name)
unique(swcovid_tweets$screen_name)

# what users are tweeting with #swthrucovid
swcovid_users <- search_users("#swthrucovid",
                      n = 500)
#0 users

#try swtech

swtech_tweets <- search_tweets(q = "swtech",n = 500000,type = "recent",include_rts = TRUE, parse = TRUE,token = my_token,
                               retryonratelimit = TRUE,verbose = TRUE)

#218 tweets

swtech_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWtech Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## create lat/lng variables using all available tweet and profile geo-location data
swtech_latlong <- lat_lng(swtech_tweets)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(swtech_latlong, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))


# what users are tweeting with #swtech
swtech_users <- search_users("#swtech",
                              n = 500)

# just view the first 2 users - the data frame is large!
head(swtech_users, n = 2)

# how many locations are represented
length(unique(swtech_users$location))
#23

# install descriptive stats libraries
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)

#where are users from 
swtech_users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(15) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "#SWtech Twitter users during #COVID19- unique locations ")

names(swtech_users)

# remove http elements manually
swtech_tweets$stripped_text <- gsub("http.*","",  swtech_tweets$text)
swtech_tweets$stripped_text <- gsub("https.*","", swtech_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swtech_tweets_clean <- swtech_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
swtech_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #swtech tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
cleaned_tweet_words <- swtech_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swtech_tweets_paired_words <- swtech_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swtech_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

library(tidyr)
swtech_tweets_separated_words <- swtech_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swtech_tweets_filtered <- swtech_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swtech_words_counts <- swtech_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swtech_words_counts)

# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swtech_words_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - SWtech",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swtech_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #swtech.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#socialworkmonthtweets
swmonth_tweets <- search_tweets(q = "socialworkmonth",n = 500000,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
                               retryonratelimit = TRUE,verbose = TRUE)

#701

#tweets over collection time (+/- 9 days)
swmonth_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWmonth Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# install descriptive stats libraries
# library(ggplot2)
# library(dplyr)
# library(tidytext)
# library(igraph)
# library(ggraph)

# remove http elements manually
swmonth_tweets$stripped_text <- gsub("http.*","",  swmonth_tweets$text)
swmonth_tweets$stripped_text <- gsub("https.*","", swmonth_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swmonth_tweets_clean <- swmonth_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words 
swmonth_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #SWmonth tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
swmonth_cleaned_tweet_words <- swmonth_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
swmonth_cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in #swmonth tweets",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
#library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swmonth_tweets_paired_words <- swmonth_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swmonth_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#library(tidyr)
swmonth_tweets_separated_words <- swmonth_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swmonth_tweets_filtered <- swmonth_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swmonth_words_counts <- swmonth_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swmonth_words_counts)

# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swmonth_words_counts %>%
  filter(n >= 12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - SWmonth",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swmonth_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #swmonth",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#swmonth tweets
swmonth2_tweets <- search_tweets(q = "swmonth",n = 500000,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
                                retryonratelimit = TRUE,verbose = TRUE)
#228

#tweets over collection time (+/- 9 days)
swmonth2_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWmonth Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# install descriptive stats libraries
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)

# remove http elements manually
swmonth2_tweets$stripped_text <- gsub("http.*","",  swmonth2_tweets$text)
swmonth2_tweets$stripped_text <- gsub("https.*","", swmonth2_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swmonth2_tweets_clean <- swmonth2_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
swmonth2_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #swmonth tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
swmonth2_cleaned_tweet_words <- swmonth2_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
swmonth2_cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in #swmonth tweets",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swmonth2_tweets_paired_words <- swmonth2_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swmonth2_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

library(tidyr)
swmonth2_tweets_separated_words <- swmonth2_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swmonth2_tweets_filtered <- swmonth2_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swmonth2_words_counts <- swmonth2_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swmonth2_words_counts)

# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swmonth2_words_counts %>%
  filter(n >= 12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - SWmonth",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swmonth2_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #swmonth.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################


#swgenerationsstrong tweets
swgens_tweets <- search_tweets(q = "swgenerationsstrong",n = 500000,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
                                retryonratelimit = TRUE,verbose = TRUE)
#152

#tweets over collection time (+/- 9 days)
swgens_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWgenerationsStrong Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# install descriptive stats libraries
# library(ggplot2)
# library(dplyr)
# library(tidytext)
# library(igraph)
# library(ggraph)

# remove http elements manually
swgens_tweets$stripped_text <- gsub("http.*","",  swgens_tweets$text)
swgens_tweets$stripped_text <- gsub("https.*","", swgens_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swgens_tweets_clean <- swgens_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
swgens_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #SWGenerationsStrong tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
swgens_cleaned_tweet_words <- swgens_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
swgens_cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in #SWGenerationsStrong tweets",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
# library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swgens_tweets_paired_words <- swgens_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swgens_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

# library(tidyr)

swgens_tweets_separated_words <- swgens_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swgens_tweets_filtered <- swgens_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swgens_words_counts <- swgens_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swgens_words_counts)

# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swgens_words_counts %>%
  filter(n >= 12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - SwGenerationsStrong",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swgens_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #SWgenerationsStrong.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#socialworkmonthtweets
swers_tweets <- search_tweets(q = "socialworkers",n = 500000,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
                                retryonratelimit = TRUE,verbose = TRUE)

#792

#tweets over collection time (+/- 9 days)
swers_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #socialworkers Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# # install descriptive stats libraries
# library(ggplot2)
# library(dplyr)
# library(tidytext)
# library(igraph)
# library(ggraph)

# remove http elements manually
swers_tweets$stripped_text <- gsub("http.*","",  swers_tweets$text)
swers_tweets$stripped_text <- gsub("https.*","", swers_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swers_tweets_clean <- swers_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
swers_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #socialworkers tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
swers_cleaned_tweet_words <- swers_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
swers_cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in #socialworker tweets",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
# library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swers_tweets_paired_words <- swers_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swers_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

# library(tidyr)
swers_tweets_separated_words <- swers_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swers_tweets_filtered <- swers_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swers_words_counts <- swers_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swers_words_counts)
# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swers_words_counts %>%
  filter(n >= 12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - socialworkers",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swers_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #socialworkers.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#sswr timeline
sswr <- get_timeline("SSWRorg", n = 3200)
#88 tweets

##############################
##############################################
###################################################################


#cswe timeline
cswe <- get_timeline("CSocialWorkEd", n = 3200)
names(cswe)
#3197 tweets

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
cswe %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by CSWE",
    subtitle = "Twitter status (tweet) counts aggregated by day from march 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


# remove http elements manually
cswe$stripped_text <- gsub("http.*","",  cswe$text)
cswe$stripped_text <- gsub("https.*","", cswe$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
cswe_tweets_clean <- cswe %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)
  

# plot the top 15 words -- notice any issues?
cswe_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @CSWE tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
cswe_cleaned_tweet_words <- cswe_tweets_clean %>%
  anti_join(stop_words) 

names(cswe_cleaned_tweet_words)

# plot the top 15 words
cswe_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @CSWE tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

#network of words

# remove punctuation, convert to lowercase, add id for each tweet!
cswe_tweets_paired_words <- cswe %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

cswe_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

cswe_tweets_separated_words <- cswe_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

names(cswe_tweets_separated_words)

cswe_tweets_filtered <- cswe_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

names(cswe_tweets_filtered)

# new bigram counts:
cswe_words_counts <- cswe_tweets_filtered %>%
  count(word1, word2, sort = TRUE)%>%
  dplyr::select(created_at)

names(cswe_words_counts)
head(cswe_words_counts)

# plot climate change word network
# (plotting graph edges is currently broken)
cswe_words_counts %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets from CSWE",
       #subtitle = "During COVID-19 (After 3/1/2020) ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- cswe_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @CSWE.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#IFSW timeline
ifsw <- get_timeline("ifsw", n = 3200)
#1132 tweets 

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
ifsw %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by IFSW",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
ifsw$stripped_text <- gsub("http.*","",  ifsw$text)
ifsw$stripped_text <- gsub("https.*","", ifsw$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
ifsw_tweets_clean <- ifsw %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)

names(ifsw_tweets_clean)
names(ifsw)

# plot the top 15 words -- notice any issues?
ifsw_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @IFSW tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
isfw_cleaned_tweet_words <- ifsw_tweets_clean %>%
  anti_join(stop_words) 

names(isfw_cleaned_tweet_words)

# plot the top 15 words
isfw_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @IFSW tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")


# join sentiment classification to the tweet words
bing_word_counts <- cswe_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @CSWE.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################


#nasw timeline
nasw <- get_timeline("nasw", n = 3200)
#3191 twets

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
nasw %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


# remove http elements manually
nasw$stripped_text <- gsub("http.*","",  nasw$text)
nasw$stripped_text <- gsub("https.*","", nasw$stripped_text)

head(nasw$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_tweets_clean <- nasw %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_cleaned_tweet_words <- nasw_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_cleaned_tweet_words)

# plot the top 15 words
nasw_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")


# join sentiment classification to the tweet words
bing_word_counts <- nasw_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWNYC timeline
naswnyc <- get_timeline("naswnyc", n = 3200)
#3098 tweets 

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswnyc %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW_NYC",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswnyc$stripped_text <- gsub("http.*","",  naswnyc$text)
naswnyc$stripped_text <- gsub("https.*","", naswnyc$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswnyc_tweets_clean <- naswnyc %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswnyc_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWNYC tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswnyc_cleaned_tweet_words <- naswnyc_tweets_clean %>%
  anti_join(stop_words) 

names(cswe_cleaned_tweet_words)

# plot the top 15 words
naswnyc_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW_NYC tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswnyc_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWNYC.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NASWNJ timeline
naswnj <- get_timeline("NASWNJ", n = 3200)
#3197

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswnj %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW_NJ",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswnj$stripped_text <- gsub("http.*","",  naswnj$text)
naswnj$stripped_text <- gsub("https.*","", naswnj$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswnj_tweets_clean <- naswnj %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswnj_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWNJ tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswnj_cleaned_tweet_words <- naswnj_tweets_clean %>%
  anti_join(stop_words) 

names(naswnj_cleaned_tweet_words)

# plot the top 15 words
naswnj_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWNJ tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswnj_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWNJ.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NASWNC timeline
naswnc <- get_timeline("NASWNC", n = 3200)
#3182

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswnc %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASWNC",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswnc$stripped_text <- gsub("http.*","",  naswnc$text)
naswnc$stripped_text <- gsub("https.*","", naswnc$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswnc_tweets_clean <- naswnc %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
naswnc_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-NC tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswnc_cleaned_tweet_words <- naswnc_tweets_clean %>%
  anti_join(stop_words) 

names(naswnc_cleaned_tweet_words)

# plot the top 15 words
naswnc_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWNC tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswnc_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWNC.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWMI timeline NASWMI
naswmi <- get_timeline("NASWMI", n = 3200)
#3196

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswmi %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW_MI",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswmi$stripped_text <- gsub("http.*","",  naswmi$text)
naswmi$stripped_text <- gsub("https.*","", naswmi$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswmi_tweets_clean <- naswmi %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
naswmi_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-MI tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswmi_cleaned_tweet_words <- naswmi_tweets_clean %>%
  anti_join(stop_words) 

names(naswmi_cleaned_tweet_words)

# plot the top 15 words
naswmi_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-MI tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswmi_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-MI.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWMA timeline
naswma <- get_timeline("NASWMA", n = 3200)
#3196

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswma %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-MA",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswma$stripped_text <- gsub("http.*","",  naswma$text)
naswma$stripped_text <- gsub("https.*","", naswma$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswma_tweets_clean <- naswma %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswma_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-MA tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswma_cleaned_tweet_words <- naswma_tweets_clean %>%
  anti_join(stop_words) 

names(naswma_cleaned_tweet_words)

# plot the top 15 words
naswma_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-MA tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswma_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-MA.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NASWAZ timeline
naswaz <- get_timeline("NASWAZ", n = 3200)
#1015

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswaz %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-AZ",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswaz$stripped_text <- gsub("http.*","",  naswaz$text)
naswaz$stripped_text <- gsub("https.*","", naswaz$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswaz_tweets_clean <- naswaz %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words
naswaz_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-AZ tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswaz_cleaned_tweet_words <- naswaz_tweets_clean %>%
  anti_join(stop_words) 

names(naswaz_cleaned_tweet_words)

# plot the top 15 words
naswaz_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-AZ tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswaz_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-AZ.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASW_Kentucky timeline
naswkentucky <- get_timeline("NASW_Kentucky", n = 3200)
#3198

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswkentucky %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-KY",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswkentucky$stripped_text <- gsub("http.*","",  naswkentucky$text)
naswkentucky$stripped_text <- gsub("https.*","", naswkentucky$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswkentucky_tweets_clean <- naswkentucky %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswkentucky_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-KY tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswkentucky_cleaned_tweet_words <- naswkentucky_tweets_clean %>%
  anti_join(stop_words) 

names(naswkentucky_cleaned_tweet_words)

# plot the top 15 words
naswkentucky_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-KY tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswkentucky_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-KY.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASW_GA timeline
naswga <- get_timeline("NASW_GA", n = 3200)
#399

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswga %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-GA",
    subtitle = "Twitter status (tweet) counts aggregated by day from Jan 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswga$stripped_text <- gsub("http.*","",  naswga$text)
naswga$stripped_text <- gsub("https.*","", naswga$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswga_tweets_clean <- naswga %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswga_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-GA tweets after 1/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswga_cleaned_tweet_words <- naswga_tweets_clean %>%
  anti_join(stop_words) 

names(naswga_cleaned_tweet_words)

# plot the top 15 words
naswga_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-GA tweets after 1/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswga_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-GA.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASW_PA timeline
naswpa <- get_timeline("NASW_PA", n = 3200)
#1951

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswpa %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-PA",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswpa$stripped_text <- gsub("http.*","",  naswpa$text)
naswpa$stripped_text <- gsub("https.*","", naswpa$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswpa_tweets_clean <- naswpa %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswpa_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-PA tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswpa_cleaned_tweet_words <- naswpa_tweets_clean %>%
  anti_join(stop_words) 

names(naswpa_cleaned_tweet_words)

# plot the top 15 words
naswpa_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-PA tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswpa_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-PA.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWAlabama timeline
naswalabama <- get_timeline("NASWAlabama", n = 3200)
#1385

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswalabama %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-AL",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswalabama$stripped_text <- gsub("http.*","",  naswalabama$text)
naswalabama$stripped_text <- gsub("https.*","", naswalabama$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswalabama_tweets_clean <- naswalabama %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswalabama_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-AL tweets after 1/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswalabama_cleaned_tweet_words <- naswalabama_tweets_clean %>%
  anti_join(stop_words) 

names(naswalabama_cleaned_tweet_words)

# plot the top 15 words
naswalabama_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-AL tweets after 1/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswalabama_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-AL",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################


#NASWIl timeline
naswil <- get_timeline("NASWIl", n = 3200)
#3186

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswil %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-IL",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswil$stripped_text <- gsub("http.*","",  naswil$text)
naswil$stripped_text <- gsub("https.*","", naswil$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswil_tweets_clean <- naswil %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswil_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-IL tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswil_cleaned_tweet_words <- naswil_tweets_clean %>%
  anti_join(stop_words) 

names(naswil_cleaned_tweet_words)

# plot the top 15 words
naswil_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-IL tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswil_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-IL.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWTX timeline
naswtx <- get_timeline("NASWTX", n = 3200)
#1851

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswtx %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-TX",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswtx$stripped_text <- gsub("http.*","", naswtx$text)
naswtx$stripped_text <- gsub("https.*","", naswtx$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswtx_tweets_clean <- naswtx %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswtx_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-TX tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswtx_cleaned_tweet_words <- naswtx_tweets_clean %>%
  anti_join(stop_words) 

names(naswtx_cleaned_tweet_words)

# plot the top 15 words
naswtx_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-TX tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswtx_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-TX.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWMN timeline
naswmn <- get_timeline("NASWMN", n = 3200)
#901

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswmn %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-MN",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswmn$stripped_text <- gsub("http.*","",  naswmn$text)
naswmn$stripped_text <- gsub("https.*","", naswmn$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswmn_tweets_clean <- naswmn %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words 
naswmn_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-MN tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswmn_cleaned_tweet_words <- naswmn_tweets_clean %>%
  anti_join(stop_words) 

names(naswmn_cleaned_tweet_words)

# plot the top 15 words
naswmn_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-MN tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswmn_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-MN",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NASWalaska timeline
naswalaska <- get_timeline("NASWalaska", n = 3200)
#346

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswalaska %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-AK",
    subtitle = "Twitter status (tweet) counts aggregated by day from Jan 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswalaska$stripped_text <- gsub("http.*","",  naswalaska$text)
naswalaska$stripped_text <- gsub("https.*","", naswalaska$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswalaska_tweets_clean <- naswalaska %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswalaska_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-AK tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswalaska_cleaned_tweet_words <- naswalaska_tweets_clean %>%
  anti_join(stop_words) 

names(naswalaska_cleaned_tweet_words)

# plot the top 15 words
naswalaska_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-AK tweets after 1/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswalaska_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-AK",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWCA_Advocacy timeline
naswca <- get_timeline("NASWCA_Advocacy", n = 3200)
#1371

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswca %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-CA",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswca$stripped_text <- gsub("http.*","",  naswca$text)
naswca$stripped_text <- gsub("https.*","", naswca$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswca_tweets_clean <- naswca %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswca_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-CA tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswca_cleaned_tweet_words <- naswca_tweets_clean %>%
  anti_join(stop_words) 

names(naswca_cleaned_tweet_words)

# plot the top 15 words
naswca_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-CA tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswca_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-CA",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWWI timeline
naswwi <- get_timeline("NASWWI", n = 3200)
#699

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswwi %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-WI",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswwi$stripped_text <- gsub("http.*","",  naswwi$text)
naswwi$stripped_text <- gsub("https.*","", naswwi$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswwi_tweets_clean <- naswwi %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswwi_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-WI tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswwi_cleaned_tweet_words <- naswwi_tweets_clean %>%
  anti_join(stop_words) 

names(naswwi_cleaned_tweet_words)

# plot the top 15 words
naswwi_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-WI tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswwi_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-WI",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWOregon timeline
naswor <- get_timeline("NASWOregon", n = 3200)
#679

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswor %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-OR",
    subtitle = "Twitter status (tweet) counts aggregated by day from Jan  1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswor$stripped_text <- gsub("http.*","",  naswor$text)
naswor$stripped_text <- gsub("https.*","", naswor$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswor_tweets_clean <- naswor %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswor_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-OR tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswor_cleaned_tweet_words <- naswor_tweets_clean %>%
  anti_join(stop_words) 

names(naswor_cleaned_tweet_words)

# plot the top 15 words
naswor_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-OR tweets after 1/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswor_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-OR",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#KSsocialworkers timeline
naswks <- get_timeline("KSsocialworkers", n = 3200)
#792

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswks %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-KS",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswks$stripped_text <- gsub("http.*","",  naswks$text)
naswks$stripped_text <- gsub("https.*","", naswks$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswks_tweets_clean <- naswks %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswks_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-KS tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswks_cleaned_tweet_words <- naswks_tweets_clean %>%
  anti_join(stop_words) 

names(naswks_cleaned_tweet_words)

# plot the top 15 words
naswks_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-KS tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswks_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-KS",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASW_IN timeline
naswin <- get_timeline("NASW_IN", n = 3200)
#1148

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswin %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-IL",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswin$stripped_text <- gsub("http.*","",  naswin$text)
naswin$stripped_text <- gsub("https.*","", naswin$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswin_tweets_clean <- naswin %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswin_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-IL tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswin_cleaned_tweet_words <- naswin_tweets_clean %>%
  anti_join(stop_words) 

names(naswin_cleaned_tweet_words)

# plot the top 15 words
naswin_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-IL tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswin_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-IL",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWOhioChapter timeline
naswoh <- get_timeline("NASWOhioChapter", n = 3200)
#799

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswoh %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-OH",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswoh$stripped_text <- gsub("http.*","",  naswoh$text)
naswoh$stripped_text <- gsub("https.*","", naswoh$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswoh_tweets_clean <- naswoh %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswoh_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-OH tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswoh_cleaned_tweet_words <- naswoh_tweets_clean %>%
  anti_join(stop_words) 

names(naswoh_cleaned_tweet_words)

# plot the top 15 words
naswoh_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-OH tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswoh_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-OH",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NASW_NH timeline
naswnh <- get_timeline("NASW_NH", n = 3200)
#332

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswnh %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-NH",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswnh$stripped_text <- gsub("http.*","",  naswnh$text)
naswnh$stripped_text <- gsub("https.*","", naswnh$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswnh_tweets_clean <- naswnh %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswnh_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-NH tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswnh_cleaned_tweet_words <- naswnh_tweets_clean %>%
  anti_join(stop_words) 

names(naswnh_cleaned_tweet_words)

# plot the top 15 words
naswnh_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-NH tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswnh_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-NH",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASW_Arkansas timeline
naswar <- get_timeline("NASW_Arkansas", n = 3200)
#347

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswar %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-AR",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswar$stripped_text <- gsub("http.*","",  naswar$text)
naswar$stripped_text <- gsub("https.*","", naswar$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswar_tweets_clean <- naswar %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswar_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-AR tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswar_cleaned_tweet_words <- naswar_tweets_clean %>%
  anti_join(stop_words) 

names(naswar_cleaned_tweet_words)

# plot the top 15 words
naswar_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-AR tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswar_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-AR",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################


#some people
#director of NASW WV
naswwv <- get_timeline("SamuelHickman", n = 3200)
#3197

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswwv %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by Samuel Hickman(NASW-WV)",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswwv$stripped_text <- gsub("http.*","",  naswwv$text)
naswwv$stripped_text <- gsub("https.*","", naswwv$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswwv_tweets_clean <- naswwv %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswwv_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @SamuelHickman(NASW-WV) tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswwv_cleaned_tweet_words <- naswwv_tweets_clean %>%
  anti_join(stop_words) 

names(naswwv_cleaned_tweet_words)

# plot the top 15 words
naswwv_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @SamuelHickman(NASW-WV) tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswwv_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @SamuelHIckman(NASW-WV)",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


##############################
##############################################
###################################################################

#president elect of NASW
nasw_prez <- get_timeline("Mittjoy", n = 3200)
#3135

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
nasw_prez %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by MIttJoy(NASW-President)",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_prez$stripped_text <- gsub("http.*","",  nasw_prez$text)
nasw_prez$stripped_text <- gsub("https.*","", nasw_prez$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_prez_tweets_clean <- nasw_prez %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_prez_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @MIttJoy(NASW-President) tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_prez_cleaned_tweet_words <- nasw_prez_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_prez_cleaned_tweet_words)

# plot the top 15 words
nasw_prez_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @MittJoy tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_prez_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @MittJoy (NASW-President)",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#other SW orgs 
#NASWFoundation timeline
naswfoundation <- get_timeline("NASWFoundation", n = 3200)
#127

##############################
##############################################
###################################################################

#TheNSWM network for social work management  timeline
nswm <- get_timeline("TheNSWM", n = 3200)
#3198

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
nswm %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NWSM",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nswm$stripped_text <- gsub("http.*","",  nswm$text)
nswm$stripped_text <- gsub("https.*","", nswm$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nswm_tweets_clean <- nswm %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nswm_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NSWM tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nswm_cleaned_tweet_words <- nswm_tweets_clean %>%
  anti_join(stop_words) 

names(nswm_cleaned_tweet_words)

# plot the top 15 words
nswm_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NSWM tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nswm_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NSWm",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################


#ACOSA timeline
acosa <- get_timeline("acosaorg", n = 3200)
#3178

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
acosa %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by ACOSA",
    subtitle = "Twitter status (tweet) counts aggregated by day from Jan 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
acosa$stripped_text <- gsub("http.*","",  acosa$text)
acosa$stripped_text <- gsub("https.*","", acosa$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
acosa_tweets_clean <- acosa %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
acosa_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @ACOSA tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
acosa_cleaned_tweet_words <- acosa_tweets_clean %>%
  anti_join(stop_words) 

names(acosa_cleaned_tweet_words)

# plot the top 15 words
acosa_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @ACOSA tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- acosa_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @ACOSA",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


##############################
##############################################
###################################################################

#The New Social Worker timeline
newsocialwrk <- get_timeline("newsocialworker", n = 3200)
#3198

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

newsocialwrk %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NewSocialWorker",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
newsocialwrk$stripped_text <- gsub("http.*","",  newsocialwrk$text)
newsocialwrk$stripped_text <- gsub("https.*","", newsocialwrk$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
newsocialwrk_tweets_clean <- newsocialwrk %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
newsocialwrk_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NewSocialWorker tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
newsocialwrk_cleaned_tweet_words <- newsocialwrk_tweets_clean %>%
  anti_join(stop_words) 

names(newsocialwrk_cleaned_tweet_words)

# plot the top 15 words
newsocialwrk_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NewSocialWorker tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- newsocialwrk_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NewSocialWork",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#missed some NASW chapter handles  
###########################

#NASW NYState
nasw_nys <- get_timeline("naswnys", n = 3200)
#1205

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_nys %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-NY state",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_nys$stripped_text <- gsub("http.*","",  nasw_nys$text)
nasw_nys$stripped_text <- gsub("https.*","", nasw_nys$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_nys_tweets_clean <- nasw_nys %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_nys_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWNYS tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_nys_cleaned_tweet_words <- nasw_nys_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_nys_cleaned_tweet_words)

# plot the top 15 words
nasw_nys_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWNYS tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_nys_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWNYS",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NABSW
nabsw_tweets <- get_timeline("nabswlive", n = 3200)
#832

head(nabsw_tweets)
#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nabsw_tweets %>%
  #dplyr::filter(created_at > "2019-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NABSW",
    subtitle = "Twitter status (tweet) counts aggregated by day from Jan 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#no tweets in 2019 or 2020, do not include in final data set. 
#######

#@naswvermont - no tweets ever
#@naswne - no tweets since March 2019 
#@rhodeislandnasw - no tweets since September 2018
#@nnaswva - no tweets since April 2019
#@naswsd- no tweets since 2019
#couldnt find handles for NASW Wyoming, North Dakota, South Carolina, New mexico, Tenessee, Louisiana,
#New Orleans, Montana, Iowa, Connecticut, American Samoa, US Virigin Islands

#######

#NASW IDAHO
nasw_idaho <- get_timeline("naswidaho", n = 3200)
#1319

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_idaho %>%
  dplyr::filter(created_at >"2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-Idaho",
    subtitle = "Twitter status (tweet) counts aggregated by day after March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_idaho$stripped_text <- gsub("http.*","",  nasw_idaho$text)
nasw_idaho$stripped_text <- gsub("https.*","", nasw_idaho$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_idaho_tweets_clean <- nasw_idaho %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words 
nasw_idaho_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWIDAHO tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_idaho_cleaned_tweet_words <- nasw_idaho_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_idaho_cleaned_tweet_words)

# plot the top 15 words
nasw_idaho_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWIDAHO tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_idaho_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWIDAHO",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
############################################################

#NASW Utah
nasw_utah <- get_timeline("naswutah", n = 3200)
#172

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_utah %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-Utah",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_utah$stripped_text <- gsub("http.*","",  nasw_utah$text)
nasw_utah$stripped_text <- gsub("https.*","", nasw_utah$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_utah_tweets_clean <- nasw_utah %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words 
nasw_utah_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWUTAH tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_utah_cleaned_tweet_words <- nasw_utah_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_utah_cleaned_tweet_words)

# plot the top 15 words
nasw_utah_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWUTAH tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_utah_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWUTAH",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
############################################################

#NASW Maine
nasw_maine <- get_timeline("naswmaine", n = 3200)
#22

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

head(nasw_maine)
#Last tweet April 13, 2018

##############################
##############################################
############################################################

#NASW AlamoTX
nasw_alamo <- get_timeline("alamosocialwork", n = 3200)
#140

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
head(nasw_alamo)

#no tweets since 12/19/2014

##############################
##############################################
############################################################

#NASW Louisiana
nasw_la <- get_timeline("naswla", n = 3200)
#270

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
head(nasw_la)
#no tweets since 3/2016

##############################
##############################################
############################################################


#NASW Heart Texas branch  
nasw_heart_tx <- get_timeline("naswoftexas", n = 3200)
#105

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

head (nasw_heart_tx)
#no tweets since 11/2017

##############################
##############################################
############################################################

#NASW Puerto Rico 
nasw_pr <- get_timeline("nasw_puertorico", n = 3200)
#1

head(nasw_pr)

##############################
##############################################
############################################################

#NASW Delaware 
nasw_de <- get_timeline("naswde", n = 3200)
#3191

head(nasw_de)
#no new tweets since 11/2019

##############################
##############################################
############################################################

#NASW Rouge Valley (Oregon) 
nasw_rv <- get_timeline("nasw_rv", n = 3200)
#36

head(nasw_rv)
#no new tweets since 05/2015

##############################
##############################################
############################################################


#NASW Nevada 
nasw_nv <- get_timeline("naswnv", n = 3200)
#365

head(nasw_nv)

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_nv %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-Nevada",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_nv$stripped_text <- gsub("http.*","",  nasw_nv$text)
nasw_nv$stripped_text <- gsub("https.*","", nasw_nv$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_nv_tweets_clean <- nasw_nv %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_nv_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWNV tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_nv_cleaned_tweet_words <- nasw_nv_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_nv_cleaned_tweet_words)

# plot the top 15 words
nasw_nv_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWNV tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_nv_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWNV",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
############################################################


#NASW Okalahoma
nasw_ok <- get_timeline("naswok", n = 3200)
#0

#there is also an okalahomanasw


nasw_ok2 <- get_timeline("OklahomaNasw", n = 3200)

head(nasw_ok2)
#no tweets since 11/2018
##############################
##############################################
############################################################

#NASW NYState
nasw_fl <- get_timeline("naswfl", n = 3200)
#887

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_fl %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-FL",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_fl$stripped_text <- gsub("http.*","",  nasw_fl$text)
nasw_fl$stripped_text <- gsub("https.*","", nasw_fl$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_fl_tweets_clean <- nasw_fl %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_fl_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWFL tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_fl_cleaned_tweet_words <- nasw_fl_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_nys_cleaned_tweet_words)

# plot the top 15 words
nasw_fl_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWFL tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_fl_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWFL",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
############################################################

#NASW North Dakota
nasw_nd <- get_timeline("naswnd", n = 3200)
#140

head(nasw_nd)
#last tweet 3/2019

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_nys %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-NY state",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )
##############################
##############################################
############################################################


#NASW NYState
nasw_flne <- get_timeline("naswfl_neunit", n = 3200)
#39

head(nasw_flne)

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_flne %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-Fl NE unit",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_flne$stripped_text <- gsub("http.*","",  nasw_flne$text)
nasw_flne$stripped_text <- gsub("https.*","", nasw_flne$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_flne_tweets_clean <- nasw_flne %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_flne_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWFLne tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_flne_cleaned_tweet_words <- nasw_flne_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_flne_cleaned_tweet_words)

# plot the top 15 words
nasw_flne_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWFlne tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_flne_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWFlne",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
############################################################

#NASW NYState
nasw_caregion <- get_timeline("naswcaregioni", n = 3200)
#38

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

head(nasw_caregion)

nasw_caregion %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-CA region I",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_caregion$stripped_text <- gsub("http.*","",  nasw_caregion$text)
nasw_caregion$stripped_text <- gsub("https.*","", nasw_caregion$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_caregion_tweets_clean <- nasw_caregion %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_caregion_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWCAregionI tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_caregion_cleaned_tweet_words <- nasw_caregion_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_nys_cleaned_tweet_words)

# plot the top 15 words
nasw_caregion_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWCAregionI tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_caregion_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWCAregionI",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
############################################################

#NASW NYState
nasw_ms <- get_timeline("naswms", n = 3200)
#529

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_ms %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-MS",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_ms$stripped_text <- gsub("http.*","",  nasw_ms$text)
nasw_ms$stripped_text <- gsub("https.*","", nasw_ms$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_ms_tweets_clean <- nasw_ms %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_ms_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWMS tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_ms_cleaned_tweet_words <- nasw_ms_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_ms_cleaned_tweet_words)

# plot the top 15 words
nasw_ms_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWMS tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_ms_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWMS",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
############################################################

#NASW NYState
nasw_hi <- get_timeline("naswhawaii", n = 3200)
#871

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_hi %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-HI",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_hi$stripped_text <- gsub("http.*","",  nasw_hi$text)
nasw_hi$stripped_text <- gsub("https.*","", nasw_hi$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_hi_tweets_clean <- nasw_hi %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_hi_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWHAWAII tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_hi_cleaned_tweet_words <- nasw_hi_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_hi_cleaned_tweet_words)

# plot the top 15 words
nasw_hi_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWhawaii tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_hi_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWhawaii",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
############################################################

#NASW NYState
nasw_miami <- get_timeline("naswflmiamidade", n = 3200)
#222

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_miami %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW_miami",
    subtitle = "Twitter status (tweet) counts aggregated by day from Mar 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_miami$stripped_text <- gsub("http.*","",  nasw_miami$text)
nasw_miami$stripped_text <- gsub("https.*","", nasw_miami$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_miami_tweets_clean <- nasw_miami %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_miami_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @naswflmiamidade tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_miami_cleaned_tweet_words <- nasw_miami_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_nys_cleaned_tweet_words)

# plot the top 15 words
nasw_miami_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWFlmiamidade tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_miami_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWflmiamidade",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
############################################################


#NASW NYState
nasw_gainsville <- get_timeline("NASWGainesville", n = 3200)
#30

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_gainsville %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-Gainesville FL",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_gainsville$stripped_text <- gsub("http.*","",  nasw_gainsville$text)
nasw_gainsville$stripped_text <- gsub("https.*","", nasw_gainsville$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_gainsville_tweets_clean <- nasw_gainsville %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_gainsville_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWGainesville tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_gainsville_cleaned_tweet_words <- nasw_gainsville_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_gainsville_cleaned_tweet_words)

# plot the top 15 words
nasw_gainsville_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWGainesville tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_gainsville_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWGainesville",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
############################################################

#NASW Colorado
nasw_co <- get_timeline("naswco", n = 3200)
#760

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_co %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-CO",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_co$stripped_text <- gsub("http.*","",  nasw_co$text)
nasw_co$stripped_text <- gsub("https.*","", nasw_co$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_co_tweets_clean <- nasw_co %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_co_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWCO tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_co_cleaned_tweet_words <- nasw_co_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_co_cleaned_tweet_words)

# plot the top 15 words
nasw_co_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWCO tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_co_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWCO",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
############################################################

#NASW NYState
nasw_nys <- get_timeline("naswnys", n = 3200)
#1205

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

nasw_nys %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-NY state",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_nys$stripped_text <- gsub("http.*","",  nasw_nys$text)
nasw_nys$stripped_text <- gsub("https.*","", nasw_nys$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_nys_tweets_clean <- nasw_nys %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_nys_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWNYS tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_nys_cleaned_tweet_words <- nasw_nys_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_nys_cleaned_tweet_words)

# plot the top 15 words
nasw_nys_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWNYS tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_nys_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWNYS",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
############################################################

swcovid2_tweets <- search_tweets(q = "swcovid19",n = 500000,type = "recent",include_rts = TRUE, parse = TRUE,token = my_token,
                                retryonratelimit = TRUE,verbose = TRUE)

swcovid2_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWcovid19 Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## create lat/lng variables using all available tweet and profile geo-location data
covid2_latlong <- lat_lng(swcovid2_tweets)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(covid2_latlong, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

# how many locations are represented
length(unique(swcovid2_tweets$location))
#133

# install descriptive stats libraries
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)

#where are users from 
swcovid2_tweets %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(15) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users during #SWcovid19- unique locations ")

names(swcovid2_tweets)

# remove http elements manually
swcovid2_tweets$stripped_text <- gsub("http.*","",  swcovid2_tweets$text)
swcovid2_tweets$stripped_text <- gsub("https.*","", swcovid2_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swcovid2_tweets_tweets_clean <- swcovid2_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
swcovid2_tweets_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #SWcovid19 tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
swcovid2_cleaned_tweet_words <- swcovid2_tweets_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
swcovid2_cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets using #SWcovid19",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swcovid2_tweets_tweets_paired_words <- swcovid2_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swcovid2_tweets_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

library(tidyr)
swcovid2_tweets_tweets_separated_words <- swcovid2_tweets_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swcovid2_tweets_filtered <- swcovid2_tweets_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swcovid2_words_counts <- swcovid2_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swcovid2_words_counts)

# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swcovid2_words_counts %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - SWcovid19",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swcovid2_tweets_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #SWcovid19",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#merge all nasw sets

nasw_all <- bind_rows(nasw, nasw_prez, naswalabama, naswalaska, naswar, naswaz, naswca, 
          naswga, naswil, naswin, naswkentucky, naswks, naswma, naswmi, naswmn, naswnc, 
          naswnh, naswnj, naswnyc, naswoh, naswor, naswpa, naswtx, naswwi,naswwv, nasw_nys,
          nasw_idaho, nasw_utah, nasw_nv, nasw_fl, nasw_flne, nasw_caregion, nasw_ms, nasw_hi, 
          nasw_miami, nasw_gainsville, nasw_co)

sw_orgs <- bind_rows(acosa, cswe, ifsw, newsocialwrk, nswm, sswr)

sw_tags <- bind_rows(swgens_tweets, swmonth_tweets, swmonth2_tweets, swtech_tweets, swcovid_tweets, swcovid2_tweets)

#subtset to after Jan 21st, first confimed COVID-19 case in US
#or December 31st, 2019 when news first broke in China? 
#timeline source: https://www.nbcnews.com/health/health-news/coronavirus-timeline-tracking-critical-moments-covid-19-n1154341

#subset 

nasw_final <- nasw_all %>% dplyr::filter(created_at > "2019-12-15")

orgs_final <- sw_orgs %>% dplyr::filter(created_at > "2019-12-15")

tags_final <- sw_tags %>% dplyr::filter(created_at > "2019-12-15")

#merge to one set
swcovid <- bind_rows (nasw_final, orgs_final, tags_final)


#plot all tweets over time in the final dataset

swcovid %>%
  ts_plot("1 day") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of all Twitter statuses in final dataset (n=7338)",
    subtitle = "Twitter status (tweet) counts aggregated using tweleve-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#conduct stm 

library(stm)
library(stminsights)

#remove emoji characters
swcovid_clean <- swcovid
# swcovid_rts <- swcovid
swcovid_clean$text <- sapply(swcovid_clean$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
# swcovid_rts$text <- sapply(swcovid_rts$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#remove retweets
swcovid_clean <- swcovid_clean %>% dplyr::filter(is_retweet == "FALSE")
#5060 tweets total since 12/15/19

swcovid_clean %>%
  ts_plot("days") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Number of Twitter statuses from 12/15/19 - 04/20/20",
    subtitle = "Twitter status (tweet) counts aggregated by day",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# swcovid_rts %>%
#   ts_plot("days") +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
#   ggplot2::labs(
#     x = NULL, y = NULL,
#     title = "Number of Twitter statuses from 12/15/19 - 04/20/20",
#     subtitle = "Twitter status (tweet) counts aggregated by day",
#     caption = "\nSource: Data collected from Twitter's REST API via rtweet"
#   )

processed <- textProcessor(swcovid_clean$text, metadata = swcovid_clean)
# processed2 <- textProcessor(swcovid_rts$text, metadata = swcovid_rts)

#Prepare

plotRemoved(processed$documents, lower.thresh = seq(1,100, by = 10))
# plotRemoved(processed2$documents, lower.thresh = seq(1,100, by = 10))


out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)
# Removing 12479 of 13635 terms (22204 of 81379 tokens) due to frequency 
# Removing 92 Documents with No Words 
# Your corpus now has 4968 documents, 1156 terms and 59175 tokens.

#out2 <- prepDocuments(processed2$documents, processed2$vocab, processed2$meta, lower.thresh = 10)
# Removing 13429 of 14906 terms (25499 of 107813 tokens) due to frequency 
# Removing 65 Documents with No Words 
# Your corpus now has 6551 documents, 1477 terms and 82314 tokens


docs<- out$documents
vocab<- out$vocab
meta <- out$meta

# docs2<- out2$documents
# vocab2<- out2$vocab
# meta2 <- out2$meta

#inspect to make sure preprocessing went ok 
head(docs)#this tells you how many words are in what position 
head(vocab)
head(meta)

#NOTE: still have stripped_text

#Estimate
swcovid_noK <- stm(documents = out$documents, vocab = out$vocab, K= 0, 
                              data = out$meta, init.type = "Spectral")
#50 topics

# swcovid_noK_rts <- stm(documents = out2$documents, vocab = out2$vocab, K= 0, 
#                    data = out2$meta, init.type = "Spectral")
# #50 topics 

labelTopics (swcovid_noK)
plot.STM(swcovid_noK,type="summary", xlim=c(0, .3))
#top 20
plot.STM(swcovid_noK,type="summary", xlim=c(0, .3), ylim=c(30,50))

# labelTopics (swcovid_noK_rts)
# plot.STM(swcovid_noK_rts,type="summary", xlim=c(0, .3))
# plot.STM(swcovid_noK_rts,type="summary", xlim=c(0, .3), ylim=c(30,50))

names(swcovid_clean)
table(swcovid_clean$is_retweet)

#Evaluate
storage <- searchK(out$documents, out$vocab, K = c(20,30,40,50,60,70,80,90,100), data = meta) 
# storage2 <- searchK(out2$documents, out2$vocab, K = c(20,30,40,50,60,70,80,90,100), data = meta) 

plot(storage)
# plot(storage2)

#lowest held out likelihood 
#lowest residual
#highest semantic coherence 
#highest lower bound

#60 seems reaonable 

library (Rtsne)
library(rsvd)
library(geometry)


#to get stm to work we need an effect variable 
library(stm)
swcovid_60K_day <- stm(documents = out$documents, vocab = out$vocab, K= 60, 
                           prevalence = ~ s(created_at), data = out$meta, init.type = "Spectral")

# swcovid_noK_rts_day <- stm(documents = out2$documents, vocab = out2$vocab, K= 0, 
#                            prevalence = ~ s(created_at), data = out2$meta, init.type = "Spectral")

labelTopics (swcovid_60K_day)
# labelTopics (swcovid_noK_rts_day, n= 10)#43
#top 20
plot.STM(swcovid_60K_day,type="summary", xlim=c(0, .3), ylim=c(40,60))

sageLabels(swcovid_60K_day, n = 7)

#Calculate topic correlations 
swcovid_60Kday_corrs <- topicCorr(swcovid_60K_day, method = "simple", cutoff = 0.01, verbose = TRUE)
# swcovid_noK_corrs <- topicCorr(swcovid_noK, method = "simple", cutoff = 0.01, verbose = TRUE)

library(igraph)
plot (swcovid_60Kday_corrs,  cex = 2.0)
# plot (swcovid_noK_corrs, cex = 2.0)

#Estimate the relationship between metadata and topics/topical content 
#effect of #whyILeft and #whyistayed hashtags
#number of simulations default = 25 

#noK
swcovid_60kday_prep <- estimateEffect(1:48 ~ s(created_at), swcovid_60K_day, meta = meta, uncertainty = "Global")
summary(swcovid_60kday_prep)

#noK_RTs
# swcovid_noK_rts_prep <- estimateEffect(1:43 ~ s(created_at), swcovid_noK_rts_day, meta = meta2, uncertainty = "Global")
# summary(swcovid_noK_rts_prep)

#use stminsight to look for covid19 topics...
require(devtools)
devtools::install_github("cschwem2er/stminsights")
devtools::install_github("rstudio/shiny")
#60k_correlations

#convert to dataframe and export to csv 
# k65_codes <- make.dt(whyistayedFit_k65)
# write.csv(k65_codes, "topicModelCodesK65.csv")

#model excluding retweets 
#swcovid_noK_rts_day
#top topics 03 32 41 34 39 33

par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))

library(wordcloud)
noRts_topic03 <- findThoughts(swcovid_noK_rts_day, texts=meta2$text, topics=03, n=2)
noRts_topic03
plotQuote(noRts_topic03$docs[[1]], main = "Topic 03")
cloud(swcovid_noK_day, topic = 03, scale = c(2, .25))

noRts_topic32 <- findThoughts(swcovid_noK_rts_day, texts=meta2$text, topics=32, n=2)
noRts_topic32
plotQuote(noRts_topic32$docs[[1]], main = "Topic 32")
cloud(swcovid_noK_day, topic = 32, scale = c(2, .25))

noRts_topic41 <- findThoughts(swcovid_noK_rts_day, texts=meta2$text, topics=41, n=2)
noRts_topic41
plotQuote(noRts_topic41$docs[[1]], main = "Topic 41")
cloud(swcovid_noK_day, topic = 41, scale = c(2, .25))

noRts_topic34 <- findThoughts(swcovid_noK_rts_day, texts=meta2$text, topics=34, n=2)
noRts_topic34
plotQuote(noRts_topic34$docs[[1]], main = "Topic 34")
cloud(swcovid_noK_day, topic = 34, scale = c(2, .25))

noRts_topic39 <- findThoughts(swcovid_noK_rts_day, texts=meta2$text, topics=39, n=2)
noRts_topic39
plotQuote(noRts_topic39$docs[[1]], main = "Topic 39")
cloud(swcovid_noK_day, topic = 39, scale = c(2, .25))

noRts_topic33 <- findThoughts(swcovid_noK_rts_day, texts=meta2$text, topics=33, n=2)
noRts_topic33
plotQuote(noRts_topic33$docs[[1]], main = "Topic 33")
cloud(swcovid_noK_day, topic = 33, scale = c(2, .25))

##############
#update hashtag
#swcovid19

updated_swcovid_tweets <- search_tweets(q = "SWcovid19",n = 500000,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
                                retryonratelimit = TRUE,verbose = TRUE)

updated_swthrucovid_tweets <- search_tweets(q = "swthrucovid",n = 500000,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
                                        retryonratelimit = TRUE,verbose = TRUE)

install.packages("devtools")
devtools::install_github("hadley/emo")
library(emo)
swcovid %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(10)

#top emojis
Selecting by n
# # A tibble: 11 x 2
# emoji     n
# <chr> <int>
# 1 ❤️       47
# 2 ✌🏽       32
# 3 💚       32
# 4 ✌️       31
# 5 👇       31
# 6 🚨       17
# 7 ✨       13
# 8 😍       11
# 9 🎉        9
# 10 👍       9
# 11 😲       9

#locations
swcovid %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(5)

# Selecting by n
# # A tibble: 6 x 2
# place_full_name     n
# <chr>           <int>
#   1 Washington, USA   180
# 2 Honolulu, HI       20
# 3 Washington, DC     12
# 4 Los Angeles, CA     8
# 5 Abingdon, MD        5
# 6 Bel Air, MD         5

#most retweeted
swcovid %>% 
  arrange(-retweet_count) %>%
  slice(1) %>% 
  select(created_at, screen_name, text, retweet_count, status_id)

install.packages("tweetrmd")
library(tweetrmd)

tweet_screenshot(tweet_url("naswidaho", "1248670927665025024"))

#top hastags
swcovid_clean %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"),
         hashtag != c("#SocialWork","#Socialworkers")) %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder (hashtag,n), y = n, fill = hashtag)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Hashtag",
       y = "Count",
       title = "Top 10 hastags", 
       subtitle = "Dec 15th,2019 through April 20th, 2020", 
       caption = "Collected via Twitter's REST API using rtweet")

# 
# Selecting by n
# # A tibble: 10 x 2
# hashtag                  n
# <chr>                <int>
#   1 #SocialWorkMonth      1178
# 2 #NASW                  927
# 3 #SWcovid19             864
# 4 #socialwork            684
# 5 #SWMonth               509
# 6 #MacroSW               473
# 7 #SWGenerationsStrong   468
# 8 #socialworkers         444
# 9 #SocialWorkStrong      339
# 10 #SocialWork            262

#top tweeters
swcovid_clean %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name)) %>%
  ggplot(aes(x = reorder(screen_name,n), y = n, fill = screen_name)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Twitter Handle",
       y = "Count",
       title = "Top Tweeters", 
       subtitle = "Dec 15th,2019 through April 20th, 2020", 
       caption = "Collected via Twitter's REST API using rtweet ")

# Selecting by n
# # A tibble: 10 x 2
# screen_name          n
# <chr>            <int>
# 1 @nasw              699
# 2 @newsocialworker   625
# 3 @Samuelhickman     525
# 4 @NASWNYC           278
# 5 @NASWCO            234
# 6 @lauranissen       220
# 7 @Mittjoy           212
# 8 @IFSW              197
# 9 @NASWMA            192
# 10 @NASWTX            161

#top words
words <- swcovid %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),         
         !str_detect(word, "@\\S+")) %>%
  count(word, sort = TRUE)

library(wordcloud) 
words %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 100))

#top mentions
swcovid %>% 
  unnest_tokens(mentions, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions, "^@")) %>%  
  count(mentions, sort = TRUE) %>%
  top_n(10)

# Selecting by n
# # A tibble: 10 x 2
# mentions             n
# <chr>            <int>
# 1 @nasw              505
# 2 @socworkpodcast    122
# 3 @DrGriseOwens      115
# 4 @CSocialWorkEd     110
# 5 @spcummings        102
# 6 @karenzgoda        100
# 7 @newsocialworker    98
# 8 @melaniesage        96
# 9 @TravelMSW          89
# 10 @IFSW               88

#filter tweets those including covid19 in hastag
#exemplars <- dplyr::filter(swcovid_clean, grepl('covid|coronoa', text))
exemplars <- dplyr::filter(swcovid_clean, grepl('covid19|corona|coronavirus|covid-19', text))

table(exemplars$screen_name)

#top tweeters
exemplars %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(20) %>%
  mutate(screen_name = paste0("@", screen_name)) %>%
  ggplot(aes(x = reorder(screen_name, n), y = n, fill = screen_name)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Twitter Handle",
       y = "Count",
       title = "Top 20 handles using 'corona, covid19, or coronavirus' in tweets", 
       subtitle = "Dec 15th,2019 through April 20th, 2020", 
       caption = "Collected via Twitter's REST API using rtweet")

#reorder bar graph 
#aes(x=reorder(TOPIC, -count),y=count) or aes(x=reorder(TOPIC, count),y=count).

nasw_covid_usage <- filter(exemplars, screen_name == "nasw")
table(nasw_covid_usage$text)

naswco_covid_usage <- filter(exemplars, screen_name == "NASWCO")
table(naswco_covid_usage$text)

naswhawaii_covid_usage <- filter(exemplars, screen_name == "NASWHawaii")
table(naswhawaii_covid_usage$text)

naswnyc_covid_usage <- filter(exemplars, screen_name == "NASWNYC")
table(naswnyc_covid_usage$text)

naswnys_covid_usage <- filter(exemplars, screen_name == "NASWNYS")
table(naswnys_covid_usage$text)

newswers_covid_usage <- filter(exemplars, screen_name == "newsocialworker")
table(newswers_covid_usage$text)

macrosw_covid_usage <- filter(exemplars, screen_name == "OfficialMacroSW")
table(macrosw_covid_usage$text)

nasw_covid_usage <- filter(exemplars, screen_name == "nasw")
table(nasw_covid_usage$text)

#top 10 hashtgas within exemplars
exemplars %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"),
         hashtag != c("#SocialWork","#Socialworkers")) %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(hashtag,n), y = n, fill = hashtag)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Hashtag",
       y = "Count",
       title = "Top 10 hastags", 
       subtitle = "Dec 15th,2019 through April 20th, 2020", 
       caption = "Collected via Twitter's REST API using rtweet")

#aes(x=reorder(TOPIC, -count),y=count) or aes(x=reorder(TOPIC, count),y=count).

filter(exemplars, screen_name == "nasw")  




