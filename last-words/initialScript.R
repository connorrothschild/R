library(dplyr)
library(purrr)
library(twitteR)
library(tidyr)
library(lubridate)
library(scales)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(readr)
library(sentimentr)
library(dplyr)
library(syuzhet)
library(broom)
library(cleanNLP)
library(textclean)
library(ggrepel)
library(topicmodels)
library(tm)
library(stm)
library(quanteda)

tx_deathrow_full <- read_csv("tx_deathrow_full.csv")

# remove weird row 
data <- tx_deathrow_full[-c(1),]
data$executionDate <- as.Date(data$executionDate, format="%m/%d/%y")

data_w_sentiment <- data %>%
  sentimentr::get_sentences(data$lastwords) %>% 
  sentimentr::sentiment(data=lastwords,polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                        valence_shifters_dt = lexicon::hash_valence_shifters) %>% 
  group_by(element_id) %>% 
  mutate(lastwords = add_comma_space(lastwords)) %>% 
  mutate(lastwordsfull = paste(lastwords, collapse=" ")) %>% 
  ungroup() %>% 
  mutate(lastwords = replace_white(lastwords)) %>% 
  mutate(lastwords = replace_white(lastwords)) %>% 
  group_by(lastwords) %>% 
  mutate(lastwordssentiment = mean(sentiment)) %>% 
  mutate(wordcount = sum(word_count)) %>% 
  ungroup() %>% 
  mutate(sentimentbinary = ifelse(sentiment>0,"Positive", 
                                  ifelse(sentiment<0,"Negative","Neutral"))) %>% 
  mutate(lastwordssentimentbinary = ifelse(lastwordssentiment>0,"Positive",
                                       ifelse(lastwordssentiment<0,"Negative","Neutral"))) %>% 
  mutate(distancefromzero = abs(sentiment)) %>% 
  mutate(lastwordsdistance = abs(lastwordssentiment)) %>% 
  filter(!is.na(word_count)) %>% 
  filter(lastwords!=" ") %>% 
  select(everything())

data1 <- data_w_sentiment %>% 
  distinct(id, .keep_all=TRUE) %>% 
  select(-c(sentiment,distancefromzero,sentimentbinary,element_id,sentence_id,word_count,lastwords))

data1$executionDate <- as.Date(data1$executionDate, format="%m/%d/%y")
## sentiment over time

data1 %>% 
  mutate(year=year(executionDate)) %>% 
  group_by(year) %>% 
  summarise(sentiment=mean(lastwordssentiment)) %>% 
  ggplot(aes(x=year,y=sentiment)) +
         geom_point() +
  geom_line() +
  geom_hline(yintercept=0,linetype=2)

data1 %>% 
  mutate(month=month(executionDate)) %>% 
  group_by(month) %>% 
  summarise(sentiment=mean(lastwordssentiment)) %>% 
  ggplot(aes(x=month,y=sentiment)) +
  geom_point() +
  geom_line()

# same but distance from zero (emotionality)

data1 %>% 
  mutate(year=year(executionDate)) %>% 
  group_by(year) %>% 
  summarise(sentiment=mean(lastwordsdistance)) %>% 
  ggplot(aes(x=year,y=sentiment)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0,linetype=2) +
  ylim(c(0,1))

data1 %>% 
  mutate(month=month(executionDate)) %>% 
  group_by(month) %>% 
  summarise(sentiment=mean(lastwordsdistance)) %>% 
  ggplot(aes(x=month,y=sentiment)) +
  geom_point() +
  geom_line() +
  ylim(c(0,1))

# common word categories
data1 <- data1 %>% 
  mutate(sad = str_detect(lastwordsfull, "Sorry|sorry|forgiveness|forgive|Forgive|apologize")) %>% 
  mutate(love = str_detect(lastwordsfull, "love|Love|Loving|loving")) %>% 
  mutate(religious = str_detect(lastwordsfull, "God|heaven|Heaven|hell|Hell|Jesus|Lord")) %>% 
  mutate(innocent = str_detect(lastwordsfull, "innocent|innocence"))

summary(data1)

## word categories
wordchoice <- data1 %>% 
  select(sad,love,religious,innocent) %>% 
  summarise(sadcount = sum(ifelse(sad==TRUE,1,0)),
            lovecount = sum(ifelse(love==TRUE,1,0)),
            religiouscount = sum(ifelse(religious==TRUE,1,0)),
            innocentcount = sum(ifelse(innocent==TRUE,1,0))) %>% 
  gather() %>% 
  rename("word" = key,
         "count" = value)

wordchoice %>% ggplot(aes(x=word, y=count)) +
  geom_col()

# over time, as percent of overall words
data1 %>% mutate(sadcount = ifelse(sad==TRUE,1,0),
                    lovecount = ifelse(love==TRUE,1,0),
                    religiouscount = ifelse(religious==TRUE,1,0),
                    innocentcount = ifelse(innocent==TRUE,1,0)) %>%
  group_by(year(executionDate)) %>% 
             summarise(sad = mean(sadcount),
                       love = mean(lovecount),
                       religious = mean(religiouscount),
                       innocent = mean(innocentcount)) %>% 
  gather("word", "mean", 2:5) %>% 
             ggplot(aes(x=`year(executionDate)`,y=mean, group=word, colour=word)) +
  geom_line(aes(x=`year(executionDate)`,y=mean)) 

### split up words
words <- data1 %>% 
  unnest_tokens(word, lastwordsfull, drop = FALSE) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

# merge Tweet words w/ NRC sentiment
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment)
colnames(nrc)[colnames(nrc)=="sentiment"] <- "wordsentiment"

words_w_nrc <- words %>%
  inner_join(nrc, by = "word")

# common sentiments
words_w_nrc %>% 
  group_by(wordsentiment) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=reorder(wordsentiment,n), y=n, fill=wordsentiment)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = 'none')

words_w_nrc %>% 
  group_by(year(executionDate), wordsentiment) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=`year(executionDate)`, y=n, color=wordsentiment)) +
  geom_line() 

## there is a peak in 2000 -- because executions peaked around then?

data1 %>% 
  group_by(year(executionDate)) %>% 
  ggplot(aes(x=`year(executionDate)`)) +
  geom_bar(stat="count")
# yeah

# so, redo that plot but do percent instead of N

words_w_nrc %>% 
  group_by(year(executionDate), wordsentiment) %>% 
  summarise(n=n()) %>% 
  mutate(percent = n/sum(n)) %>% 
  ggplot(aes(x=`year(executionDate)`, y=percent, color=wordsentiment)) +
  geom_line() 

# avg age at execution
data %>% 
  group_by(year(executionDate)) %>% 
  summarise(meanage = mean(ageAtExecution)) %>% 
  ggplot(aes(x=`year(executionDate)`, y=meanage)) +
  geom_point() +
  geom_line()

# heatmap by month and year
data %>% 
  mutate(year = year(executionDate),
         month = month(executionDate)) %>% 
  group_by(month, year) %>% 
  summarise(monthly=n(),
            yearly=sum(monthly)) %>% 
  ggplot(aes(x=year, y=month, fill = monthly)) +
    geom_tile()
    
library(gganimate)

# most popular words over time
words_by_time <- words %>%
  mutate(time_floor = year(executionDate)) %>%
  count(time_floor, word) %>%
  group_by(time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  mutate(percent_of_time = count/time_total) %>% 
  filter(word_total > 30,
         time_total > 10)

words_by_time %>% View()
# make male rank variable
time_w_rank <- words_by_time %>%
  group_by(time_floor) %>%
  mutate(rank = rank(-percent_of_time, ties.method = "random") * 1) %>%
  filter(rank <= 5,
         sum(rank) > 11) %>%
  ungroup()

time_w_rank <- time_w_rank[1:15, ]
# plot male animation
library(gganimate)
animation <- time_w_rank %>% 
  ggplot(aes(rank, group = word, 
             fill = as.factor(word), color = as.factor(word))) +
  geom_tile(aes(y = percent_of_time/2,
                height = percent_of_time,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(word, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title="Most Popular Biblical Baby Names for Males", 
       subtitle='in {closest_state}', x = element_blank(), y = "Percent of Names",
       caption = "Source: U.S. Social Security Administration\n Design: www.connorrothschild.com") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        plot.subtitle = element_text(hjust = 0, size = 16),
        axis.ticks.y = element_blank(), 
        axis.text.y  = element_blank(), 
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(time_floor, transition_length = 4, state_length = 0) +
  ease_aes('cubic-in-out') +
  enter_drift(x_mod = -1) + exit_drift(x_mod = 1)

animate(animation, width = 800, height = 600)
  