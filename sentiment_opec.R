# ==============================================================================================================
# Purpose:      Sentiment Analysis of OPEC Speeches
# Author:       Kyven Tan
# DOC:          16-Apr-2019
# Topics:       Sentiment Analysis
# Data Source:  OPEC Speeches 2007, 2008, 2009, 2010, 2014, 2015, 2016
# Packages:     dplyr, tidytext, tidyr, reshape2, wordcloud, ggplot2
# Notes:
# For dply useful functions, see http://genomicsclass.github.io/book/pages/dplyr_tutorial.html
#================================================================================================================

library(dplyr)
library(tidytext)
library(tidyr)
data(stop_words)

setwd('/Users/tankyven/Desktop/BC3409/Assignment 2/OPEC Speeches/')

# Read txt files in
opec2007 <- readLines(con <-file("opec_2007.txt"))
opec2008 <- readLines(con <-file("opec_2008.txt"))
opec2009 <- readLines(con <-file("opec_2009.txt"))
opec2010 <- readLines(con <-file("opec_2010.txt"))
opec2014 <- readLines(con <-file("opec_2014.txt"))
opec2015 <- readLines(con <-file("opec_2015.txt"))
opec2016 <- readLines(con <-file("opec_2016.txt"))


# Pre-process speeches
opec2007 <- data_frame(text = opec2007)
opec2007.t <- opec2007 %>% unnest_tokens(word, text)
opec2007.t <- opec2007.t %>% anti_join(stop_words)

opec2008 <- data_frame(text = opec2008)
opec2008.t <- opec2008 %>% unnest_tokens(word, text)
opec2008.t <- opec2008.t %>% anti_join(stop_words)

opec2009 <- data_frame(text = opec2009)
opec2009.t <- opec2009 %>% unnest_tokens(word, text)
opec2009.t <- opec2009.t %>% anti_join(stop_words)

opec2010 <- data_frame(text = opec2010)
opec2010.t <- opec2010 %>% unnest_tokens(word, text)
opec2010.t <- opec2010.t %>% anti_join(stop_words)

opec2014 <- data_frame(text = opec2014)
opec2014.t <- opec2014 %>% unnest_tokens(word, text)
opec2014.t <- opec2014.t %>% anti_join(stop_words)

opec2015 <- data_frame(text = opec2015)
opec2015.t <- opec2015 %>% unnest_tokens(word, text)
opec2015.t <- opec2015.t %>% anti_join(stop_words)

opec2016 <- data_frame(text = opec2016)
opec2016.t <- opec2016 %>% unnest_tokens(word, text)
opec2016.t <- opec2016.t %>% anti_join(stop_words)

sentiments # 3 sentiment lexicons avail in package tidytext

# 3 Sources of sentiments in R Script: nrc, afinn, bing
get_sentiments("nrc")
## positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

get_sentiments("afinn")
## Scoring from - 5 to +5 for each word

# bing is a famous text mining professor
get_sentiments("bing")
## Neg or Pos for each word

# Most common words associated with Joy ------------------------------------------------------------------------
## inner_join: AnB/ intersection
nrcjoy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
o2007.joy <- opec2007.t %>% inner_join(nrcjoy) %>% count(word, sort = T)
o2008.joy <- opec2008.t %>% inner_join(nrcjoy) %>% count(word, sort = T)
o2009.joy <- opec2009.t %>% inner_join(nrcjoy) %>% count(word, sort = T)
o2010.joy <- opec2010.t %>% inner_join(nrcjoy) %>% count(word, sort = T)
o2014.joy <- opec2014.t %>% inner_join(nrcjoy) %>% count(word, sort = T)
o2015.joy <- opec2015.t %>% inner_join(nrcjoy) %>% count(word, sort = T)
o2016.joy <- opec2016.t %>% inner_join(nrcjoy) %>% count(word, sort = T)

# Most common positive and negative words by Bing lexicon ------------------------------------------------------
# Spread() in package tidyr to list Neg and Pos sentiments in another columns
# Mutate() function in dplyer that creates a new column
o2007.sen <- opec2007.t %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative) %>% arrange(desc(sentiment))

o2008.sen <- opec2008.t %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative) %>% arrange(desc(sentiment))

o2009.sen <- opec2009.t %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative) %>% arrange(desc(sentiment))

o2010.sen <- opec2010.t %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative) %>% arrange(desc(sentiment))

o2014.sen <- opec2014.t %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative) %>% arrange(desc(sentiment))

o2015.sen <- opec2015.t %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative) %>% arrange(desc(sentiment))

o2016.sen <- opec2016.t %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative) %>% arrange(desc(sentiment))

# Sentiment by Afinn score -5 to 5 per word -----------------------------------------------------------------------
o2007.sen.af <- opec2007.t %>% inner_join(get_sentiments("afinn")) %>% count(word, score) %>% arrange(desc(score))
o2008.sen.af <- opec2008.t %>% inner_join(get_sentiments("afinn")) %>% count(word, score) %>% arrange(desc(score))
o2009.sen.af <- opec2009.t %>% inner_join(get_sentiments("afinn")) %>% count(word, score) %>% arrange(desc(score))
o2010.sen.af <- opec2010.t %>% inner_join(get_sentiments("afinn")) %>% count(word, score) %>% arrange(desc(score))
o2014.sen.af <- opec2014.t %>% inner_join(get_sentiments("afinn")) %>% count(word, score) %>% arrange(desc(score))
o2015.sen.af <- opec2015.t %>% inner_join(get_sentiments("afinn")) %>% count(word, score) %>% arrange(desc(score))
o2016.sen.af <- opec2016.t %>% inner_join(get_sentiments("afinn")) %>% count(word, score) %>% arrange(desc(score))

# Overall Sentiment Across the 3 Rallys ----------------------------------------------------------------------
overall <- data.frame(Sentiment.Bing = c(0,0,0,0,0,0,0), Sentiment.Afinn = c(0,0,0,0,0,0,0), 
                      row.names = c("Opec2007", "Opec2008", "Opec2009", "Opec2010", "Opec2014", "Opec2015", "Opec2016"))

overall$Sentiment.Bing[1] <- o2007.sen %>% summarise(sum(sentiment))
overall$Sentiment.Bing[2] <- o2008.sen %>% summarise(sum(sentiment))
overall$Sentiment.Bing[3] <- o2009.sen %>% summarise(sum(sentiment))
overall$Sentiment.Bing[4] <- o2010.sen %>% summarise(sum(sentiment))
overall$Sentiment.Bing[5] <- o2014.sen %>% summarise(sum(sentiment))
overall$Sentiment.Bing[6] <- o2015.sen %>% summarise(sum(sentiment))
overall$Sentiment.Bing[7] <- o2016.sen %>% summarise(sum(sentiment))

overall$Sentiment.Afinn[1] <- o2007.sen.af %>% summarise(sum(score))
overall$Sentiment.Afinn[2] <- o2008.sen.af %>% summarise(sum(score))
overall$Sentiment.Afinn[3] <- o2009.sen.af %>% summarise(sum(score))
overall$Sentiment.Afinn[4] <- o2010.sen.af %>% summarise(sum(score))
overall$Sentiment.Afinn[5] <- o2014.sen.af %>% summarise(sum(score))
overall$Sentiment.Afinn[6] <- o2015.sen.af %>% summarise(sum(score))
overall$Sentiment.Afinn[7] <- o2016.sen.af %>% summarise(sum(score))
overall

## Shows a dip in positive sentiment in 2009-2010 (Post 2008-2009 Recession)
## Shows a spike in 2016

# Wordcloud of most common positive and negative words --------------------------------------------------------
library(reshape2)
library(wordcloud)

opec2016.t %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 300)