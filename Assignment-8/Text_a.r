library(readr)
library(stringr)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

tweets <- read.csv("../input/tweets.csv")

#--- get unique words for Trump and Clinton with their correconding frequencies

get_words_with_freq <- function(tweet.text) {
  # separate the words (&amp; and &gt; appear quite a lot, especially in Trump's tweets)
  elements <- str_split(tweet.text, "(&amp;|&gt;|\\s)")[[1]]
  # remove urls
  elements <- grep("^http", elements, value = TRUE, invert = TRUE)
  # remove all punctuation except ', # and @ (because we're on twitter)
  tweet.words <- elements %>% gsub(pattern = "[^[:alnum:][:space:]'#@]",
                                   replacement = "")
  # strip trailing white spaces if any
  tweet.words <- str_trim(tweet.words)
  # remove empty strings
  tweet.words <- tweet.words[-which(tweet.words == "")]
  # remove stopwords (list of stopwords was obtained from http://www.ranks.nl/stopwords)
  stopwords.regex <- "^(a|about|above|after|again|against|all|am|an|and|any|are|aren't|as|at|be|because|been|before|being|below|between|both|but|by|can't|cannot|could|couldn't|did|didn't|do|does|doesn't|doing|don't|down|during|each|few|for|from|further|had|hadn't|has|hasn't|have|haven't|having|he|he'd|he'll|he's|her|here|here's|hers|herself|him|himself|his|how|how's|i|i'd|i'll|i'm|i've|if|in|into|is|isn't|it|it's|its|itself|let's|me|more|most|mustn't|my|myself|no|nor|not|of|off|on|once|only|or|other|ought|our|ours|ourselves|out|over|own|same|shan't|she|she'd|she'll|she's|should|shouldn't|so|some|such|than|that|that's|the|their|theirs|them|themselves|then|there|there's|these|they|they'd|they'll|they're|they've|this|those|through|to|too|under|until|up|very|was|wasn't|we|we'd|we'll|we're|we've|were|weren't|what|what's|when|when's|where|where's|which|while|who|who's|whom|why|why's|with|won't|would|wouldn't|you|you'd|you'll|you're|you've|your|yours|yourself|yourselves)$"
  tweet.words <- grep(stopwords.regex, tweet.words, value = TRUE,
                      ignore.case = TRUE, invert = TRUE)
  # get unique words with frequencies
  uniq.tweet.words <- tbl_df(table(tweet.words)) %>% arrange(desc(n))
  return(uniq.tweet.words)
}

trump <- filter(tweets, handle == "realDonaldTrump")
trump.text <- paste(trump$text, collapse = " ")
trump.words <- get_words_with_freq(trump.text)
clinton <- filter(tweets, handle == "HillaryClinton")
clinton.text <- paste(clinton$text, collapse = " ")
clinton.words <- get_words_with_freq(clinton.text)

#--- prepare the plotting device
dev.new(width = 1000, height = 1000, unit = "px")
par(mfrow = c(1,2))

#--- Trump word cloud
# pick a red palette (with the help of colorbrewer2.org)
pal <- brewer.pal(n = 9, name = "Greens")
pal <- pal[-(1:4)]
# word cloud
wordcloud(trump.words$tweet.words, trump.words$n, 
          scale = c(4, .5), max.words = 50, colors = pal)

#--- Clinton word cloud
# pick a blue palette (with the help of colorbrewer2.org)
pal <- brewer.pal(n = 9, name = "Blues")
pal <- pal[-(1:4)]
# word cloud
wordcloud(clinton.words$tweet.words, clinton.words$n, 
          scale = c(4, .5), max.words = 50, colors = pal)

