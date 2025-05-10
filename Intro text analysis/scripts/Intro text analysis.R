# Datacamp 
# Text mining track
# Intro to text analysis (1)

library(tidytext)

# CHAPTER 1 ---- 

# roomba reviews ---- 

room <- read_csv("Intro text analysis/data/Roomba Reviews.csv")

head(room)
frq(room$Product)

room %>%
    filter(Product=="iRobot Roomba 650 for Pets") %>%
    summarise(stars_mean=mean(Stars, na.rm=T))

room %>%
    group_by(Product) %>%
    summarise(stars=mean(Stars))

room %>%
    count(Product)

room_tk <- room %>%
    unnest_tokens(word, Review)
room_tk

room_tk %>%
    count(word) %>%
    arrange(desc(n))

head(stop_words)

room_tk2 <- room %>%
    unnest_tokens(word, Review) %>%
    anti_join(stop_words)

head(room_tk2)

room_tk2 %>%
    count(word) %>%
    arrange(desc(n))

# tweets ---- 

twt <- read_rds("Intro text analysis/data/ch_1_twitter_data.rds")
head(twt)

twt %>%
    count(complaint_label)

twt %>%
    group_by(complaint_label) %>%
    summarise(followers_ave=mean(usr_followers_count),
              followers_min=min(usr_followers_count),
              followers_max=max(usr_followers_count))

twt %>%
    filter(complaint_label=="Complaint") %>%
    count(usr_verified)

twt %>%
    group_by(usr_verified) %>%
    summarise(follower_ave=mean(usr_followers_count),
              n=n())

custom_stop_words <- tribble(
    ~word, ~lexicon,
    "roomba", "CUSTOM",
    "1", "CUSTOM",
    "2", "CUSTOM",
    "http", "CUSTOM",
    "win", "CUSTOM",
    "t.co", "CUSTOM")

stop_words2 <- stop_words %>%
    bind_rows(custom_stop_words)


twt_tk <- twt %>%
    unnest_tokens(word,tweet_text) %>%
    anti_join(stop_words2)

head(twt_tk)

twt_tk %>%
    filter(complaint_label=="Complaint") %>%
    count(word) %>%
    arrange(desc(n))

wrd_counts <- twt_tk %>%
    filter(complaint_label=="Complaint") %>%
    count(word) %>%
    filter(n>100) %>%
    arrange(desc(n))

wrd_counts

# CHAPTER 2 ---- 

# plotting word counts ---- 

ggplot(wrd_counts, aes(n, fct_reorder(word, n))) + 
    geom_col(width=.4,
             fill="dodgerblue",
             color="blue",
             alpha=.6) +
    labs(title="Complaints")

wrd_counts2 <- twt_tk2 %>%
    filter(complaint_label=="Non-Complaint") %>%
    count(word) %>%
    filter(n>150) %>%
    arrange(desc(n))

wrd_counts2

ggplot(wrd_counts2, aes(n, fct_reorder(word, n))) +
    geom_col(width=.4,
             fill="dodgerblue2",
             color="blue",
             alpha=.6) +
    labs(title="Non-complaints")

head(twt)

wrd_counts3 <- twt_tk %>%
    count(word, complaint_label) %>%
    group_by(complaint_label) %>%
    slice_max(n, n=20) %>%
    ungroup() %>%
    mutate(word2=fct_reorder(word, n))

wrd_counts3

ggplot(wrd_counts3, aes(n, fct_reorder(word, n))) + 
    geom_col() +
    facet_wrap(~complaint_label,
               scales="free_y") +
    faceted

# wordclouds ---- 

library(wordcloud)

wrd_counts <- twt_tk %>%
    count(word) %>%
    filter(n>100) %>%
    arrange(desc(n))

wrd_counts

wordcloud(wrd_counts$word,
          wrd_counts$n,
          min.freq=100,
          max.words=100,
          random.order=F,
          rot.per=.2,
          scale=c(3, .5),
          colors=brewer.pal(8, "Dark2"))


# CHAPTER 3 ----

# sentiment analysis ----

library(tidytext)

get_sentiments("nrc")

get_sentiments("nrc") %>%
    count(sentiment) %>%
    arrange(desc(n))

sentiment_counts <- get_sentiments("nrc") %>%
    count(sentiment) %>%
    mutate(sentiment2=fct_reorder(sentiment, n)) %>%
    arrange(desc(n))

sentiment_counts    

library(ggchicklet)

ggplot(sentiment_counts, aes(n, sentiment2)) + 
    geom_col(width=.5,
             color="blue",
             fill="dodgerblue2",
             alpha=.6) +
    labs(title="Sentiment counts in NRC",
         x="Counts",
         y="Sentiment") +
    theme(axis.title.y=element_text(angle=0, vjust=.5))
    # geom_chicklet(width=.3,
    #          color="blue",
    #          fill="dodgerblue2",
    #          alpha=.6) 


# counting sentiment ---- 

head(twt)
head(twt_tk)
get_sentiments("nrc")

twt_sent <- twt_tk %>%
    inner_join(get_sentiments("nrc")) 

head(twt_sent)

twt_sent %>%
    count(sentiment) %>%
    arrange(desc(n))

word_cnts <- twt_tk %>%
    inner_join(get_sentiments("nrc")) %>%
    filter(sentiment %in% c("positive","fear","trust")) %>%
    count(word, sentiment) %>%
    group_by(sentiment) %>%
    slice_max(n, n=10) %>%
    arrange(desc(n)) %>%
    ungroup() %>%
    mutate(word2=fct_reorder(word, n))

word_cnts    

ggplot(word_cnts, aes(n, fct_reorder(word2, n), fill=sentiment)) + 
    geom_col(show.legend=F,
             width=.5,
             alpha=.8) +
    facet_wrap(~sentiment,
               scales="free_y") +
    faceted




