# Datacamp 
# Text mining track
# Intro to text analysis (1)

library(tidytext)

# CHAPTER 1 ---- 

# roomba reviews ---- 

room <- read_csv("Intro text analysis/data/Roomba Reviews.csv") %>%
    mutate(id=1:nrow(room))

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

room_tk <- room %>%
    unnest_tokens(word, Review) %>%
    anti_join(stop_words2)

head(room_tk)

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

# improving sentiment analysis ---- 

head(room_tk)

sent_stars <- room_tk %>%
    inner_join(get_sentiments("bing")) %>%
    count(Stars, sentiment) %>%
    pivot_wider(names_from=sentiment,
                values_from=n) %>%
    mutate(sent=positive-negative,
           stars=fct_reorder(as_character(Stars), sent))

sent_stars

ggplot(sent_stars, aes(sent, stars, fill=stars)) +
    geom_col()

twt_tk %>% 
    # Append the afinn sentiment dictionary
    inner_join(get_sentiments("afinn")) %>% 
    # Group by both complaint label and whether or not the user is verified
    group_by(complaint_label, usr_verified) %>% 
    # Summarize the data with an aggregate_value = sum(value)
    summarise(aggregate_value = sum(value)) %>% 
    # Spread the complaint_label and aggregate_value columns
    pivot_wider(names_from = complaint_label, values_from = aggregate_value) %>% 
    mutate(overall_sentiment = Complaint + `Non-Complaint`)

# CHAPTER 4 ----

# Latent Dirichlet Allocation ---- 

room_tk <- room_tk %>%
    mutate(id=1:nrow(room_tk))

head(room_tk)

room_tk %>%
    count(word, id) %>%
    arrange(desc(n))


?cast_dtm

room_dtm <- room_tk %>%
    count(word, id) %>% 
    cast_dtm(id, word, n) %>%
    as.matrix()

room_dtm[1:4,1000:1004]

library(topicmodels)

room_lda <- LDA(room_dtm,
               k=2,
               method="Gibbs",
               control=list(seed=42))

room_lda
summary(room_lda)
glimpse(room_lda)

room_topics <- lda_out %>%
    tidy(matrix="beta")

room_topics %>%
    arrange(desc(beta))

room_topics %>%
    group_by(topic) %>%
    summarise(sum=sum(beta),
              n=n())

room_probs <- room_topics %>%
    group_by(topic) %>%
    slice_max(beta, n=10) %>%
    arrange(desc(beta)) %>%
    ungroup() %>%
    mutate(term2=fct_reorder(term, beta))

room_probs

ggplot(room_probs, aes(beta, term2)) + 
    geom_col(width=.5,
             fill="dodgerblue2",
             alpha=.6) +
    facet_wrap(~topic,
               scales="free_y") +
    labs(title="Top words in each topic",
         x="Probability",
         y="Word") +
    theme(axis.title.y=element_text(angle=0, vjust=.5)) +
    faceted

# Run an LDA with 3 topics and a Gibbs sampler
room_lda2 <- LDA(
    room_dtm,
    k=3,
    method="Gibbs",
    control = list(seed = 42)) 

head(room_lda2)
glimpse(room_lda2)

# Tidy the matrix of word probabilities
room_topics2 <- room_lda2 %>% 
    tidy(matrix="beta")

room_probs2 <- room_topics2 %>%
    group_by(topic) %>%
    slice_max(beta, n=15) %>%
    #ungroup() %>%
    mutate(term2=fct_reorder(term, beta)) %>%
    arrange(desc(beta))

room_probs2

ggplot(room_probs2, aes(beta, fct_reorder(term, beta), fill=topic)) +
    geom_col() +
    facet_wrap(~topic,
               scales="free") +
    faceted



