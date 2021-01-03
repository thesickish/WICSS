#############################################################
#
#				WICSS-TUCSON	
#	Winter Institute in Computational Social Science
#
#	   Application Programming Interfaces (APIs)
#			Wednesday January 6th, 2020
#
#############################################################

# This script goes through two R packages that can be used to
# extract Reddit data: pushshiftR and RedditExtractoR. 

# The script then goes through the peRspective package, which
# accesses Google's Perspective API to label text for Toxicity
# and several other language features. Using the peRspective 
# package requires an API key. To obtain one, you must:

# 1) Create a Google Cloud project https://console.developers.google.com/
# 2) Enable the Perspective API https://console.developers.google.com/apis/api/commentanalyzer.googleapis.com/overview
# 3) Go to the API credentials page https://console.developers.google.com/apis/credentials

# The peRspective package requires adding your API key as an environment variable
# The command below will open up the file .Renviron. Add the following line to it:
# perspective_api_key="YOUR_API_KEY" (substituting YOUR_API_KEY with your key, of course)
# You will need to install and load the package usethis first if it is not already installed
install.packages("usethis") # skip if already installed
library(usethis) # skip if already installed
usethis::edit_r_environ()

# Now save the .Renviron file and restart R for the environment variables to be updated


#############################################################
#
# SECTION 1: Using the PushshiftR package
#
#############################################################


# Before installing PushshiftR, you need to install devtools and dplyr
install.packages("devtools") # skip if already installed
install.packages("dplyr") # skip if already installed

# Now you can install and load the package PushshiftR
devtools::install_github("https://github.com/nathancunn/pushshiftR")
library(pushshiftR)

# Test the pushshiftR package (example from package's Github page)
test <- getPushshiftData(postType = "comment",
                 size = 100,
                 after = "1546300800",
                 subreddit = "soccer",
                 nest_level = 1)
test

# Note: for now, scores (i.e. up votes - down votes) are always 1
# reddit.com/r/pushshift/comments/avwfjw/comment_score_always_1
table(test$score)

# Convert timestamps to readable dates
test$date = as.POSIXlt(test$created_utc, origin="1970-01-01 00:00:00")
test

# Define a machine readable timestamp for 11/3/2020 (U.S. Election)
election <- as.POSIXlt("2020-11-03 00:00:00",origin="1970-01-01 00:00:00")
election <- as.character(as.numeric(election))

# Collect 100 submissions to r/politics on or after election day
sub_after <- getPushshiftData(postType = "submission",
                 size = 100,
                 after = election,
                 subreddit = "politics",
                 nest_level = 1)

sub_after$date = as.POSIXlt(sub_after$created_utc, origin="1970-01-01 00:00:00")
sub_after

# Collect 100 submissions to r/politics before election day
sub_before <- getPushshiftData(postType = "submission",
                 size = 100,
                 before = election,
                 subreddit = "politics",
                 nest_level = 1)

sub_before$date = as.POSIXlt(sub_before$created_utc, origin="1970-01-01 00:00:00")
sub_before

# Collect 100 pre-election submissions containing the word "Biden"
sub_biden <- getPushshiftData(postType = "submission",
                 size = 100,
                 q = "Biden",
                 before = election,
                 subreddit = "politics",
                 nest_level = 1)

sub_biden$date = as.POSIXlt(sub_biden$created_utc, origin="1970-01-01 00:00:00")
sub_biden


#############################################################
#
# SECTION 2: Using the RedditExtractoR package
#
#############################################################


# Install and load the package RedditExtractoR
install.packages("RedditExtractoR") # skip if already installed
library(RedditExtractoR)

# Testing the RedditExtractoR package
url <- "reddit.com/r/pushshift/comments/avwfjw/comment_score_always_1"
red <- reddit_content(url, wait_time = 2)

red

# Collect comment information for 10 Biden pre-election submissions

sub_biden$url = paste0("https://www.reddit.com/comments/",sub_biden$id)
sub_biden[1:10]
sub_biden$url[1:10]

com_biden <- reddit_content(sub_biden$url[1:10], wait_time = 2)

# Summarize comments data
dim(com_biden)
names(com_biden)
table(com_biden$URL)

# Compare to comment counts in submission data
sub_biden[1:10,c("url","num_comments")]

sub_biden$URL = paste0(sub_biden$url,"/?ref=search_posts")
sub_biden[1:10,c("num_comments","url","URL")]

tab_biden = cbind(names(table(com_biden$URL)),table(com_biden$URL))
tab_biden

dimnames(tab_biden) = NULL
colnames(tab_biden) = c("URL","comments_obtained")
tab_biden

mer_biden = merge(x = sub_biden[1:10,],y = tab_biden, by = "URL")
mer_biden[,c("URL","title","num_comments","comments_obtained")]

# Summarize comment and post scores 
plot(com_biden$comment_score)
max(com_biden$comment_score)
max = subset(com_biden, comment_score == max(com_biden$comment_score))
max

table(com_biden$URL,com_biden$post_score)

# Plot comment graph of the most popular pre-election Biden submission
max_biden = subset(com_biden,URL == max$URL)
dim(max_biden)
graph_max <- construct_graph(max_biden)

# Plot comment graph of the most popular pre-election Biden submission
last_biden = subset(com_biden,URL == sub_biden$URL[1])
dim(last_biden)
graph_last <- construct_graph(last_biden)


#############################################################
#
# SECTION 3: Using the peRspective package
#
#############################################################


# Install and load the package peRspective
devtools::install_github("favstats/peRspective") # skip if already installed
library(peRspective)

# Test the peRspective package
# Example taken from the Perspective API homepage
# https://www.perspectiveapi.com/
example = c("What if technology could help improve conversations online? 
Abuse and harassment stop people from expressing themselves or makes them give up on conversations entirely")

# Score the example text
score <- prsp_score(
           example, 
           score_sentences = F,
           score_model = peRspective::prsp_models)

# Plot the example text scores
score %>% 
  tidyr::gather() %>% 
  dplyr::mutate(key = forcats::fct_reorder(key, value)) %>% 
  ggplot2::ggplot(ggplot2::aes(key, value)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::ylim(0, 1) +
  ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed") +
  ggplot2::labs(x = "Model", y = "Probability", title = "Perspective API Results")

# Score the example text with sentences scored separately
score2 <- prsp_score(
           example,
           score_sentences = T,
           score_model = peRspective::prsp_models)

# Plot the examples text scores with sentences scored separately
score2 %>% 
  tidyr::unnest(sentence_scores) %>% 
  dplyr::select(type, score, sentences) %>% 
  tidyr::gather(value, key, -sentences, -score) %>% 
  dplyr::mutate(key = forcats::fct_reorder(key, score)) %>% 
  ggplot2::ggplot(ggplot2::aes(key, score)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~sentences, ncol = 2) +
  ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed") +
  ggplot2::labs(x = "Model", y = "Probability", title = "Perspective API Results")

# Notes: You can use the parameter languages = "es" to label Spanish text
# You can use the parameter doNotStore = TRUE for private texts

# Score the first 10 comments of the most popular pre-election Biden submission
names(max_biden)
max_biden10 = max_biden[1:10,]
max_biden10

score_biden10 <- max_biden10 %>%
  prsp_stream(text = comment,
              text_id = structure,
              score_model = c("TOXICITY", "SEVERE_TOXICITY", "INSULT"),
              safe_output = T)

score_biden10

# Merge comment data with the scores
colnames(score_biden10)[1] = c("structure")
mer_score10 = merge(x = max_biden10,y = score_biden10, by = "structure")
mer_score10[,c("structure","comment_score","TOXICITY","SEVERE_TOXICITY","INSULT","comment")]

# Plot toxicity scores against the comments scores
plot(mer_score10$comment_score[1:10],mer_score10$TOXICITY[1:10])

# Score all comments of the most popular pre-election Biden submission
# Note: this takes a few minutes to run
# score_biden <- max_biden %>%
#  prsp_stream(text = comment,
#              text_id = structure,
#              score_model = c("TOXICITY", "SEVERE_TOXICITY", "INSULT"),
#              safe_output = T)
#
# Merge comment data with the scores
# colnames(score_biden)[1] = c("structure")
# mer_score = merge(x = max_biden,y = score_biden, by = "structure")
# write.csv(mer_score,"mer_score.csv")

# Read saved merged file of comments on the most popular pre-election Biden submission
mer_read <- read.csv("mer_score.csv")

# Plot toxicity scores against the comments scores
plot(mer_read$comment_score,mer_read$TOXICITY)
plot(mer_read$comment_score,mer_read$TOXICITY,log="xy")
cor(log(mer_read$comment_score),log(mer_read$TOXICITY),use = "pairwise.complete.obs")


