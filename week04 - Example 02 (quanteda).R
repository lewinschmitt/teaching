## This tutorial is based on http://www.luigicurini.com/applied-scaling--classification-techniques-in-political-science.html, with minor updates and fixes

install.packages("quanteda")
library(quanteda)
install.packages("quanteda.textplots")
library(quanteda.textplots)

# Let's load a corpus already presented in Quanteda: the corpus of all the US Presidents' Inaugural Speeches
# To summarize the texts from a corpus, we can call a summary() method defined for a corpus.
View(summary(data_corpus_inaugural))
# inspect the document-level variables (this is a very important option, as we will see later on...)
head(docvars(data_corpus_inaugural))

# to extract texts from a a corpus, we simply use the as.character() function
as.character(data_corpus_inaugural)[1]
as.character(data_corpus_inaugural)[2]

# to see the entire Trump speech
trump <- corpus_subset(data_corpus_inaugural, President == "Trump")
as.character(trump )[1]

# use strwrap() to format paragraphs
strwrap(trump [[1]])

# Adding two corpus together
# First five inaug. speeches
mycorpus1 <- corpus(data_corpus_inaugural[1:5])
# Last five inaug. speeches
mycorpus2 <- corpus(data_corpus_inaugural[53:58])
mycorpus3 <- mycorpus1 + mycorpus2
summary(mycorpus3)

# subsetting a corpus 
summary(corpus_subset(data_corpus_inaugural, Year > 1990))
summary(corpus_subset(data_corpus_inaugural, President == "Adams"))
summary(corpus_subset(data_corpus_inaugural, Year > 1990 & Party== "Republican"))

# Exploring corpus texts
# The kwic function (keywords-in-context) performs a search for a word and allows us to view 
# the contexts in which it occurs:

options(width = 200)
kwic(data_corpus_inaugural, "spain")
kwic(data_corpus_inaugural, "europe")
# also the words starting with "Europe" including "European"
kwic(data_corpus_inaugural, "europe", valuetype = "regex")
kwic(data_corpus_inaugural, "communist*")

#########################################################################
# Statistical summaries (1): Plotting the wordclouds 
#########################################################################

myCorpus <- corpus_subset(data_corpus_inaugural, Year > 1990)
summary(myCorpus)

# construct a dfm
myDfm <- tokens(data_corpus_inaugural,
                   remove_punct = TRUE, 
                   remove_numbers = TRUE) %>%
  dfm(tolower = TRUE)
  
head(myDfm)

# remove stopwords
myDfm <- dfm_remove(myDfm, stopwords("english"))
head(myDfm)

# stem
myDfm <- dfm_wordstem(myDfm)
head(myDfm)

# by defining a seed number, you are sure to get always the same plot
set.seed(100)
textplot_wordcloud(myDfm , min_count = 6, random_order = FALSE,
                   rot_per = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

textplot_wordcloud(myDfm , min_count = 100,
                   colors = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))

# Trump's speech
trump <- corpus_subset(data_corpus_inaugural, Year > 2015)
summary(trump)
trump2 <- dfm(trump, remove = stopwords("english"),
              remove_punct = TRUE)
set.seed(100)
textplot_wordcloud(trump2, min_count= 1, random_order = FALSE,
                   rot_per = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))


#########################################################################
# Statistical summaries (2): Lexical diversity 
#########################################################################

# textstat_lexdiv() calcuates lexical diversity in various measures based on the number of unique types of tokens 
# and the length of a document. It is useful for analysing speakers’ or writers’ linguistic skill, or complexity 
# of ideas expressed in documents.

inaug_dfm <- dfm(data_corpus_inaugural, remove = stopwords('en'))
lexdiv <- textstat_lexdiv(inaug_dfm)

# Type-Token Ratio (TTR) is estimated as V/N, where
# V (types=total number of unique terms); N (tokens=total number of words in the dfm)
head(lexdiv, 5)
tail(lexdiv, 5)

#########################################################################
# Statistical summaries (3): Comparing words associated with a target group vs. reference group
#########################################################################

# If you want to compare the differential associations of keywords in a target and reference group, 
# you can calculate “keyness” which is based on textstat_keyness. 
# In this example, we compare the inaugural speech by Donald Trump with the speeches by Barack Obama

pres_corpus <- corpus_subset(data_corpus_inaugural, 
                             President %in% c("Obama", "Trump"))
summary(pres_corpus)

# Create a dfm grouped by president
pres_dfm <- tokens(pres_corpus, remove_punct = TRUE) %>%
  dfm(tolower = T) %>%
  dfm_remove(stopwords("english"))

# Calculate keyness and determine Trump as target group
result_keyness <- textstat_keyness(pres_dfm, target = "2017-Trump")

# if you get a negative value, it means that Obama uses that word more than Trump (i.e., the target group) and viceversa
# Plot estimated word keyness
textplot_keyness(result_keyness) 
# what is chi2? Chi-squared test is used to determine whether there is a statistically significant difference between 
# the expected frequencies and the observed frequencies in one or more categories of a contingency table (in our cases
# we are talking about the frequencies of words in two different set of texts)
# Plot without the reference text (in this case Obama)
textplot_keyness(result_keyness, show_reference = FALSE)

head(result_keyness , 10)
tail(result_keyness , 10)
