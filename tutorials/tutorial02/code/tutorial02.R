#######################################
# Tutorial 2: APIs and pre-processing #
#######################################
# guardian api key
#ef52c89d-84a8-473c-bc5d-7fd4e6f193ae
rm(list=ls())

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(prompt = "# ")
#ts+TAB

# ref datastore 
#"C:\Data\tcd_asds\data_files"

## Load packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "guardianapi", # for working with the Guardian's API
         "quanteda", # for QTA
         "quanteda.textstats", # more Quanteda!
         "quanteda.textplots", # even more Quanteda!
         "readtext", # for reading in text data
         "stringi", # for working with character strings
         "textstem" # an alternative method for lemmatizing
       ), pkgTest)


# Fri Feb  3 12:17:50 2023 ------------------------------

### A. Using the Guardian API with R
gu_api_key() # run this interactive function

# We want to query the API on articles featuring Ukraine since Jan 1 2023
dat <- gu_content(query = "Ukraine", from_date = "2023-01-01") # making a tibble

# We'll save this data
saveRDS(dat, "data/df2023.RDS")
# And make a duplicate to work on
df <- dat  

# Take a look at the kind of object which gu_content creates. 
# Try to find the column we need for our text analyses
head(df) # checking our tibble
View(df)
# headline
# standfirst - 'abstract' at top of article
# body_text = article with no html tags

# get articles from world section  (ie not liveblogs or comment)
df <- df[df$type == "article" & df$section_id == "world",] # see if you can subset the object to focus on the articles we want

# corpus doc names must be unique, so elim dupliates
which(duplicated(df$web_title) == TRUE) # sometimes there are duplicates...
df <- df[!duplicated(df$web_title),] # which we can remove

### B. Making a corpus
# We can use the corpus() function to convert our df to a quanteda corpus
corpus_ukr <- corpus(df, 
                     docid_field = "headline", 
                     text_field = "body_text") # select the correct column here

# Checking our corpus
summary(corpus_ukr, 5)

as.character(corpus_ukr)[1]

#test data quality
stri_replace_first(corpus_ukr, replacement = "",
                                 regex = "\u2022.+5")[1]


corpus_ukr <- stri_replace_first(corpus_ukr, 
                                 replacement = "",
                                 regex = "\,+?\'")

### C. Pre-processing
## 1. Cleaning the text with regexs and stringi

# Let's take a look at the first article and see if we can spot any big problems
#docid + body text - in ""
as.character(corpus_ukr)[1]

# It looks like each text includes the headline, with the body inside "". We might
# decide we only want the body text, in which case we'd need to get rid of everything 
# before the first ". We can use the stringi package to help with this.
test <- as.character(corpus_ukr)[1] # make a test object

stri_replace_first(test, 
                   replacement = "", # nothing here (i.e. we're removing)
                   regex = "^.+?\"") #try to write the correct regex - this may help: https://www.rexegg.com/regex-quickstart.html

# Sometimes there's also boilerplate at the end of an article after a big centre dot. 
as.character(corpus_ukr)[which(grepl("\u2022.+$", corpus_ukr))[1]]

# We could get rid of all that too with a different function
test <- as.character(corpus_ukr)[which(grepl("\u2022.+$", corpus_ukr))[1]]
stri_replace_last(test, 
                  replacement = "",
                  regex = "\u2022.+$")

# These might be useful to out analysis though, so for now we'll keep them in.
stri_replace_first(corpus_ukr, 
                   replacement = "", # nothing here (i.e. we're removing)
                   regex = "^.+?\"") #try to write the correct regex - this may help: https://www.rexegg.com/regex-quickstart.html
#stri_replace_last(corpus_ukr, 
#                  replacement = "",
#                  regex = "\u2022.+$")




## 2. Tokenize the text
# quanteda only uses text_field, so ignores rest of columns
# The next step is to turn all the words into tokens. 
# The tokens() function can also remove punctuation and symbols for us.
toks <- quanteda::tokens(corpus_ukr, 
                         remove_punct = TRUE, 
                         remove_symbols = TRUE)

## 3. Lowercase the text
toks <- tokens_tolower(toks) # lowercase tokens
print(toks[10]) # print lowercase tokens from the 10th article in corpus.

## 4. Remove stop words
# Now we can use a dictionary to remove stop words, such as articles, etc. 
# We do this using quanteda's built-in stopwords() function and the "english" 
# dictionary. 

#Let's have a quick look at these.
stop_list <- stopwords("english") # load English stopwords from quanteda
head(stop_list)                   # show first 6 stopwords from stopword list.  
length(stop_list)                 # 175
# Notice how these stopwords are also lowercased.
class(stop_list)
stop_list <- append(stop_list,"say")
tail(stop_list)

# The tokens_remove() function allows us to apply the stop_list to our toks object
toks <- tokens_remove(toks, stop_list)

toks[10] # print list of tokens from 10th article without stop words.  

# Notice how much shorter the list is now. Can you imagine how much longer it
# might take to run your model if you don't do this bit properly...

## 5.a. Normalising (or stemming) the tokens
# Now we'll stem the words using the tokens_wordstem() function
stem_toks <- tokens_wordstem(toks)

stem_toks[10] # print stemmed tokens from 10th document - notice any differences?

## 5.b. Lemmatizing - an alternative
# An alternative normalization technique is to collapse different inflections of 
# a word to a root form. We'll use the textstem package to do this.

# i. Convert quanteda tokens object to list of tokens
toks_list <- as.list(toks) 

# ii. Apply the lemmatize_words function from textstem to the list of tokens
lemma_toks <- lapply(toks_list, lemmatize_words) 

# iii. Convert the list of lemmatized tokens back to a quanteda tokens object
lemma_toks <- as.tokens(lemma_toks) 

stem_toks[10] # print stemmed tokens from 10th document - notice any differences?
lemma_toks[10] # print stemmed tokens from 10th document - notice any differences?

# Compare article 10 in toks, stem_toks and lemma_toks: what do you notice?
# Which is smallest?

## 6. Detect collocations
# Collocations are groups of words (grams) that are meaningful in combination. 
# To identify collocations we use the quanteda textstats package 

# i. Identify collocations
collocations <- textstat_collocations(lemma_toks, size = 2)
head(collocations)

# ii. Choose which to keep
keep_coll_list <- collocations$collocation[1:20]
keep_coll_list

# iii. Apply to tokens object
comp_tok <- tokens_compound(lemma_toks, keep_coll_list)

### D. Creating the document-feature matrix (dfm)
# Now that we've finished pre-processing our tokens object, we can convert it 
# into a dfm using quanteda's dfm() function

# Convert to dfm...
dfm_ukr <- dfm(comp_tok)

# ...and asave
saveRDS(dfm_ukr, "data/dfm.RDS")

# We'll leave operations on the dfm until next time, but to give a preview, here are 
# some functions we can use to analyse the dfm.
topfeatures(dfm_ukr)

# We can also visualise the dfm using the textplots package from quanteda
dfm_ukr %>%
  dfm_trim(min_termfreq = 3) %>%
  textplot_wordcloud(min_size = 1, max_size = 10, max_words = 100)

### Activity
# In this week's \data repository, you'll find a file called df2022. This is 
# an extract of articles from January 2022, shortly before the war began. 
# See if you can repeat pre-processing on this data, and compare the features
# and wordcloud that results.

# Fri Feb  3 13:42:04 2023 ------------------------------

df2022 <- readRDS("data/df2022")

df <- df2022  

# Take a look at the kind of object which gu_content creates. 
# Try to find the column we need for our text analyses
head(df) # checking our tibble
View(df)

# get articles from world section  (ie not liveblogs or comment)
df <- df[df$type == "article" & df$section_id == "world",] # see if you can subset the object to focus on the articles we want

# corpus doc names must be unique, so elim duplicates
which(duplicated(df$web_title) == TRUE) # sometimes there are duplicates...
df <- df[!duplicated(df$web_title),] # which we can remove

### B. Making a corpus
# We can use the corpus() function to convert our df to a quanteda corpus
corpus_ukr <- corpus(df, 
                     docid_field = "headline", 
                     text_field = "body_text") # select the correct column here

# Checking our corpus
summary(corpus_ukr, 5)

as.character(corpus_ukr)[1]

#test data quality
stri_replace_first(corpus_ukr, replacement = "",
                   regex = "\u2022.+5")[1]


##corpus_ukr <- stri_replace_first(corpus_ukr, 
#                                 replacement = "",
#                                 regex = "\,+?\'")

### C. Pre-processing
## 1. Cleaning the text with regexs and stringi

# Let's take a look at the first article and see if we can spot any big problems
#docid + body text - in ""
as.character(corpus_ukr)[1]

# It looks like each text includes the headline, with the body inside "". We might
# decide we only want the body text, in which case we'd need to get rid of everything 
# before the first ". We can use the stringi package to help with this.
test <- as.character(corpus_ukr)[1] # make a test object

stri_replace_first(test, 
                   replacement = "", # nothing here (i.e. we're removing)
                   regex = "^.+?\"") #try to write the correct regex - this may help: https://www.rexegg.com/regex-quickstart.html

# Sometimes there's also boilerplate at the end of an article after a big centre dot. 
as.character(corpus_ukr)[which(grepl("\u2022.+$", corpus_ukr))[1]]

# We could get rid of all that too with a different function
test <- as.character(corpus_ukr)[which(grepl("\u2022.+$", corpus_ukr))[1]]
stri_replace_last(test, 
                  replacement = "",
                  regex = "\u2022.+$")

# These might be useful to out analysis though, so for now we'll keep them in.
stri_replace_first(corpus_ukr, 
                   replacement = "", # nothing here (i.e. we're removing)
                   regex = "^.+?\"") #try to write the correct regex - this may help: https://www.rexegg.com/regex-quickstart.html
stri_replace_last(corpus_ukr, 
                  replacement = "",
                  regex = "\u2022.+$")




## 2. Tokenize the text
# quanteda only uses text_field, so ignores rest of columns
# The next step is to turn all the words into tokens. 
# The tokens() function can also remove punctuation and symbols for us.
toks <- quanteda::tokens(corpus_ukr, 
                         remove_punct = TRUE, 
                         remove_symbols = TRUE)

## 3. Lowercase the text
toks <- tokens_tolower(toks) # lowercase tokens
print(toks[10]) # print lowercase tokens from the 10th article in corpus.

## 4. Remove stop words
# Now we can use a dictionary to remove stop words, such as articles, etc. 
# We do this using quanteda's built-in stopwords() function and the "english" 
# dictionary. 

#Let's have a quick look at these.
stop_list <- stopwords("english") # load English stopwords from quanteda
head(stop_list)                   # show first 6 stopwords from stopword list.  
length(stop_list)                 # 175
# Notice how these stopwords are also lowercased.
class(stop_list)
stop_list <- append(stop_list,"say")
tail(stop_list)

# The tokens_remove() function allows us to apply the stop_list to our toks object
toks <- tokens_remove(toks, stop_list)
toks<- tokens_remove(toks, "say")

toks[10] # print list of tokens from 10th article without stop words.  

# Notice how much shorter the list is now. Can you imagine how much longer it
# might take to run your model if you don't do this bit properly...

## 5.a. Normalising (or stemming) the tokens
# Now we'll stem the words using the tokens_wordstem() function
stem_toks <- tokens_wordstem(toks)

stem_toks[10] # print stemmed tokens from 10th document - notice any differences?

## 5.b. Lemmatizing - an alternative
# An alternative normalization technique is to collapse different inflections of 
# a word to a root form. We'll use the textstem package to do this.

# i. Convert quanteda tokens object to list of tokens
toks_list <- as.list(toks) 

# ii. Apply the lemmatize_words function from textstem to the list of tokens
lemma_toks <- lapply(toks_list, lemmatize_words) 

# iii. Convert the list of lemmatized tokens back to a quanteda tokens object
lemma_toks <- as.tokens(lemma_toks) 

stem_toks[10] # print stemmed tokens from 10th document - notice any differences?
lemma_toks[10] # print stemmed tokens from 10th document - notice any differences?

# Compare article 10 in toks, stem_toks and lemma_toks: what do you notice?
# Which is smallest?

## 6. Detect collocations
# Collocations are groups of words (grams) that are meaningful in combination. 
# To identify collocations we use the quanteda textstats package 

# i. Identify collocations
collocations <- textstat_collocations(lemma_toks, size = 2)
head(collocations)

# ii. Choose which to keep
keep_coll_list <- collocations$collocation[1:20]
keep_coll_list

# iii. Apply to tokens object
comp_tok <- tokens_compound(lemma_toks, keep_coll_list)

### D. Creating the document-feature matrix (dfm)
# Now that we've finished pre-processing our tokens object, we can convert it 
# into a dfm using quanteda's dfm() function

# Convert to dfm...
dfm_ukr <- dfm(comp_tok)

# ...and asave
saveRDS(dfm_ukr, "data/dfm.RDS")

# We'll leave operations on the dfm until next time, but to give a preview, here are 
# some functions we can use to analyse the dfm.
topfeatures(dfm_ukr)

# We can also visualise the dfm using the textplots package from quanteda
dfm_ukr %>%
  dfm_trim(min_termfreq = 3) %>%
  textplot_wordcloud(min_size = 1, max_size = 10, max_words = 100)

