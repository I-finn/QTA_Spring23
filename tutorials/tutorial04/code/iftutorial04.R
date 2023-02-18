#############################
# Tutorial 4: Supervised ML #
#############################
rm(list=ls())

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(prompt = "# ")
#ts+TAB

## Load packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "guardianapi",
         "quanteda", 
         "lubridate",
         "quanteda.textmodels", 
         "quanteda.textstats", 
         "caret", # For train/test split
         "MLmetrics", # For ML
         "doParallel"), # For parallel processing
       pkgTest)

## 1. Acquire (?), read in and wrangle data
dat <- readRDS("data/df2023")

# You need to a) Subset on section_name, using World news and Opinion, and 
#                type, using article.

#caret doesn't like labels with whitespace
# This code relabels our data, because "World news" contains whitespace...
dat$section_name <- ifelse(dat$section_name == "World news", "World", 
                           dat$section_name)

View(dat)

#ukr23 <- dat[dat$type == "article" & (dat$section_name == "World" | dat$section_name == "Opinion"),]
ukr23 <- dat[dat$type == "article" & dat$section_name %in% c("World" , "Opinion"),]


unique(ukr23$section_name)
unique(dat$section_name)
unique(ukr23$section_id)  # commentisfree is id for section_name = Opinion

#             b) Select relevant columns.
# First, we'll tidy up our initial dataframes.
tidy23 <- ukr23 %>%
  select(headline,
         byline,
         date = web_publication_date, # Rename date variable
         section_name,
         standfirst,
         body_text
  ) %>%
  mutate(date = as_datetime(date)) # parse date

#class(tidy23$date)
# Next, we'll remove the initial (large) dataframes from memory
rm(ukr23)

# Now we have a more wieldy tibble. 
head(tidy23)

#             c) Remove duplicates.
# Let's check for duplicates again:
which(duplicated(tidy23$headline))

# We can use the same code to drop duplicated headlines:
tidy23 <- tidy23[-which(duplicated(tidy23$headline)),]
#  ?duplicated(x, fromLast = "TRUE") - reverse search

## 2. QTA Preparation
# You need to a) Remove the large round symbol.
# Let's also tidy the body_text column before we transform into a corpus
tidy23$body_text <- str_replace(tidy23$body_text, "\u2022.+$", "")
#str_detect(tidy23$body_text, "\u2022")  check

#             b) Convert to a corpus.
corp23 <- corpus(tidy23, 
                 docid_field = "headline",
                 text_field = "body_text")

# Creating a useful summary object of our corpus
corpSum23 <- summary(corp23, 
                     n = nrow(docvars(corp23)) #note: the default is n=100
)
head(corpSum23[,-8])
#head(corpSum23[,]) # col 8 is full text


#             c) and d) Clean the corpus and find collocations.


# For steps c) and d), check out the pre_processing.R script.

source("code/pre_processing.R")
prepped_toks <- prep_toks(corp23) # basic token cleaning

ggplot(data = NULL) +
  geom_density(aes(yday(corpSum23$date)), color = "blue")  +
# doesn't work
  theme(legend.text = element_text(), legend.position = "bottom")

# We can calculate additional statistics using the summary object. 
# For example, the TTR is the ratio of types to tokens:
corpSum23$ttr <- corpSum23$Types / corpSum23$Tokens

# We can plot this over time as well:
ggplot(data = NULL) +
  geom_point(aes(yday(corpSum23$date), corpSum23$ttr), col = "blue") +
  geom_smooth(aes(yday(corpSum23$date), corpSum23$ttr), col = "blue")

#             e) Make tokens.
# prepped_toks = tokens obj - lowercase, stopwords(en) removed
class(prepped_toks)
#             f) Clean tokens.
#prepped_toks <- tokens_replace(prepped_toks, "2s", "2")

collocations <- get_coll(prepped_toks) # get collocations
#summary(collocations)

#View(collocations)

#toks23 <- tokens_compound(prepped_toks, pattern = collocations[collocations$z > 16,])
toks23 <- tokens_compound(prepped_toks, pattern = collocations[collocations$z > 10,])
#             g) Create the dfm.
toks23 <- tokens_remove(quanteda::tokens(toks23), "") 
toks23 <- tokens(toks23, remove_punct = TRUE, remove_symbols = TRUE, 
                 remove_numbers = TRUE , remove_url = TRUE, remove_separators = TRUE)

# Stem tokens
#toks23 <- tokens_wordstem(toks23)

# Removing extra stopwords
# Last week we used the topfeatures() function to check our dfm
# for features that should have been removed. Let's do that again
# this time: create a dfm for both tokens objects, then go back to
# remove the necessary stopwords.
#extra_stopwords=c("said", "russia", "ukrain", "say", "russian", "ukrainian")

dfm <- dfm(toks23)

#topfeatures(dfm)

# iterate
#extra_stopwords=c("said", "also", "russia", "ukrain", "say", 
#                  "russian", "ukrainian",
#                  valuetype="fixed")
#toks23 <- tokens_remove(toks23, extra_stopwords)


#             h) Trim and weight the dfm
dfm <- dfm_trim(dfm, min_docfreq = 10) # trim DFM
dfm <- dfm_tfidf(dfm) # weight DFM

#             i) Convert dfm to dataframe for ML
tmpdata <- convert(dfm, to = "data.frame", docvars = NULL)
tmpdata <- tmpdata[, -1] # drop document id variable (first variable)
# quanteda = S4 objects = @ metadata
section_labels <- dfm@docvars$section_name # get section labels - note, the @ operator is specific to S4 class object
tmpdata <- as.data.frame(cbind(section_labels, tmpdata)) # labelled data frame

str(tmpdata)
## 3. ML Preparation
# You need to a) Create a 5% validation split   - 5% std for validation set
set.seed(2023) # set seed for replicability
tmpdata <- tmpdata[sample(nrow(tmpdata)), ] # randomly order labelled dataset
split <- round(nrow(tmpdata) * 0.05) # determine cutoff point of 5% of documents
vdata <- tmpdata[1:split, ] # validation set
ldata <- tmpdata[(split + 1):nrow(tmpdata), ] # labelled dataset minus validation set

#             b) Create an 80/20 test/train split
train_row_nums <- createDataPartition(ldata$section_labels, 
                                      p=0.8, 
                                      list=FALSE) # set human_labels as the Y variable in caret
Train <- ldata[train_row_nums, ] # training set
Test <- ldata[-train_row_nums, ] # testing set

head(Train,1)
head(Test, 1)
#             c) Create five-fold cross validation with 3 repeats object - to supply to train()
# subsetting training set so don't train on whole thing
train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs= TRUE, 
  summaryFunction = multiClassSummary,
  selectionFunction = "best", # select the model with the best performance metric
  verboseIter = TRUE
)

## Data now prepped and subsetted

## 4. Naive Bayes classification
# You need to a) Check the parameters for Naive Bayes algorithm
modelLookup(model = "naive_bayes")

#  b) Create a matrix of combinations of parameters to supply to tuneGrid arg of train()
tuneGrid <- expand.grid(laplace = c(0,0.5,1.0),
                        usekernel = c(TRUE, FALSE),
                        adjust=c(0.75, 1, 1.25, 1.5))
# model runs once for each set of parameters
tuneGrid

#             c) Set up parallel processing
cl <- makePSOCKcluster(6) # create number of copies of R to run in parallel and communicate over sockets
# Note that the number of clusters depends on how many cores your machine has.  
registerDoParallel(cl) # register parallel backed with foreach package

#require(naivebayes)
#require(caret)
#             d) Train the model

# Thu Feb 16 16:56:05 2023 ------------------------------
# =========================================================================
# Thu Feb 16 16:56:05 2023 ------------------------------

nb_train <- train(section_labels ~ .,   # use all variables (words) in model
                  data = Train,  
                  method = "naive_bayes", 
                  metric = "F1",
                  trControl = train_control,
                  tuneGrid = tuneGrid,
                  allowParallel= TRUE
)
# V slow
#Aggregating results
#Selecting tuning parameters
#Fitting laplace = 0, usekernel = FALSE, adjust = 0.75 on full training set
#Warning messages:
#  1: In makepredictcall(variables[[i]], vars[[i + 1L]]) :
#  closing unused connection 8 (<-hex-2022:11646)
  
#             e) Save the model!
saveRDS(nb_train, "data/nb_train")
# =========================================================================

#             f) If your machine is running slow... read in the model
nb_train <- readRDS("data/nb_train")

#             g) Stop the cluster
stopCluster(cl) # stop parallel process once job is done

#             h) Evaluate performance
print(nb_train) # print cross-validation results

pred <- predict(nb_train, newdata = Test) # generate prediction on Test set using training set model
#============================================================================
# different processing from martin's trainRDS

head(pred) # first few predictions

confusionMatrix(reference = as.factor(Test$section_labels), data = pred, mode='everything') # generate confusion matrix

#             i) Finalise the model
nb_final <- train(section_labels ~ ., 
                  data = ldata,  
                  method = "naive_bayes", 
                  trControl = trainControl(method = "none"),
                  tuneGrid = data.frame(nb_train$bestTune))

#             j) Save the model!
saveRDS(nb_final, "data/nb_final")

#             k) If your machine is running slow... read in the model 
nb_final <- readRDS("data/nb_final")

#             l) Predict from validation set
pred2 <- predict(nb_final, newdata = vdata)
#===========================================================================
#===========================================================================

head(pred2) # first few predictions

#             m) Evaluate confusion matrix (because we actually have labels...)
confusionMatrix(reference = as.factor(vdata$section_labels), data = pred2, mode='everything')


#################################################################################
## 4. Training a Support Vector Machine
# This time, you fill in the blanks based on the procedure we used for 
# Naive Bayes...
# You need to a) Examine parameters 
modelLookup(model = "svmLinear")

#             b) Create a grid
tuneGrid <- expand.grid(C = c(0.5, 1, 1.5))

#             c) Set up parallel processing
cl <- makePSOCKcluster(6) # using 6 clusters. 
registerDoParallel(cl)

#             d) Train the model
svm_train <- train(section_labels ~ .,   # use all variables (words) in model
                   data = Train,  
                   method = "svmLinear", 
                   metric = "F1",
                   trControl = train_control,
                   tuneGrid = tuneGrid,
                   allowParallel= TRUE
)

#             e) Save the model!
saveRDS(svm_train, "data/svm_train")

#             f) If your machine is running slow... read in the model
#svm_train <- readRDS("data/svm_train") 

#             g) Stop the cluster
stopCluster(cl)

#             h) Evaluate performance
print(svm_train)
pred_svm <- predict(svm_train, newdata = Test) # Predict on test sample using best model

##??
confusionMatrix(reference = as.factor(Test$section_labels), data = pred_svm, mode='everything')

#length(vdata$section_labels)
#length(pred_svm)

#             i) Finalise by training on all labelled data
svm_final <- train()
print(svm_final)

#             j) Save the model!
saveRDS(svm_final, "data/svm_final")

#             k) In case your computer is running slow... read in the model
#svm_final <- readRDS("data/svm_final")

#             l) Predict from validation set
svm_pred2 <- predict()

#             m) Evaluate confusion matrix
confusionMatrix()
