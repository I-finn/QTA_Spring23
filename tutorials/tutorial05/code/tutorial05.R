###############################
# Tutorial 5: Unsupervised ML #
###############################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
getwd()

# remove objects
rm(list=ls())

## Load packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Acquire stmBrowser package from github
if(!require(devtools)) install.packages("devtools")
library(devtools)
#install_github("mroberts/stmBrowser",dependencies=TRUE)

lapply(c("tidyverse",
         "quanteda",
         "quanteda.textstats",
         "lubridate",
         "stm",
         "wordcloud",
         "stmBrowser",
         "LDAvis"),
       pkgTest)

## 1. Read in and wrangle data
#     a) In the data folder you'll find a large data.frame object called 
#        ukr_h1_2022. Read it in, and check the type of articles it contains.
dat <- readRDS("data/ukr_h1_2022")


View(dat)
#     b) Pre-process the data.frame.
source("code/pre_processing.R")
dat$section_name <- ifelse(dat$section_name == "World news", "World", 
                           dat$section_name)

unique(dat$section_name)

which(duplicated(dat$headline))
# no duplicates
# We can use the same code to drop duplicated headlines:
#dat <- dat[-which(duplicated(dat$headline)),]
#  ?duplicated(x, fromLast = "TRUE") - reverse search

## 2. QTA Preparation


# You need to a) Remove the large round symbol.
# Let's also tidy the body_text column before we transform into a corpus
dat$body_text <- str_replace(dat$body_text, "\u2022.+$", "")

# want to exclude 'briefing' - too much noise - crosswords, etc

dat <- dat[-str_detect(dat$headline, "briefing"),]

head(dat)

corp22 <- corpus(dat, 
                 docid_field = "headline",
                 text_field = "body_text")


toks<-prep_toks(corp22)
colls<-get_coll(toks)

toks <- tokens_compound(toks, pattern = colls[colls$z > 10,])
toks <- tokens_remove(quanteda::tokens(toks), "") 
toks <- tokens(toks, remove_punct = TRUE, 
               remove_symbols = TRUE, 
               remove_numbers = TRUE , 
               remove_url = TRUE, 
               remove_separators = TRUE)

# iterate
extra_stopwords=c("said", "one", "also", "now", "say", 
#                  "russian", "ukrainian", "ukraine", "russia",
                  valuetype="fixed")
toks <- tokens_remove(toks, extra_stopwords)


# Create the dfm.
dfm <- dfm(toks)


#getwd()  # try docfreq = 50
dfm <- dfm_trim(dfm, min_docfreq = 20)

topfeatures(dfm)

## 2. Perform STM 
# Convert dfm to stm
stmdfm <- convert(dfm, to = "stm")

# Set k
# Constantine - tutorial on dropbox - 30 clusters
# k = number of categories/clusters that we think will be in our data
# lower k = less computation

K <- 8
rm(colls, dat, toks, corp22)
# prevalence = topics vary over matrix of categories 
# - conditional probability /a priori 
# source/section_name = meta data which we use to control for expected differences
# s() = smoothing function

# Run STM algorithm
modelFit <- stm(documents = stmdfm$documents,
                vocab = stmdfm$vocab,
                K = K,
                prevalence = ~ section_name + s(month(date)),
                #prevalence = ~ source + s(as.numeric(date_month)), 
                data = stmdfm$meta,
                max.em.its = 500,
                init.type = "Spectral",
                seed = 2023,
                verbose = TRUE)

# Save your model!
saveRDS(modelFit, "data/modelFit")

# Load model (in case your computer is running slow...)
#modelFit <- readRDS("data/modelFit")

## 3. Interpret Topic model 
# Inspect most probable terms in each topic
labelTopics(modelFit)
# H Prob - most common words - less useful
# FREX - frequency 
# Lift - 
# Score


# Further interpretation: plotting frequent terms
plot.STM(modelFit, 
         type = "summary", 
         labeltype = "frex", # plot according to FREX metric
         text.cex = 0.7,
         main = "Topic prevalence and top terms")

plot.STM(modelFit, 
         type = "summary", 
         labeltype = "prob", # plot according to probability
         text.cex = 0.7,
         main = "Topic prevalence and top terms")

# Use wordcloud to visualise top terms per topic
cloud(modelFit,
      topic = 1,
      scale = c(2.5, 0.3),
      max.words = 50)

# Reading documents with high probability topics: the findThoughts() function
# standfirst = guardian's summary of the articles
findThoughts(modelFit,
             # If you include the original corpus text, we could refer to this here
             texts = dfm@docvars$standfirst, 
             topics = 5,
             n = 10)

names(dfm@docvars)
#can retrieve any docvar from dfm

## 4. Topic validation: predictive validity using time series data
#     a) Convert metadata to correct format
#stmdfm$meta$num_month <- as.numeric(stmdfm$meta$date_month)
stmdfm$meta$num_month <- month(stmdfm$meta$date)

#     b) Aggregate topic probability by month
agg_theta <- setNames(aggregate(modelFit$theta,
                                by = list(month = stmdfm$meta$num_month),
                                FUN = mean),
                      c("month", paste("Topic",1:K)))
agg_theta <- pivot_longer(agg_theta, cols = starts_with("T"))

#     c) Plot aggregated theta over time
ggplot(data = agg_theta,
       aes(x = month, y = value, group = name)) +
  geom_smooth(aes(colour = name), se = FALSE) +
  labs(title = "Topic prevalence",
       x = "Month",
       y = "Average monthly topic probability") + 
  theme_minimal()

## 5. Semantic validation (topic correlations)
topic_correlations <- topicCorr(modelFit)
plot.topicCorr(topic_correlations,
               # we could change this to a vector of meaningful labels
               vlabels = seq(1:ncol(modelFit$theta)), 
               vertex.color = "white",
               main = "Topic correlations")

# measures of quality of model clustering - want high for both
# use to id problems

# use to deselect documents - need theoretical argument for dropping 

## 6. Topic quality (semantic coherence and exclusivity)
topicQuality(model = modelFit,
             documents = stmdfm$documents,
             xlab = "Semantic Coherence",
             ylab = "Exclusivity",
             labels = 1:ncol(modelFit$theta),
             M = 15)

# An alternative approach, using underlying functions
SemEx <- as.data.frame(cbind(c(1:ncol(modelFit$theta)), 
                             exclusivity(modelFit),
                             semanticCoherence(model = modelFit,
                                               documents = stmdfm$documents,
                                               M = 15)))

names(SemEx)
colnames(SemEx) <- c("k", "ex", "coh")
# error
SemExPlot <- ggplot(SemEx, aes(coh, ex)) +
  geom_text(aes(label=k)) +
  labs(x = "Semantic Coherence",
       y = "Exclusivity",
       title = "Topic Semantic Coherence vs. Exclusivity") +
  geom_rug() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "gray", linewidth=1))
SemExPlot

# Inspect outliers
labelTopics(modelFit,
            topics = c(6,3))

## 7. Extended visualisations
# opens in browser
#     a) Using LDAvis
toLDAvis(mod = modelFit,
         docs = stmdfm$documents,
         open.browser = interactive(),
         reorder.topics = TRUE)

#     b) Using stmBrowser
#        Warning: this will (silently) change your working directory!
stmBrowser(mod = modelFit,
           data = stmdfm$meta,
           covariates = c("section_name", "num_month"),
           text = "standfirst",
           n = 1000)

## 8. Estimating covariate effects
#     a) Calculate
estprop <- estimateEffect(formula = c(1:ncol(modelFit$theta)) ~ section_name + s(num_month),
                          modelFit,
                          metadata = stmdfm$meta,
                          uncertainty = "Global",
                          nsims = 25)

summary(estprop)


#     b) Plot topic probability differences
custom_labels <- seq(1:K)
# shows split of topics between categories - one is a reference value - check
plot.estimateEffect(x = estprop,
                    #model = modelFit,
                    method = "difference",
                    covariate = "section_name",
                    cov.value1 = "World",
                    cov.value2 = "Opinion",
                    topics = estprop$topics,
                    #xlim = c(-.05, .05),
                    #labeltype = "custom",
                    #custom.labels = custom_labels
                    )

#     c) Plot topic probability over time (similar to above plot in 4.)
plot.estimateEffect(x = estprop,
                    #model = modelFit,
                    method = "continuous",
                    covariate = "num_month",
                    topics = estprop$topics,
                    #xlim = c(-.05, .05),
                    labeltype = "custom",
                    custom.labels = custom_labels,
                    printlegend = T) # Toggle this to see January

## 9. Using data to determine k
?searchK
kResult <- searchK(documents = stmdfm$documents,
                   vocab = stmdfm$vocab,
                   K=c(4:7),
                   init.type = "Spectral",
                   data = stmdfm$meta,
                   prevalence = ~ section_name + s(month(date)))
                   #cores = 4) # This no longer works on windows 10 :(

saveRDS(kResult, "data/kResult")
#kResult <- readRDS("data/kResult")                   
plot(kResult)

## 10. Refine
# Now that we've run the model and evaluated the output, try to refine
# it for yourselves: can we further trim the tokens? Are there any cases
# we should drop from our corpus? (For instance, the daily digest) How
# large should we set our k? 