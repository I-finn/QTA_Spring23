#############################
# Tutorial 4: Supervised ML #
#############################

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

## 1. Acquire, read in and wrangle data
#gu_api_key() # run this interactive function
#dat <- gu_content(query = "Ukraine", from_date = "2022-01-01", to_date = "2022-07-01")

dat <- readRDS("data/df2023")
dat <- dat[dat$section_name %in% c("World news", "Opinion") 
           & dat$type == "article",]
dat <- dat %>%
  select(headline,
         byline,
         date = web_publication_date, # Rename date variable
         section_name,
         standfirst,
         body_text
  ) %>%
  mutate(date = as_datetime(date)) # parse date
dat <- dat[-which(duplicated(dat$headline)),]
dat$section_name <- ifelse(dat$section_name == "World news", "World", dat$section_name)

## 2. Preparation
dat$body_text <- str_replace(dat$body_text, "\u2022.+$", "")

corp <- corpus(dat, 
               docid_field = "headline",
               text_field = "body_text")

# Clean and make tokens
source("code/pre_processing.R")
prepped_toks <- prep_toks(corp) # basic token cleaning
collocations <- get_coll(prepped_toks) # get collocations
toks <- tokens_compound(prepped_toks, pattern = collocations[collocations$z > 10,]) # replace collocations
toks <- tokens_remove(tokens(toks), "") # let's also remove the whitespace placeholders

toks <- tokens(toks, 
               remove_numbers = TRUE,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_separators = TRUE,
               remove_url = TRUE) # remove other uninformative text

# Create dfm and weight using tf/idf
dfm <- dfm(toks) # create DFM
dfm <- dfm_trim(dfm, min_docfreq = 10) # trim DFM
dfm <- dfm_tfidf(dfm) # weight DFM

# Convert dfm to dataframe for ML
tmpdata <- convert(dfm, to = "data.frame", docvars = NULL)
tmpdata <- tmpdata[, -1] # drop document id variable (first variable)
section_labels <- dfm@docvars$section_name # get section labels - note, the @ operator is specific to S4 class object
tmpdata <- as.data.frame(cbind(section_labels, tmpdata)) # labelled data frame

## 3. ML Preparation
# You need to a) Create a 5% validation split
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

#             c) Create five-fold cross validation with 3 repeats object - to supply to train()
train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs= TRUE, 
  summaryFunction = multiClassSummary,
  selectionFunction = "best", # select the model with the best performance metric
  verboseIter = TRUE
)

## 4. Naive Bayes classification
# You need to a) Check the parameters for Naive Bayes algorithm
modelLookup(model = "naive_bayes")

#             b) Create a matrix of combinations of parameters to supply to tuneGrid arg of train()
tuneGrid <- expand.grid(laplace = c(0,0.5,1.0),
                        usekernel = c(TRUE, FALSE),
                        adjust=c(0.75, 1, 1.25, 1.5))

tuneGrid

#             c) Set up parallel processing
cl <- makePSOCKcluster(6) # create number of copies of R to run in parallel and communicate over sockets
# Note that the number of clusters depends on how many cores your machine has.  
registerDoParallel(cl) # register parallel backed with foreach package

#             d) Train the model
nb_train <- train(section_labels ~ ., 
                  data = Train,  
                  method = "naive_bayes", 
                  metric = "F1",
                  trControl = train_control,
                  tuneGrid = tuneGrid,
                  allowParallel= TRUE
)

#             e) Save the model!
saveRDS(nb_train, "data/nb_train")

#             f) If your machine is running slow... read in the model
#nb_train <- readRDS("data/nb_train")

#             g) Stop the cluster
stopCluster(cl) # stop parallel process once job is done

#             h) Evaluate performance
print(nb_train) # print cross-validation results
pred <- predict(nb_train, newdata = Test) # generate prediction on Test set using training set model
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
#nb_final <- readRDS("data/nb_final")

#             l) Predict from validation set
pred2 <- predict(nb_final, newdata = vdata)
head(pred2) # first few predictions

#             m) Evaluate confusion matrix (because we actually have labels...)
confusionMatrix(reference = as.factor(vdata$section_labels), data = pred2, mode='everything')

## 4. Training a Support Vector Machine
# You need to a) Examine parameters 
modelLookup(model = "svmLinear")

#             b) Create a grid
tuneGrid <- expand.grid(C = c(0.5, 1, 1.5))

#             c) Set up parallel processing
cl <- makePSOCKcluster(6) # using 6 clusters. 
registerDoParallel(cl)

#             d) Train the model
svm_train <- train(section_labels ~ ., 
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
confusionMatrix(reference = as.factor(Test$section_labels), data = pred_svm, mode='everything')

#             i) Finalise by training on all labelled data
svm_final <- train(section_labels ~ ., 
                   data = ldata,  
                   method = "svmLinear", 
                   trControl = trainControl(method = "none"),
                   tuneGrid = data.frame(svm_train$bestTune))
print(svm_final)

#             j) Save the model!
saveRDS(svm_final, "data/svm_final")

#             k) In case your computer is running slow... read in the model
#svm_final <- readRDS("data/svm_final")

#             l) Predict from validation set
svm_pred2 <- predict(svm_final, newdata = vdata)

#             m) Evaluate confusion matrix
confusionMatrix(reference = as.factor(vdata$section_labels), data = svm_pred2, mode='everything')
