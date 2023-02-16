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

## 1. Acquire (?), read in and wrangle data
dat <- readRDS()

# You need to a) Subset on section_name, using World news and Opinion, and 
#                type, using article.

#             b) Select relevant columns.

#             c) Remove duplicates.

# This code relabels our data, because "World news" contains whitespace...
dat$section_name <- ifelse(dat$section_name == "World news", "World", dat$section_name)

## 2. QTA Preparation
# You need to a) Remove the large round symbol.

#             b) Convert to a corpus.

#             c) and d) Clean the corpus and find collocations.

# For steps c) and d), check out the pre_processing.R script.
source("code/pre_processing.R")
prepped_toks <- prep_toks(corp) # basic token cleaning
collocations <- get_coll(prepped_toks) # get collocations

#             e) Make tokens.

#             f) Clean tokens.

#             g) Create the dfm.

#             h) Trim and weight the dfm
dfm <- dfm_trim(dfm, min_docfreq = 10) # trim DFM
dfm <- dfm_tfidf(dfm) # weight DFM

#             i) Convert dfm to dataframe for ML
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
svm_train <- train(
)

#             e) Save the model!
saveRDS(svm_train, "data/svm_train")

#             f) If your machine is running slow... read in the model
#svm_train <- readRDS("data/svm_train") 

#             g) Stop the cluster
stopCluster(cl)

#             h) Evaluate performance
print(svm_train)
pred_svm <- predict() # Predict on test sample using best model
confusionMatrix()

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
