# Function for cleaning
prep_toks <- function(text_corpus){
  toks <- tokens(text_corpus,
                 include_docvars = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("english"), padding = TRUE) %>%
    tokens_remove('[\\p{P}\\p{S}]', valuetype = 'regex', padding = TRUE)
  return(toks)
}

# Function for collocations
get_coll <-function(tokens){
  unsup_col <- textstat_collocations(tokens,
                                     method = "lambda",
                                     size = 2,
                                     min_count = 5,
                                     smoothing = 0.5)
  unsup_col <- unsup_col[order(-unsup_col$count),] # sort detected collocations by count (descending)
  return(unsup_col)
}
