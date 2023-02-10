corpSum22$fk <- textstat_readability(corp22, measure = "Flesch.Kincaid")
corpSum23$fk <- textstat_readability(corp23, measure = "Flesch.Kincaid")

grepl("Luke Harding|Jennifer Rankin|Julian Borger", 
      corpSum22$byline, ignore.case = TRUE)

corpSum22 %>%
  filter(grepl("Luke Harding|Jennifer Rankin|Julian Borger", 
        byline, ignore.case = TRUE)) %>%
  group_by(grp = str_extract(byline, "Luke Harding|Jennifer Rankin|Julian Borger")) %>%
  summarise(av = mean(fk$Flesch.Kincaid)) %>%
  ggplot(aes(x = reorder(grp, -av), y = av)) +
  geom_col() +
  ggtitle(label = "FK Readability by Correspondent") +
  xlab(label = NULL) +
  ylab("Mean Flesch-Kincaid")

corpSum23 %>%
  filter(grepl("Luke Harding|Jennifer Rankin|Julian Borger", 
               byline, ignore.case = TRUE)) %>%
  group_by(grp = str_extract(byline, "Luke Harding|Jennifer Rankin|Julian Borger")) %>%
  summarise(av = mean(fk$Flesch.Kincaid)) %>%
  ggplot(aes(x = reorder(grp, -av), y = av)) +
  geom_col() +
  ggtitle(label = "FK Readability by Correspondent") +
  xlab(label = NULL) +
  ylab("Mean Flesch-Kincaid")

summary(colc22$z) # Look at the z scores and see what cut-off to use
toks22 <- tokens_compound(toks22, pattern = colc22$collocation[colc22$z > 17.5])
toks23 <- tokens_compound(toks23, pattern = colc23$collocation[colc23$z > 16])

dfm22 <- dfm(toks22)
dfm23 <- dfm(toks23)
topfeatures(dfm22)
topfeatures(dfm23)

toks22 <- tokens_remove(toks22, c("said",
                                  "say",
                                  "also",
                                  "ukrain",
                                  "ukrainian",
                                  "russia",
                                  "russian"),
                        valuetype = "fixed")
toks23 <- tokens_remove(toks23, c("said",
                                  "say",
                                  "also",
                                  "ukrain",
                                  "ukrainian",
                                  "russia",
                                  "russian"),
                        valuetype = "fixed")
