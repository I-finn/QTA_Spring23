dat <- gu_content(query = "Ukraine", from_date = "2023-01-01")

df <- df[df$type == "article" & df$section_id == "world",]

corpus_ukr <- corpus(df, 
                     docid_field = "web_title", 
                     text_field = "body_text")

corpus_ukr <- stri_replace_first(corpus_ukr, 
                                 replacement = "",
                                 regex = "^.+?\"")

corpus_ukr <- stri_replace_last(corpus_ukr, 
                  replacement = "",
                  regex = "\u2022.+$")
