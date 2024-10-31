#multiracial data exploration

load("data/OMB2023_final.RData")

OMB2023 <- OMB2023 %>%
  select(CommentID,Comment,Campaign)

# creating corpus of the original comments
doc.corpus <- corpus(OMB2023$Comment, docnames = OMB2023$CommentID)

summary(doc.corpus)

data_tokens <- tokens(
  doc.corpus, 
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_wordstem() %>%
  tokens_remove(
    c(stopwords("english"),
      "may", "shall", "can",
      "must", "upon", "with", "without"
    )
  )  %>%
  tokens_select(min_nchar = 3)

data_dfm <- dfm(data_tokens) %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 2)

dim(data_dfm)

tokensdf <- convert(data_dfm, to = "data.frame")

multiracial <- tokensdf %>% 
  select(doc_id, mix,`mixed-rac`,mixtur,`multi-ethn`, `multi-raci`, multiraci) %>%
  mutate(sum = rowSums(.[, 2:length(.)], na.rm = TRUE)) %>%
  filter(sum > 0) %>%
  mutate(multiracial = 1) %>%
  select(doc_id, multiracial)

multiracial <- multiracial %>%
  rename("CommentID" = "doc_id")

OMB2023 <- OMB2023 %>%
  left_join(multiracial, by = "CommentID")

OMB2023 <- OMB2023 %>%
  filter(multiracial %in% 1) %>%
  select(-Campaign)

save(OMB2023, file = "OMB2023_multiracial.csv")

  
  