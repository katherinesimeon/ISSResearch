library(readtext)
library(tidytext)

## Load Text Data
setwd('/Users/katherinesimeon/Documents/GitHub/ISSResearch/data/ISSPubs_txt/')

filelist <- list.files(path="/Users/katherinesimeon/Documents/GitHub/ISSResearch/data/ISSPubs_txt",all.files=TRUE)

lapply(filelist, FUN=readtext, header=TRUE)

DATA_DIR <- system.file("data/ISSPubs_txt", package = "readtext")

text <- readtext(paste0(DATA_DIR, "data/ISSPubs_txt/*"))

by_word <- text %>% 
  unnest_tokens(word, text)

by_word
tail(by_word)

data("stop_words")

by_word_clean <- by_word %>% 
  anti_join(stop_words)  %>% 
  filter(!word %in% c('t.co', 'https','de','el', 'en','^\"','0rg','^[0-9]{1}$'))

head(by_word_clean)

text_wc <- by_word_clean %>% 
  count(doc_id, word, sort = TRUE) 

head(text_wc)
