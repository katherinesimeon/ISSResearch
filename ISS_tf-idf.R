library(tidyverse)
library(readtext)
library(tidytext)

## Load Text Data

# Create list of files in a directory
DATA_DIR <- system.file("data/ISSPubs_txt", package = "readtext")

# Make a table with doc_id as each row, text as the second column 
text <- readtext(paste0(DATA_DIR, "data/ISSPubs_txt/*"))

# Make each row a word
by_word <- text %>% 
  unnest_tokens(word, text)

# View data
by_word
tail(by_word)

# Pull in stop word data
data("stop_words")

# Filter words that are not informative
by_word_clean <- by_word %>% 
  anti_join(stop_words)  %>% 
  filter(!word %in% c('t.co', 'https','de','el', 'en','^\"','0rg','ref','cite')) %>% 
  filter(!str_detect(word, "[0-9]"))

head(by_word_clean) # view data

# Get word count for each word by document
text_wc <- by_word_clean %>% 
  count(doc_id, word, sort = TRUE) 

text_wc # View data

# get total number of instances of each word, make it new column in table
total_words <- text_wc %>% group_by(doc_id) %>% summarize(total = sum(n))
text_by_doc <- left_join(text_wc, total_words)
text_by_doc

# Calculate tf-idf
text_calc <- text_by_doc %>%
  bind_tf_idf(word, doc_id, n)

text_calc %>% 
  arrange(desc(tf_idf,n))

# Graph first 30 rows of text_calc table
text_calc %>%
  arrange(desc(tf_idf)) %>%
  head(30) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(doc_id) %>% 
  top_n(50) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) +
  geom_point(size=2.7,aes(colour=doc_id),show.legend = FALSE) +
  labs(x = "Word/Token", y = "tf-idf") +
  coord_flip() +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_blank())
