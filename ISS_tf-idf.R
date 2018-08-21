library(tidyverse)
library(readtext)
library(tidytext)
library(

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
  filter(!str_detect(word, "[0-9]")) %>% 
  filter(nchar(word)>5)

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

# Graph first 50 rows of text_calc table
text_calc %>%
  arrange(desc(tf_idf)) %>%
  head(50) %>% # change to set number of words on y axis
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(doc_id) %>% 
  top_n(50) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) +
  geom_point(size=2.7,aes(colour=doc_id),show.legend = FALSE) +
  labs(x = "Word", y = "tf-idf") +
  coord_flip() +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


# Oldest Article
a1984 <- text_calc %>%
  filter(doc_id == "Kikuchi-Kossel_Yoshiyama_1984[related].pdf.txt") %>% 
  arrange(desc(tf_idf)) %>%
  head(50) %>% # change to set number of words on y axis
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  # group_by(doc_id) %>% 
  #top_n(50) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = word)) +
  geom_point(size=2.7,aes(colour=word),show.legend = FALSE) +
  labs(x = "Word", y = "tf-idf") +
  ggtitle("Kikuchi-Kossel_Yoshiyama_1984") +
  coord_flip() +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

# Newest article
a2017 <- text_calc %>%
  filter(doc_id %in% c("ARED_CEVIS_IRED_TVIS_Moore_2017[related].pdf.txt","ARED_CEVIS_TVIS_Moore_2017[related].pdf.txt","ARED_Fregly_2012[related].pdf.txt")) %>% 
  arrange(desc(tf_idf)) %>%
  head(50) %>% # change to set number of words on y axis
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  # group_by(doc_id) %>% 
  #top_n(50) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = word)) +
  geom_point(size=2.7,aes(colour=word),show.legend = FALSE) +
  labs(x = "", y = "tf-idf") +
  ggtitle("ARED 2017") +
  coord_flip() +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) 

# Patchwork to put oldest and newest article together
a1984 + a2017

############### 




# get total number of instances of each word, make it new column in table
text_calc_count <- text_calc %>% 
  count(word, sort=TRUE) %>% 
  filter(nn > 1000) 
tail(text_calc_count)

