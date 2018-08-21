## Analyses based on filenames
# I DON'T THINK THIS WORKS
library(tidyverse)
library(ggplot2)

file_names <- list.files("./data/ISSPubs_txt/")


titles <- read.csv('/Users/katherinesimeon/Documents/GitHub/ISSResearch/data/topics.csv')

titles %>% 
  arrange(desc(year))

titles %>% 
  arrange(desc(year)) %>% 
  head(6)

# use regex to grab years
# note: pretty crude, gets any 4 digits
#   could refine with following/preceding 
#   the 4 digits with any non-number character?
years <- file_names %>% 
  stringr::str_extract(pattern = "[0-9]{4}") %>% 
  as.numeric() 


# Extracts first group of characters (partially)
# representing projects by matching any digit/letter
# up until the first punctuation character
projects <- file_names %>% 
  stringr::str_extract(pattern = "[:alnum:]+(?=[ -_])")

# make data frame and drop years that are outside
# plausible range
iss_research <- data.frame(project_name = projects,
                           year = years,
                           stringsAsFactors = FALSE) %>% 
  mutate(year = ifelse(year > 1960 & year < 2020,
                       year, 
                       NA)) %>% 
  tidyr::drop_na(year)


# count number of ocurrence and get some extra
# info (start / end year of project)
iss_research_summary <- iss_research %>% 
  group_by(project_name) %>% 
  summarise(n = n())


iss_research_summary_byYear <- iss_research %>% 
  group_by(project_name, year) %>% 
  summarise(n_years = n())

# bind number of ocurrences back to original
# data frame (useful for adding another layer to plot)
iss_research <- list(iss_research,
                     iss_research_summary,
                     iss_research_summary_byYear) %>% 
  purrr::reduce_right(left_join)


iss_research %>% 
  filter(n > 15) %>% 
  ggplot(aes(x = year,
             y = n_years,
             color = n)) +
  geom_density() +
  geom_point() +
  geom_jitter()
  # scale_size_continuous(breaks = c(10,20,40)) 

titles %>%
  group_by(topic_short,year) %>% 
  summarise(n_years = n()) %>% 
  filter(n > 15) %>% 
  ggplot(aes(x = year, y=n_years, colour = topic_short)) + 
  geom_point(group = topic_short)
