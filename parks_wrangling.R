#Data wrangling
#Parks and recreation data!
library(tidyverse)
library(textdata)
library(tidytext)
library(stringr)
library(plotly)

#starting with script data
#Loops through folder reading csv files of each episode
#Note - season 6 opener and finale are both two part episodes - I want to break them apart, so I found an episode break point

seasons = 1:7
episodes = c(6,24, 16, 22,22,20,12)

#setwd('fakepath/scripts')

all_eps = data.frame()

overall_ep = 0

for(sn in seasons){
  for(ep in 1:episodes[sn]){
    eps = paste(ep)
    if(ep < 10) eps = paste('0', ep, sep = '')
    df <- read_csv(paste('s', sn, 'e', eps, '.csv', sep = ''))
    overall_ep = overall_ep + 1
    all_eps = rbind(all_eps, cbind(df, 'Season' = sn, 'Episode' = ep, 'Overall_Episode' = overall_ep))
  }
}

s6e1break = 48735
s6e21break = 58963
s7break = 59365
end_line = 65942
  
all_eps[s6e1break:end_line,5] = all_eps[s6e1break:end_line,5] + 1
all_eps[s6e21break:end_line, 5] = all_eps[s6e21break:end_line, 5] + 1

all_eps[s6e1break:s7break,4] = all_eps[s6e1break:s7break,4] + 1
all_eps[s6e21break:s7break,4] = all_eps[s6e21break:s7break,4] + 1


all_eps %>% group_by(Season) %>% summarise(n_distinct(Episode))

episodes = c(6,24, 16, 22,22,22,12)

## next is the imdb data
## scraping from imdb seasons page, getting episode titles, ratings, descriptions

imdb_df = data.frame()

for(sn in seasons){
  page <- read_html(paste('https://www.imdb.com/title/tt1266020/episodes?season=', sn, sep = ''))
  nodes = html_nodes(page, '.info')
  text = html_text(nodes)
  text <- gsub('\n', '', text)
  text <- gsub('  ', '', text)
  descriptions <- word(text, 2, sep = fixed('please try again.'))
  ttl <- word(text, 1, sep = fixed('('))
  scores <- as.numeric(str_sub(ttl, -3,-1))
  titles <- word(ttl, 2, sep = '20\\d\\d') %>% str_sub(1,-4)
  df <- data.frame('Season' = rep(sn, episodes[sn]), 'Episode' = 1:episodes[sn], 'Title' = titles, 'imdb_score' = scores, 'description' = descriptions)
  imdb_df = rbind(imdb_df, df)
}

#Build episode data, merged with imdb_df


all_eps_tidy <- all_eps %>% 
  group_by(Season, Episode) %>% 
  mutate(line_num = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word, Line)

episode_data <- all_eps_tidy %>% 
  inner_join(get_sentiments('bing')) %>% 
  count(Season, Episode, Overall_Episode, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  mutate(sentiment = positive - negative)  %>% 
  inner_join(imdb_df, by = c('Season', 'Episode')) 

episode_data$Title[112] = '2017' #This got lost in the string value trimming due to confusion with date

top_chars <- all_eps %>% 
  group_by(Season,Character) %>%
  filter(Character != 'Extra') %>% 
  summarise(line_count = n())%>% 
  arrange(Season, desc(line_count)) %>% 
  ungroup() %>% 
  group_by(Season) %>% 
  slice_head( n = 15)

#characters that aren't to be included
exclude = c('Lawrence', 'Janine Restrepo', 'Paul', 'Kate Spivack', 'Kate Spivack', 'Raul', 
            'Kelly Larson', 'Lindsay Carlisle Shay', 'Dennis Feinstein', 
            'Nadia Stasky', 'Dr. Saperstein', 'Roscoe', 'Lucy', 'Chris')

top_chars = top_chars %>% filter(!(Character %in% exclude))

#top_chars %>% ungroup() %>% distinct(Character) %>% View()

#clear out some unnecessary vars
rm(df, imdb_df, nodes, page, descriptions, end_line, episodes, s6e1break, s6e21break, s7break, 
   scores, sn, text, titles, ttl)
