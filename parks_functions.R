### Parks cleaner
library(tidyverse)
library(textdata)
library(tidytext)
library(thematic)
library(bslib)
library(stringr)
library(plotly)

library(showtext)
library(ragg)

#install.packages('fmsb')
library(fmsb)


#function to generate plotly for episode sentiments
# fill determines whether to use color for sentiment/imdb score
episode_bars <- function(season, fill = 'Sentiment'){
  fill_var <- case_when(fill == 'Sentiment' ~ 'sentiment', 
                        T ~ 'imdb_score')
  
  episode_data %>% 
    filter(Season == season) %>% 
    ggplot(aes(label = Title, x = Episode, y =1 ,fill = !!sym(fill_var),  text = description))+ 
    geom_col(show.legend = F) +
    scale_fill_distiller(palette = 'RdYlGn', direction = 0)+
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    labs(title = '', 
         x = '', 
         y = '')+
     theme_void()
  
  ggplotly(tooltip = c( 'label','x',  'fill'), height = 300)
  
}

#episode_bars(1)

#generates bar plotly for the whole series
full_series_bar <- function(){
  episode_data %>% 
    mutate(Season = as.factor(Season)) %>% 
    ggplot(aes(fill = Season, label = Episode, x = Overall_Episode,  text = Title, y = sentiment )) + 
    geom_col(show.legend = F) + 
    labs(title = 'Overall Sentiment per Episode of Parks and Recreation',
         x = 'Episode Number', 
         y = 'Overall Sentiment')+
    theme_void()
  ggplotly(tooltip = c('text', 'fill', 'label', 'y'))
}

#full_series_bar()

#lollipop chart showing characters who have the most lines per season
line_share_lollipop <- function(season){
  all_eps %>% 
    filter(Season == season) %>% 
    group_by(Character) %>%
    summarise(line_count = n()) %>%
    ungroup() %>% 
    mutate(lineshare = line_count / sum(line_count)) %>% 
    arrange(desc(lineshare)) %>% 
    mutate(Character = factor(Character, levels = Character[order(lineshare)])) %>% 
    head(15) %>% 
    ggplot(aes(x = Character, y = lineshare)) +
    geom_segment(aes(x = Character, xend = Character, y = 0, yend = lineshare), color ="#264653") + 
    geom_point(color = "#264653", size = 3, alpha = .6)+
    scale_y_continuous(labels = scales::percent)+
    coord_flip() + 
    labs(#title = paste('Percentage of Total Lines per Character: Season', season), 
      title = paste('Percentage of Total Lines: Season', season),
         x = '',
         y = '') +theme_void()
  ggplotly(height = 600) 
}

#line_share_lollipop(6)
#Shows top sentiments per character, per season
sentiment_lollipop <- function(char, season){
  
  #this block would allow for all seasons to be shown - did not make it into final version
  if(season != 0){
    data <- all_eps_tidy %>% filter(Season == season, 
                                    Character == char) 
  } else{
    data <- all_eps_tidy %>% filter(Character == char)
  }
  
  data %>% 
    inner_join(nrc) %>% 
    #inner_join(get_sentiments('nrc')) %>% 
    count(sentiment) %>% 
    ggplot(aes(x = sentiment, y = n)) +
    geom_segment(aes(x = sentiment, xend = sentiment, y = 0, yend = n), color = "#264653") + 
    geom_point(color = "#264653", size = 3, alpha = .6)+
    scale_y_continuous()+
    coord_flip() + 
    labs(title =  paste('Sentiment breakdown: ', char,  ', Season ', season, sep = ''), 
         x = '',
         y = '') + 
    theme_void()
  ggplotly(height = 300)
  
}

#sentiment_lollipop('Leslie Knope', 5)

#spider chart of same info as lollipop chart - did not make final version
sentiment_spider <- function(char, season){
  if(season != 0){
    data <- all_eps_tidy %>% filter(Season == season, 
                                    Character != 'Extra')
  } else data = all_eps_tidy %>% filter(Character != 'Extra')
  
  chars = data %>% 
    count(Character) %>% 
    arrange(desc(n)) %>% 
    head(10) %>% 
    pull(Character)
  
  data <- data %>% 
    filter(Character %in% chars) %>% 
    inner_join(get_sentiments('nrc')) %>% 
    count(Character, sentiment) %>% 
    pivot_wider(names_from = sentiment, values_from = n) %>% 
    mutate(across(where(is.integer), ~ as.double(.)))
  
  for(i in 1:nrow(data)) data[i,2:11] = (20.0 * data[i,2:11]) / sum(data[i,2:11])
  
  maxval = data %>% summarise(across(where(is.numeric), max))
  minval = data %>% summarise(across(where(is.numeric), min))
  
  rbind(maxval,minval,filter(nrc_top20,Character == char)[,-1] ) %>% 
    radarchart(pcol = '#3d405b', title = paste('Sentiment chart: ', char, ', Season ', season, sep = ''))
  
  
}


#sentiment_spider('Leslie Knope', 5)
#function to return top/bottom episode titles and descriptions
# when top = T and title = T, returns title of highest rated episode per season
# when top = F and title = F, returns description of lowest rated episode
episode_info <- function(season, top = T, title = T){
  data = episode_data %>% filter(Season == season)
  if(top){
    data = data %>% arrange(desc(imdb_score))
  }else{data %>% arrange(imdb_score)}
  
  if(title){return(data %>% head(1) %>% pull(Title))}
  else{return(data %>% head(1) %>% pull(description))}
}



#calculates percentage of positive token
char_positivity <- function(char, season){
  pos <- all_eps_tidy %>% 
    filter(Character == char,
           Season == season) %>% 
    inner_join(get_sentiments('bing')) %>% 
    summarise(positivity = as.character(round(100 * mean(sentiment == 'positive'), 1))) %>% 
    pull(positivity)
  return(paste(pos, '%', sep = ''))
}

#calculates number of lines per character per season
char_num_lines <- function(char, season){
  all_eps %>% 
    filter(Character == char,
           Season == season) %>% 
    tally() %>% 
    pull(n) %>% 
    as.character()
}

#calculates top sentiment per character per season
char_top_sentiment <- function(char, season){
  all_eps_tidy %>% 
    filter(Character == char,
           Season == season) %>% 
    inner_join(nrc) %>% 
    #inner_join(get_sentiments('nrc')) %>% 
    count(sentiment) %>% 
    slice_max(order_by = n, n = 1) %>% 
    pull(sentiment)
    
}

#gets path for character photo
char_photo <- function(char) {
  if (char %in% c('Bobby Newport', 'Jennifer Barkley')) {
    paste('www/', char, '.png', sep = '')
  }
  else{
    paste('www/', char, '.jpg', sep = '')
  }
}

