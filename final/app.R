# Parks app v1.0

library(shiny)
library(tidyverse)
library(stringr)
library(ggthemes)
library(plotly)
library(tidytext)
library(textdata)
library(bslib)

# library(textshaping)
# library(showtext)
# library(ragg)

#install.packages('fmsb')
#library(fmsb)

# font_add_google('montserrat')
# bs_theme_update(theme, bg = "#F5F4ED", primary = "#264653",
#                 secondary = "#E9c46a", success = "#2a9d8f", info = "#8789c0",
#                 danger = "#bb4430", base_font = font_google("Montserrat"),
#                 fg = "#000")



#setwd("~/MSA 21/Side Work/Parks/final")
# load("app_run_data.RData")

source('./global.R')

thematic_on(bg = "#F5F4ED", fg = 'auto',accent = "#2a9d8f", font = "montserrat")
onStop(thematic_off)


ui <- fluidPage(
  theme = bs_theme( bg = "#ffffff", primary = "#264653",
                   secondary = "#E9c46a", success = "#2a9d8f", info = "#8789c0",
                   danger = "#bb4430", base_font = font_google("Montserrat"),
                   fg = "#000"),
  titlePanel('Parks and Recreation Sentiment'), 
  
  mainPanel(width = 12,
    fluidRow(column(2,selectInput('season', 'Season:', 1:7)), 
             column(2,  uiOutput('charbox')),#selectInput('character', 'Character: ', charlist, selected = 'Leslie Knope')),
             column(4,selectInput('episode_measure', 'Color:', c('Sentiment', 'IMDB Score'))),
             column(4, h3('Season Info'))
    ), 
    
    fluidRow(class = 'myRow1',
             column(width = 4, 
                    #fluidRow(box(img(src = 'LeslieKnope.png'))),
                    fluidRow(class = 'myRow2',
                             column(6,h3(textOutput('char_name')),
                                    h5(textOutput('char_positivity')),
                                    h5('Number of lines:'),
                                    p(textOutput('char_num_lines')), 
                                    h5('Top Sentiment:'),
                                    p(textOutput('char_top_sentiment'))),
                             column(6, 
                                    imageOutput('char_image'))
                    ),
                    fluidRow(plotlyOutput('sent_lollipop'))), 
             column(width = 4, 
                    fluidRow(class = 'myRow2',
                             plotlyOutput('episodes')),
                    fluidRow(h5('Highest Rated Episode (IMDB):', textOutput('top_title')), 
                             p(textOutput('top_desc'))),
                    fluidRow(h5('Lowest Rated Episode (IMDB):', textOutput('bottom_title')), 
                             p(textOutput('bottom_desc')))), 
             column(width = 4, 
                    fluidRow(class = 'myRow1', plotlyOutput('lineshare')),
                    )
    ), 
    
    fluidRow(class = 'myRow2',
             plotlyOutput('fullseries')
    ), 
    fluidRow(column(6,
      h4('Reference'),
      p(
        'This app was created using the ',
        tags$a(
          href = 'http://saifmohammad.com/WebPages/lexicons.html',
          'NRC Emotion Lexicon, created by Saif M. Mohammad and Peter D. Turney'
        ),
        ', and the ',
        tags$a(href = 'https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html', 'Bing Lexicon, created by Bing Liu and Minqing Hu.')
      )
    )), 
    tags$head(tags$style(
      '.myRow1{height:600px;}
           .myRow2{height:300px;}'
    ))
    
    
  )
)

server <- function(input, output){
  
  output$charbox <- renderUI({
    charlist = top_chars %>% filter(Season == input$season) %>% pull(Character)
    selectInput('character', 'Character', charlist, selected = 'Leslie Knope')
  })
  
  output$fullseries <- renderPlotly({full_series_bar() %>% layout(height = 300)})
  
  output$lineshare <- renderPlotly({line_share_lollipop(input$season)})
  
  output$sent_lollipop <- renderPlotly({sentiment_lollipop(input$character, input$season)})
  
  output$episodes <- renderPlotly({episode_bars(input$season, fill = input$episode_measure)})
  
  output$char_image <- renderImage(list(src = char_photo(input$character), height = 270), deleteFile = F)
  
  output$char_name <- renderText({input$character})
  
  output$char_positivity <- renderText({paste('Positivity: ', char_positivity(input$character, input$season), sep = '')})
  
  output$char_num_lines <- renderText({char_num_lines(input$character, input$season)})
  
  output$char_top_sentiment <- renderText({char_top_sentiment(input$character, input$season)})
  
  output$top_title <- renderText({episode_info(input$season)})
  output$bottom_title <- renderText({episode_info(input$season, F, T)})
  output$top_desc <- renderText({episode_info(input$season, T, F)})
  output$bottom_desc <- renderText({episode_info(input$season, F, F)})
}

shinyApp(ui, server)