#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

grid_main = grid_template(
    default = list(areas = rbind(c('l1', 'm1', 'r'), 
                                 c('l2', 'm2', 'r'), 
                                 c('b', 'b', 'b')), 
                   rows_height = c('auto', 'auto', '20%'), 
                   cols_width = c('30%', '30%', '40%'))
)

library(shiny)
library(shiny.semantic)



# Define UI for application that draws a histogram
ui <- semanticPage(

    # Application title
    title = "Parks and Recreation" ,
    h1('Parks and Recreation Sentiment Analysis'), 
    
    main_panel(
        grid(grid_main, 
             area_styles = list(l1 = 'margin: 20px', l2 = 'margin: 20px', m1 = 'margin: 20px',
                                l2 = 'margin: 20px', b = 'margin: 20px'),
             l1 = card(class = 'character info', 
                       div(class = 'header', 'Leslie Knope'),
                       img(src = 'LeslieKnope.png')), 
             l2 = img(src = 'LeslieKnope.png'),
             m1 = plotOutput('Lineshare'),
             m2 = plotOutput('SentLollipop'),
             r = card(class = 'plot', 
                      plotlyOutput('EpisodeBars')), 
             b = plotlyOutput('FullSeries'))
    )

    # fluidRow(column(width = 4, img(src = 'LeslieKnope.png')),
    #          column(width = 4, plotOutput('Lineshare')), 
    #          column(width = 4, plotlyOutput('EpisodeBars'))), 
    # fluidRow(plotlyOutput('FullSeries'))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$FullSeries <- renderPlotly({full_series_bar()})
    
    output$Lineshare <- renderPlot({line_share_lollipop(4)})
    
    output$SentLollipop <- renderPlot({sentiment_lollipop('Leslie Knope', 4)})
    
    output$EpisodeBars <- renderPlotly({episode_bars(4)})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
