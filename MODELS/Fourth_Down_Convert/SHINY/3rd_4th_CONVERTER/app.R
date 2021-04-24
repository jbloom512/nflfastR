rm(list=ls())

library(nfl4th)
library(tidyverse)
library(gt)
library(ggplot2)
library(ggrepel)
library(ggimage)

theme_set(  theme_classic() +
              theme(axis.title = element_text(size = 14),
                    axis.text = element_text(size = 14),
                    plot.title = element_text(size = 22, hjust = 0.5),
                    plot.subtitle = element_text(size = 14, hjust = 0.5),
                    plot.caption = element_text(size = 12),
                    legend.position = "none",
                    axis.title.y=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank()) 
)

test_preds <- read.csv("2018_2020_GLM_FDOE.csv")


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NFL First Downs Over Expected on 3rd/4th Down Attempts"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        submitButton(text = "Update", icon = NULL, width = NULL),
        
        
        # Copy the chunk below to make a group of checkboxes
        radioButtons("model", label = h5("Model"), 
                           choices = list('GLM','RF'),
                           selected = 'GLM'),
        
        
        # Copy the chunk below to make a group of checkboxes
        checkboxGroupInput("seasons", label = h5("Seasons"), 
                           choices = list(2018,2019,2020),
                           selected = list(2018,2019,2020)),
        
        # Copy the line below to make a slider range 
        sliderInput("weeks", label = h3("Week Range"), min = 1, 
                    max = 21, value = c(1,17)),
        
        
        # Copy the chunk below to make a group of checkboxes
        checkboxGroupInput("quarters", label = h5("Quarters"), 
                           choices = list(1,2,3,4,5),
                           selected = list(1,2,3,4,5)),
        
        # Copy the chunk below to make a group of checkboxes
        checkboxGroupInput("downs", label = h5("Downs"), 
                           choices = list(3,4),
                           selected = list(3,4)),
        width = 2
        
      ),
      
      # Show a plot 
      mainPanel(

          plotOutput("plot")
          
      
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
     
     ##### Graph Parameters #####
     quarters = input$quarters
     
     seasons = input$seasons
     
     weeks = input$weeks
     min_week <- min(weeks)
     max_week <- max(weeks)
     
     downs = input$downs
     
     model = input$model
     
     ## For fdoe calculations
     model_fdoe <- paste0('fdoe_',str_to_lower(model))
     
     ## For Caption
     min_season <- min(seasons)
     max_season <- max(seasons)
   
     
     
     ## View Teams by FDOE
     plt <- test_preds %>% 
       filter(qtr %in% quarters,
              season %in% seasons,
              week >= min_week & week <= max_week,
              down %in% downs) %>% 
       group_by(posteam,team_color,team_logo_espn) %>% 
       summarise(
         total_fdoe = sum(!!sym(model_fdoe)),
         avg_fdoe =  mean(!!sym(model_fdoe))
       ) %>% 
       arrange(desc(total_fdoe)) %>% 
       ungroup()
     
     ## Create Plot
     plt %>% 
       ggplot(aes(x = reorder(posteam,total_fdoe), y = total_fdoe)) + 
       geom_bar(stat='identity',fill = plt$team_color,width = .7) +
       geom_image(aes(x=reorder(posteam,total_fdoe),y=total_fdoe,image = team_logo_espn),asp = 16/9,size=.027) +
       labs(
         title = 'First Downs Over Expected',
         subtitle = paste0(min_season,' - ',max_season,', Weeks ',min_week,' - ',max_week,', Quarters - ',toString(quarters),', Downs - ',toString(downs),', Rush + Pass Plays, FDOE calculated from ',model,' Model'),
         caption = "Joey Bloom - @data_bears (Data - nflfastR)",
         y = "FDOE"
       ) +
       coord_flip() 
     
     
   },height = 660, width = 925)
}

# Run the application 
shinyApp(ui = ui, server = server)

