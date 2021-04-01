rm(list=ls())

library(nfl4th)
library(tidyverse)
library(gt)
library(ggplot2)
library(ggrepel)
library(ggimage)

theme_set(  theme_classic() +
              theme(axis.title = element_text(size = 16),
                    axis.text = element_text(size = 14),
                    plot.title = element_text(size = 25, hjust = 0.5),
                    plot.subtitle = element_text(size = 14, hjust = 0.5),
                    plot.caption = element_text(size = 12),
                    legend.position = "none") 
)


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NFL 4th Down Go Rate"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        submitButton(text = "Change Team", icon = NULL, width = NULL),
        
        textInput("team_name", label = h3("Team Name"), value = "CHI"),
        h6("Enter a comma separated list of teams"),
        h6("IE: CHI,GB,DET"),
        
        width = 4
        
      ),
      
      # Show a plot 
      mainPanel(
        
        #fillPage(
          #tags$style(type = "text/css", "#Waterfall {height: calc(100vh - 80px) !important;}"),
          plotOutput("plot")
      
        # plotOutput("plot")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
     
     ##### 4th Down Go Rate Spaghetti Graphs #####
     TEAM = input$team_name
     
     highlight_names <- TEAM %>% strsplit(",") %>% as.vector()
     highlight_names <- highlight_names[[1]]
     
     for (i in 1:length(highlight_names)) {
       highlight_names[[i]] <- str_trim(highlight_names[[i]])
     }
     
     


     
     all_dec <- read.csv("nfl4th_all_dec_2014_2020.csv") %>% 
       as_tibble() %>% 
       mutate(team_logo_espn = ifelse(posteam %in% highlight_names,as.character(team_logo_espn),as.character(NULL)))

     
     
     all_dec %>% 
       ggplot(aes(x = season, y = go_rate, group = posteam)) +
       geom_smooth(group=1,
                   color = 'red',
                   linetype = 'dashed',
                   size = 1) +
       geom_path(aes(x = season, y = go_rate), 
                 color = all_dec$team_color,
                 alpha = ifelse(all_dec$posteam %in% highlight_names,7/8,1/8),
                 size = ifelse(all_dec$posteam %in% highlight_names,1,.5)
       ) + 
       geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
       geom_text_repel(label = ifelse(all_dec$posteam %in% highlight_names & all_dec$season == 2014,
                                as.character(all_dec$posteam),'')) +
       geom_hline(yintercept = .5,color='red',linetype='dotted') + 
       labs(
         title = 'Fourth Down Go Rate',
         subtitle = paste0(TEAM,', 2014 - 2020, Go Boost > 1'),
         x = 'Season',
         y = 'Go Rate',
         caption = "@data_bears (Data from nflfastR + Ben Baldwin's nfl4th Model)"
       ) 
     
   },height=650,width=800)
}

# Run the application 
shinyApp(ui = ui, server = server)

