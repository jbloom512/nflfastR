rm(list=ls())

library(tidyverse)
library(baseballr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(shiny)


## Define UI for application that draws a histogram ##
ui <- fluidPage(

  
   ## Application title ##
   titlePanel("MLB Pitcher Spin Rate"),
   
   ## Sidebar with a slider input for number of bins ##
   sidebarLayout(

      sidebarPanel(
        
        
        ## Update button to update the dashboard ##
        submitButton(text = "Update Chart", icon = NULL, width = NULL),

        ## Pitch name selection ##
        textInput("pitcher_name", label = h5("Pitcher Name"), value = "Lucas Giolito"),
        h6("Enter a Pitchers full name"),

        
        ## Start date ##
        textInput("start_date", label = h5("Start Date"), value = Sys.Date() - 31),
        h6("Enter a start date - YYYY-MM-DD"),
         
        ## End date ##
        textInput("end_date", label = h5("End Date"), value = Sys.Date()),
        h6("Enter an end date - YYYY-MM-DD"),
        
        
        ## Pitch Types ##
        checkboxGroupInput("pitch_type", label = h5("Pitch Type"), 
                           choices = list('Slider'='SL',
                                          '4-Seam Fastball'='FF',
                                          'Curveball'='CU',
                                          'Sinker'='SI',
                                          'Changeup'='CH',
                                          'Cutter'='FC',
                                          'Knuckle Curve'='KC',
                                          'Split-Finger'='FS',
                                          'Curveball (cs)'='CS',
                                          'Fastball'='FA',
                                          'Knuckleball'='KN'),
                           selected = list('SL','FF','CU','SI','CH','FC',
                                           'KC','FS','CS','FA','KN')),
 
        width = 2
        
      ),
      
      ## Show a plot ##
      mainPanel(
        h4("Plot of Pitcher Spin Rate over Time by Pitcher & Pitch Type"),
        h6("Source - baseballR & baseball Savant"),
   
        plotOutput("plot"),
        height = '100%'
      )
   )
)


## Define server logic required to draw a histogram ##
server <- function(input, output) {
   
   output$plot <- renderPlot({
     
     ## Define parameters used for plot ##
     start <-  as.Date(input$start_date)
     end <- as.Date(input$end_date)
     
     first_name <- str_split(input$pitcher_name,' ')[[1]][1]
     last_name <- str_split(input$pitcher_name,' ')[[1]][2]
     
     pitch_type_list <- input$pitch_type
     
     
     
     # start <- as.Date('2021-04-01')
     # end <- as.Date('2021-06-27')
     # 
     # first_name = 'Trevor'
     # last_name = 'Bauer'
     # 
     # pitch_type_list = c('SL','FF','CU','SI','CH','FC',
     #                     'KC','FS','CS','FA','KN')
     
     ## Get player ID ##
     player <- playerid_lookup(last_name,first_name) %>% 
       filter(!is.na(mlbam_id))

     ## Scrape player data using player ID ##
     pitcher <- scrape_statcast_savant_pitcher(start_date = start, end_date = end, pitcherid = player$mlbam_id)

     ## Summarize pitches, group by date and pitch type, count pitches, get average speed/spin rate ##
     plt <- pitcher %>%
       select(game_date, player_name,pitch_type, pitch_name, release_speed,release_spin_rate) %>%
       group_by(game_date,player_name,pitch_name,pitch_type) %>%
       summarise(
         total_pitches = n(),
         avg_release_speed = mean(release_speed),
         avg_release_spinrate = mean(release_spin_rate)
       ) %>%
       ungroup() %>%
       mutate(
         max_date = max(game_date),
         label = ifelse(game_date == max_date,pitch_name,NA)) %>%
       filter(pitch_type %in% pitch_type_list)

     
     ## Identify pitch types thrown by pitcher ##
     pitches <- plt %>% distinct(pitch_name)
     pitches <- pitches$pitch_name
     
     y_label_pos <- min(plt$avg_release_spinrate)
     
     ## Plot average spin rate by game date for pitcher ##
     plt %>%
       ggplot(aes(x = game_date, y = avg_release_spinrate)) +
       geom_path(aes(y = avg_release_spinrate, color = pitch_name)) +
       geom_point(aes(y = avg_release_spinrate, color = pitch_name), size = plt$total_pitches/5, alpha = 3/4) +
       geom_vline(xintercept = as.Date('2021-06-15'), linetype = 'dotted', color = 'black') +
       geom_text(aes(x=as.Date('2021-06-15'), label="2021-06-15\nMLB Enforces Sticky Rules"), y=y_label_pos, colour="black") + #, text=element_text(size=10)) +
       #geom_text(aes(x=as.Date('2021-06-15'), label="MLB Enforces Sticky Rules"), y=y_label_pos + 20, colour="black") + #, text=element_text(size=10)) +
       geom_text_repel(aes(label = label)) +
       labs(
         title = 'Spin Rate over Time',
         subtitle = paste0(first_name,' ',last_name,',    ',start,'  --  ',end,',    Pitch Types - ',toString(pitches)),
         caption = "Joey Bloom - @data_bears (Data - baseballR)",
         y = "Average Release Spin Rate"
       ) +
       scale_x_date(date_labels = "%Y %b %d",date_breaks = "5 days") +
       scale_y_continuous(breaks = seq(floor(min(plt$avg_release_spinrate,na.rm = TRUE)) - 25, ceiling(max(plt$avg_release_spinrate,na.rm = TRUE)) + 25, by = 100)) +
       theme_classic() +
       theme(axis.title = element_text(size = 12),
             axis.text = element_text(size = 12),
             plot.title = element_text(size = 17, hjust = 0.5),
             plot.subtitle = element_text(size = 10, hjust = 0.5),
             plot.caption = element_text(size = 10),
             legend.position = "none",
             axis.text.x=element_text(angle=60, hjust=1))
     
  },height = 650)
}


## Run the application ##
shinyApp(ui = ui, server = server)

