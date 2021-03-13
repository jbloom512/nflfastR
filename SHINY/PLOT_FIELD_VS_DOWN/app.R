#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Plot Field by Down (Team or QB)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("team_or_qb", label = h3("Team or QB Graph"), 
                    choices = list("Team" = 'team', "QB" = 'qb'), 
                    selected = 'team'),
        
        # Copy the line below to make a slider range 
        sliderInput("year_range", label = h3("Year Range"), min = 2006, 
                    max = 2020, value = c(2015, 2020)),
        
        # Copy the line below to make a slider range 
        sliderInput("week_range", label = h3("Week Range"), min = 1, 
                    max = 20, value = c(1,17)),
 
        
        selectInput("metric", label = h3("Metric"), 
                    choices = list(
                      'Average EPA'='avg_epa' ,
                      'Average QB EPA'='avg_qb_epa' ,
                      'Average CPOE'='avg_cpoe' ,
                      'Average CPOE+EPA'='avg_cpoe_epa',
                      'Average Yds To Go'= 'avg_yds_to_go' ,
                      'Average Yds Gained'='avg_yds_gained',
                      'Dropback Rate'='db_rate' ,
                      'Rush Rate'= 'rush_rate' ,
                      'Total Dropbacks'= 'n_dropbacks' ,
                      'Total Rushs'=    'n_rush' ,
                      'Total Plays'= 'n_plays'
                      ), 
                      selected = 'avg_epa'),
        
               
        textInput("team_or_qb_name", label = h3("Team Abrev or QB Name"), value = "CHI,GB"),
        
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
  
     path <- "/Users/joeybloom/Desktop/NFLScrapR/database/pbp_db"
     connection <- dbConnect(SQLite(), path)
     pbp_db <- tbl(connection, "nflfastR_pbp")
     colors <- tbl(connection, "team_color_logos")
     
     
     year_range = input$year_range
     week_range = input$week_range
     metric = input$metric
     team_or_qb = input$team_or_qb
     name = input$team_or_qb_name

     min_year = year_range[1]
     max_year = year_range[ length(year_range) ]

     min_week = week_range[1]
     max_week = week_range[ length(week_range) ]     
     
     highlight_names <- name %>% strsplit(",") %>% as.vector()
     highlight_names <- highlight_names[[1]]
     
     #### QB PLOTTER  ####
     view_qb_epa_by_down <- function(min_year,max_year,min_week,max_week,highlight_qb,highlight_field) {
       
       highlight_field_disp <- highlight_field %>% 
         ## Get rid of a bunch of underscores if any in field name ##
         str_replace('_',' ') %>% 
         str_replace('_',' ') %>% 
         str_replace('_',' ') %>% 
         str_replace('_',' ') %>%
         str_replace('_',' ') %>%
         str_to_upper()
       
       n_dropbacks_filter = 50 * (max_year - min_year + 1)
       
       ## Review QBs by Down  ##
       qbs <- pbp_db %>% 
         left_join(
           colors,
           by = c('posteam'='team_abbr')
         ) %>% 
         filter(pass == 1 | rush == 1,
                down <= 3 ,
                season >= min_year & season <= max_year,
                week >= min_week & week <= max_week) %>% 
         group_by(name,team_color,down) %>% 
         summarise(
           avg_epa = mean(epa),
           avg_qb_epa = mean(qb_epa),
           avg_cpoe = mean(cpoe),
           avg_cpoe_epa = mean(cpoe + epa),
           avg_yds_to_go = mean(ydstogo),
           avg_yds_gained = mean(yards_gained),
           n_dropbacks = sum(pass),
           n_rush = sum(rush)
         ) %>% 
         filter(n_dropbacks >= n_dropbacks_filter) %>% 
         collect()
       
       
       ## Plot QBs by field ##
       qbs %>% 
         ggplot(aes_string(x = "down", y = highlight_field, group = "name")) +
         geom_path(aes_string(x = "down", y = highlight_field),
                   color = qbs$team_color,
                   alpha = ifelse(qbs$name %in% highlight_qb,7/8,1/6)) +
         geom_hline(yintercept =lapply(qbs[highlight_field], mean) %>% as.double(), color = 'red', linetype = 'dotted') +
         geom_text_repel(label = ifelse(qbs$name %in% highlight_qb & qbs$down == 3,qbs$name,'')) +
         scale_x_continuous(name = 'Down',breaks = c(1,2,3)) +
         labs(
           x = "Down",
           y = highlight_field_disp,
           caption = "@data_bears (Data from nflfastR)",
           title = paste0(highlight_field_disp,' vs Down'),
           subtitle = paste0(min_year,' - ',max_year,', Weeks ',min_week,' - ',max_week,', ',n_dropbacks_filter,' Dropbacks, Pass and Rush attempts')
         ) + 
         theme_classic() +
         theme(axis.title = element_text(size = 12),
               axis.text = element_text(size = 7),
               plot.title = element_text(size = 16, hjust = 0.5),
               plot.subtitle = element_text(size = 10, hjust = 0.5),
               plot.caption = element_text(size = 10),
               legend.position = "none") 
       
     }
     
     
     #### TEAM PLOTTER  ####
     view_team_by_down <- function(min_year,max_year,min_week,max_week,highlight_team,highlight_field) {
       
       highlight_field_disp <- highlight_field %>% 
         ## Get rid of a bunch of underscores if any in field name ##
         str_replace('_',' ') %>% 
         str_replace('_',' ') %>% 
         str_replace('_',' ') %>% 
         str_replace('_',' ') %>%
         str_replace('_',' ') %>%
         str_to_upper()
       
       
       ## Review QBs by Down  ##
       teams <- pbp_db %>% 
         left_join(
           colors,
           by = c('posteam'='team_abbr')
         ) %>% 
         filter(pass == 1 | rush == 1,
                down <= 3 ,
                season >= min_year & season <= max_year,
                week >= min_week & week <= max_week) %>% 
         group_by(posteam,team_color,team_color2,down) %>% 
         summarise(
           avg_epa = mean(epa),
           avg_qb_epa = mean(qb_epa),
           avg_cpoe = mean(cpoe),
           avg_cpoe_epa = mean(cpoe + epa),
           avg_yds_to_go = mean(ydstogo),
           avg_yds_gained = mean(yards_gained),
           db_rate = sum(pass) / ( sum(pass) + sum(rush)),
           rush_rate = sum(rush) / ( sum(pass) + sum(rush)),
           n_dropbacks = sum(pass),
           n_rush = sum(rush),
           n_plays = sum(pass) + sum(rush)
         ) %>% 
         collect()
       

       ## Plot QBs by field ##
       teams %>% 
         ggplot(aes_string(x = "down", y = highlight_field, group = "posteam")) +
         geom_path(aes_string(x = "down", y = highlight_field),
                   color = teams$team_color,
                   alpha = ifelse(teams$posteam %in% highlight_team,7/8,1/6)) +
         geom_hline(yintercept =lapply(teams[highlight_field], mean) %>% as.double(), color = 'red', linetype = 'dotted') +
         geom_text_repel(label = ifelse(teams$posteam %in% highlight_team & teams$down == 3,teams$posteam,'')) +
         scale_x_continuous(name = 'Down',breaks = c(1,2,3)) +
         labs(
           x = "Down",
           y = highlight_field_disp,
           caption = "@data_bears (Data from nflfastR)",
           title = paste0(highlight_field_disp,' vs Down'),
           subtitle = paste0(min_year,' - ',max_year,', Weeks ',min_week,' - ',max_week,', Pass and Rush attempts')
         ) + 
         theme_classic() +
         theme(axis.title = element_text(size = 12),
               axis.text = element_text(size = 7),
               plot.title = element_text(size = 16, hjust = 0.5),
               plot.subtitle = element_text(size = 10, hjust = 0.5),
               plot.caption = element_text(size = 10),
               legend.position = "none") 
       
     }
     
      
     if(team_or_qb == 'team'){
       view_team_by_down(min_year,max_year,min_week,max_week,highlight_names,metric)
     }
     else{
       view_qb_epa_by_down(min_year,max_year,min_week,max_week,highlight_names,metric)
     }
     
   },height=650,width=800)
}

# Run the application 
shinyApp(ui = ui, server = server)

