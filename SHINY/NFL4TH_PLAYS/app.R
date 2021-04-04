rm(list=ls())

library(tidyverse)
library(gt)
library(shiny)

fourth <- read.csv("nfl4th_all_4th_downs_2014_2020.csv") %>%
  left_join(
    nflfastR::teams_colors_logos %>% mutate(posteam_logo = team_logo_espn) %>% select(c(team_abbr,posteam_logo)),
    by = c('posteam' = 'team_abbr')
  ) %>%
  left_join(
    nflfastR::teams_colors_logos %>% mutate(defteam_logo = team_logo_espn) %>% select(c(team_abbr,defteam_logo)),
    by = c('defteam' = 'team_abbr')
  ) %>% 
  filter(play_type %in% c('punt','field_goal','run','pass','no_play'))


# Define UI for application that draws a histogram
ui <- fluidPage(

  
   # Application title
   titlePanel("NFL 4th Down Go Rate + Win Probability Added"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(

      sidebarPanel(
        
        
        
        submitButton(text = "Update Chart", icon = NULL, width = NULL),

        textInput("team_name", label = h5("Team Name"), value = "ALL"),
        h6("Enter a comma separated list of teams. ALL for all teams."),
        h6("IE: CHI,GB,DET   or   ALL"),

        # Copy the line below to make a slider range 
        sliderInput("season_range", label = h5("Season Range"), min = 2014, 
                    max = 2020, value = c(2014,2020)),
                
        # Copy the line below to make a slider range 
        sliderInput("week_range", label = h5("Week Range"), min = 1, 
                    max = 21, value = c(1,21)),
        
        
        # Copy the line below to make a slider range 
        sliderInput("display_rows", label = h5("Rows to Display"), min = 1, 
                    max = 100, value = 20),          
        
        # Copy the line below to make a slider range 
        sliderInput("wpa_range", label = h5("Win Prob (WP) Range"), min = 0, 
                    max = 1, value = c(0,1)),
 
        
        # Copy the line below to make a slider range 
        sliderInput("go_boost_range", label = h5("Go Boost Range"), min = -60, 
                    max = 60, value = c(1,60)),
 

        # Copy the chunk below to make a group of checkboxes
        checkboxGroupInput("go_flag_range", label = h5("Go Flag"), 
                           choices = list("Go" = 100, "No Go" = 0),
                           selected = 100),

        # Copy the chunk below to make a group of checkboxes
        checkboxGroupInput("play_flag", label = h5("Play Type"), 
                           choices = list('Punt'='punt','Field Goal'='field_goal','Rush'='run','Pass'='pass','Penalty'='no_play'),
                           selected = list('punt','field_goal','run','pass','no_play')), 
        

        # Copy the chunk below to make a group of checkboxes
        checkboxGroupInput("first_down_flag", label = h5("First Down?"), 
                           choices = list("Yes" = 1, "No" = 0),
                           selected = list(1,0)),
        
        
        # Copy the chunk below to make a group of checkboxes
        checkboxGroupInput("quarter_flag", label = h5("Quarter"), 
                           choices = list("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4),
                           selected = list(1,2,3,4)),
        
        # Copy the chunk below to make a group of checkboxes
        checkboxGroupInput("season_type_flag", label = h5("Season Type"), 
                           choices = list("Regular Season" = 'REG', "Post Season" = 'POST'),
                           selected = list('REG','POST')),
 
               
        # Copy the chunk below to make a group of checkboxes
        radioButtons("sort_by_flag", label = h5("Sort By"), 
                           choices = list("WPA" = 'wpa', "Go Boost" = 'go_boost'),
                           selected = "go_boost"),
        
        # Copy the chunk below to make a group of checkboxes
        radioButtons("arrange_flag", label = h5("Arrange By"), 
                     choices = list("Descending" = 'desc', "Ascending" = 'asc'),
                     selected = "desc"),
               

        
        width = 2
        
      ),
      
      # Show a plot 
      mainPanel(
        h4("Go Boost shows the predicted gain (or loss, when negative) in win probability associated with going for it, relative to the next-best alternative (whether kicking a field goal or punting)."),
        h6("Source - Ben Baldwin"),
        h6("https://www.nfl4th.com/articles/articles/4th-down-research.html"),
        #tags$a(href="https://www.nfl4th.com/articles/articles/4th-down-research.html", "Click here!"),
        
        gt_output("plot"),
        width = '100%'
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- render_gt({
     
     TEAM = input$team_name
     
     if(TEAM == 'ALL'){
       highlight_teams <- fourth %>% select(posteam) %>% distinct()
       highlight_teams <- as.character(highlight_teams$posteam)
       print(highlight_teams)
     }
     else{
     
     
       highlight_teams <- TEAM %>% strsplit(",") %>% as.vector()
       highlight_teams <- highlight_teams[[1]]
       
       for (i in 1:length(highlight_teams)) {
         highlight_teams[[i]] <- str_trim(highlight_teams[[i]])
       }
     
   }
     
     
     
     
     
     min_wp <- min(input$wpa_range)
     max_wp <- max(input$wpa_range)
     
     min_go_boost <- min(input$go_boost_range)
     max_go_boost <- max(input$go_boost_range)
     
     
     min_season <- min(input$season_range)
     max_season <- max(input$season_range)
     
     go_flag <- input$go_flag_range
     
     first_down_flag <- input$first_down_flag

     sort_flag <- input$sort_by_flag
     arrange_flag <- input$arrange_flag 
     
     season_type_l <- input$season_type_flag
     
     display_rows <- input$display_rows
     
     quarter_flag <- input$quarter_flag
     
     play_flag <- input$play_flag
     
     
     min_week <- min(input$week_range)
     max_week <- max(input$week_range)     
     # 
     # ## FOR TEST ##
     # TEAM <- 'CHI'
     # 
     # 
     # if(TEAM == 'ALL'){
     #   highlight_teams <- fourth %>% select(posteam) %>% distinct()
     #   highlight_teams <- as.character(highlight_teams$posteam)
     #   print(highlight_teams)
     # }else{
     # 
     # 
     #   highlight_teams <- TEAM %>% strsplit(",") %>% as.vector()
     #   highlight_teams <- highlight_teams[[1]]
     # 
     #   for (i in 1:length(highlight_teams)) {
     #     highlight_teams[[i]] <- str_trim(highlight_teams[[i]])
     #   }
     # 
     # }
     # 
     # 
     # min_wp <- 0
     # max_wp <- 1
     # 
     # min_go_boost <- -5
     # max_go_boost <- 5
     # 
     # 
     # min_season <- 2014
     # max_season <- 2020
     # 
     # go_flag <- c(1,0)
     # 
     # first_down_flag <- c(1,0)
     # 
     # sort_flag <- 'wpa'
     # arrange_flag <- 'desc'
     #
     # season_type_l <- c('POST')
     #
     # display_rows <- 10
     
     
     chart <- fourth %>% 
       filter(wp > min_wp & wp < max_wp,
              go_boost >= min_go_boost & go_boost <= max_go_boost,
              season >= min_season & season <= max_season,
              week >= min_week & week <= max_week,
              go %in% go_flag,
              first_down %in% first_down_flag,
              posteam %in% highlight_teams,
              season_type %in% season_type_l,
              qtr %in% quarter_flag,
              play_type %in% play_flag
       ) %>% 
       mutate(wpa = round(wpa*100,2),
              wp = round(wp*100,2),
              go_boost = round(go_boost,2)) %>% 
       select(posteam_logo,defteam_logo,season,week,qtr,yrdln,ydstogo,score_differential,
              wp,wpa,
              #epa,
              go_boost, 
              #first_down_prob,
              #wp_fail, wp_succeed, go_wp, 
              desc
       ) %>% 
       arrange(
         
         if('desc' %in% arrange_flag){
           desc(!!sym(sort_flag))
         }else{
           !!sym(sort_flag)
         }
         
       ) %>% 
       filter(row_number() <= display_rows) %>% 
       as_tibble()
     

     
     
     
     min_wpa_col <- chart %>% select(wpa) %>% min()
     max_wpa_col <- chart %>% select(wpa) %>% max()
     
     if(min_wpa_col < 0 & max_wpa_col < 0){
       wpa_domain <- c(min_wpa_col,max_wpa_col,0)
       wpa_color <- c('red','red','yellow')
       
     } else if (min_wpa_col < 0 & max_wpa_col >= 0){
       wpa_domain <- c(min_wpa_col,0,max_wpa_col)
       wpa_color <- c('red','yellow','green')
       
     } else if (min_wpa_col >= 0 & max_wpa_col >= 0){
       wpa_domain <- c(0,min_wpa_col,max_wpa_col)
       wpa_color <- c('yellow','green','green')
     }
     

     
     min_gb_col <- chart %>% select(go_boost) %>% min()
     max_gb_col <- chart %>% select(go_boost) %>% max()
     
     if(min_gb_col < 0 & max_gb_col < 0){
       gb_domain <- c(min_gb_col,max_gb_col,0)
       gb_color <- c('red','red','yellow')
       
     } else if (min_gb_col < 0 & max_gb_col >= 0){
       gb_domain <- c(min_gb_col,0,max_gb_col)
       gb_color <- c('red','yellow','green')
       
     } else if (min_gb_col >= 0 & max_gb_col >= 0){
       gb_domain <- c(0,min_gb_col,max_gb_col)
       gb_color <- c('yellow','green','green')
     }
     
     chart %>% 
       gt() %>%
       tab_header(
         title = "", #paste0("4th Down Gain/Loss in WPA"),
         subtitle = paste0("Teams: ",TEAM,', Season >= ',min_season,' & Season <= ',max_season,', Week >= ',min_week,' & Week <= ',max_week,', Quarters - ',toString(quarter_flag),', Win Prob >= ',min_wp,' & Win Prob <= ',max_wp,', Go Boost >= ',min_go_boost,' & Go Boost <= ',max_go_boost,', Go Flag = ',go_flag,', Play Type = ',toString(play_flag),', First Down = ',toString(first_down_flag))
       ) %>% 
       tab_source_note(md("**Data:** nflfastR + Ben Baldwin's nfl4th Model<br>**App:** Joey Bloom (@data_bears)"))  %>% 
       text_transform(
         locations = cells_body(vars(posteam_logo,defteam_logo)),
         fn = function(x){
           web_image(
             url = x,
             height = px(30)
           )
         }
       ) %>%
       cols_label(
         posteam_logo = "Off",
         defteam_logo = "Def",
         score_differential = 'Score Dif',
         season = "Season",
         week = "Week",
         qtr = "Qtr",
         ydstogo = 'ToGo',
         wp = 'Win Prob',
         wpa = 'WP Added',
         go_boost = 'Go Boost',
         desc = 'Result',
         yrdln = 'YardLine'
       ) %>% 
       data_color(
         columns = vars(wpa),
         colors = scales::col_numeric(
           palette = wpa_color,
           domain = wpa_domain)
         ) %>% 
       data_color(
         columns = vars(go_boost),
         colors = scales::col_numeric(
           palette = gb_color,
           domain = gb_domain)
       ) %>% 
       # tab_style(
       #   style = cell_text(weight = "bold"),
       #   locations = cells_body(
       #     columns = vars(RK, name)
       #   )
       # ) %>% 
       tab_options(
         column_labels.background.color = "white",
         column_labels.font.weight = "bold",
         table.border.top.width = px(3),
         table.border.top.color = "transparent",
         table.border.bottom.color = "transparent",
         table.border.bottom.width = px(3),
         column_labels.border.top.width = px(3),
         column_labels.border.top.color = "transparent",
         column_labels.border.bottom.width = px(3),
         column_labels.border.bottom.color = "black",
         data_row.padding = px(3),
         source_notes.font.size = 12,
         table.font.size = 16,
         heading.align = "left"
       ) %>%
       opt_table_font(
         font = list(
           google_font("Chivo"),
           default_fonts()
         )
       ) 
     
     

     
  
     
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

