library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(ggimage)
library(ggrepel)
library(h2o)
library(zoo)
library(corrplot)


test_preds <- read.csv("/Users/joeybloom/Desktop/NFLScrapR/GIT/nflfastR/MODELS/Fourth_Down_Convert/SHINY/2018_2020_GLM_FDOE.csv") %>% 
  as_tibble() %>% 
  left_join(
    nflfastR::teams_colors_logos %>% 
      rename(def_team_color = team_color,
            def_team_color2 = team_color2,
            def_team_logo_espn = team_logo_espn) %>% 
    select(team_abbr,starts_with('def_')), 
    by=c('defteam' = 'team_abbr')
  )







plt <- test_preds %>% 
  filter(qtr %in% c(1,2,3,4,5),
         season %in% c(2018),
         week >= 1 & week <= 17,
         down %in% c(3,4)) %>% 
  group_by(defteam,def_team_color,def_team_logo_espn) %>% 
  summarise(
    total_fdoe = sum(fdoe_glm),
    avg_fdoe =  mean(fdoe_glm)
  ) %>% 
  arrange(desc(total_fdoe)) %>% 
  ungroup() %>% 
  mutate(label_pos = ifelse(total_fdoe < 0, 2, -2)) 




plt



## Plot total FDOE by Team 
plt %>% 
  ggplot(aes(x = reorder(defteam,total_fdoe), y = total_fdoe)) + 
  geom_bar(stat='identity',fill = plt$def_team_color,width = .7) +
  geom_image(aes(x=reorder(defteam,total_fdoe),y=total_fdoe,image = def_team_logo_espn),asp = 16/9,size=.027) +
  geom_text(aes(x = reorder(defteam,total_fdoe), y = label_pos, label = round(total_fdoe,2)),size = 4) +
  labs(
    title = 'First Downs Over Expected',
    subtitle = '2018, Weeks 1-17, Quarters 1-5, 3rd/4th Downs, Rush + Pass Plays, FDOE calculated from Generalized Linear Model',
    caption = "Joey Bloom - @data_bears (Data - nflfastR)",
    y = "Total FDOE"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.caption = element_text(size = 7),
        legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_flip() 












plt <- test_preds %>% 
  filter(qtr %in% c(1,2,3,4,5),
         season %in% c(2018,2019,2020),
         week >= 1 & week <= 17,
         down %in% c(3,4)) %>% 
  group_by(name,team_color) %>% 
  summarise(
    total_fdoe = sum(fdoe_glm),
    avg_fdoe =  mean(fdoe_glm),
    avg_epa = mean(epa),
    plays = n()
  ) %>% 
  arrange(desc(total_fdoe)) %>% 
  filter(plays > 300) %>% 
  ungroup() %>% 
  mutate(label_pos = ifelse(total_fdoe < 0, 2, -2)) 




## Plot total FDOE by Team 
plt %>% 
  ggplot(aes(x = avg_epa, y = total_fdoe)) + 
  #geom_bar(stat='identity',width = .7) +
  #geom_image(aes(x=reorder(name,total_fdoe),y=total_fdoe,image = def_team_logo_espn),asp = 16/9,size=.027) +
  #geom_smooth(method = 'lm') +
  geom_point(aes(x = avg_epa, y = total_fdoe), 
             color = plt$team_color,
             size = plt$plays / 100,
             alpha = 5/8) +
  geom_text_repel(label = plt$name) +
  geom_hline(yintercept = mean(plt$total_fdoe), color = 'red', linetype = 'dotted') +
  geom_vline(xintercept = mean(plt$avg_epa,na.rm=TRUE), color = 'red', linetype = 'dotted') +
  labs(
    title = 'Total FDOE vs Average EPA',
    subtitle = '2018 - 2020, Weeks 1-17, Quarters 1-5, 3rd/4th Downs, Rush + Pass Plays, Min. 300 plays',
    caption = "Joey Bloom - @data_bears (Data - nflfastR)",
    y = "Total FDOE",
    x = "Avg EPA"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.caption = element_text(size = 7),
        legend.position = "none",
        #axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.y=element_blank()
        ) 





test_preds %>% 
  arrange(desc(fdoe_glm)) %>% 
  select(c(team_logo_espn,def_team_logo_espn,season,week,score_differential,yardline_100,qtr,down,ydstogo,fdoe_glm,desc)) %>% 
  filter(str_contains(desc,'PENALTY'))
  head(10) %>% 
  gt() %>%
  tab_header(
    title = paste0("3rd/4th Down Total FDOE")
    #subtitle = paste0(highlight_teams,', wp >= ',min_wp,' & win <= ',max_wp,', go_boost >= ',min_go_boost,' & go_boost <= ',max_go_boost,', go = ',go_flag)
  ) %>% 
  tab_source_note(md("**Data:** nflfastR<br>**Table:** @data_bears"))  %>% 
  text_transform(
    locations = cells_body(vars(team_logo_espn,def_team_logo_espn)),
    fn = function(x){
      web_image(
        url = x,
        height = px(30)
      )
    }
  ) %>% 
  cols_label(
    team_logo_espn = "Offense",
    def_team_logo_espn = "Defense",
    season = "Season",
    week = "Week",
    qtr = "Quarter",
    score_differential = "Score Dif", 
    yardline_100 = "Yrd Line",
    fdoe_glm = 'FDOE',
    ydstogo = 'Yards To Go',
    desc = 'Play Result'
  ) %>% 
  data_color(
    columns = vars(fdoe_glm),
    colors =  c("red","green")
  ) %>% 
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
    heading.align = "Center"
  ) %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) 

  
  
  View()

  
  
  
