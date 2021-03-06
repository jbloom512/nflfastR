#install.packages('nfl4th')

rm(list=ls())

library(nfl4th)
library(tidyverse)
library(gt)
library(ggplot2)
library(ggrepel)

theme_set(  theme_classic() +
              theme(axis.title = element_text(size = 12),
                    axis.text = element_text(size = 9),
                    plot.title = element_text(size = 13, hjust = 0.5),
                    plot.subtitle = element_text(size = 10, hjust = 0.5),
                    plot.caption = element_text(size = 10),
                    legend.position = "none") 
            )



#### Pull in Seasons ####
data <- nfl4th::load_4th_pbp(2014:2020)


#### Process Fourth Down Data ####
fourth <- data %>% 
  select(season,week,down,defteam,
         posteam, ydstogo, yardline_100, posteam, go_boost, first_down_prob, 
         wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, 
         fg_wp, punt_wp, go, desc
  ) %>% 
  filter(down == 4) %>% 

  #### Assign Model Suggestions ####

  # The key columns generated by the main nfl4th function, load_4th_pbp(), 
  # are go_boost, which gives the predicted gain (or loss, when negative) in 
  # win probability associated with going for it, relative to the next-best alternative 
  # (whether kicking a field goal or punting), and go, which is an indicator for whether 
  # the team went for it on a given play. Note that go_boost and go are measured in 
  # percentage points (i.e., 0 to 100) in order to make creating figures like the following easier. 
  # This means that the values for go are either 0 or 100 in every row.

  mutate(rec = case_when(
    go_boost >= 4 ~ "Definitely go for it",
    go_boost > 1 & go_boost < 4 ~ "Probably go for it",
    go_boost >= -1 & go_boost <= 1 ~ "Toss-up",
    go_boost < -1 & go_boost > -4 ~ "Probably kick",
    go_boost <= -4 ~ "Definitely kick"
  ),
  rec_num = case_when(
    go_boost >= 4 ~ 2,
    go_boost > 1 & go_boost < 4 ~ 1,
    go_boost >= -1 & go_boost <= 1 ~ 0,
    go_boost < -1 & go_boost > -4 ~ -1,
    go_boost <= -4 ~ -2
  ),
  go_type = case_when(
    go == 100 ~ 'Go',
    go == 0   ~ 'NoGo'
  )) %>% 
  filter(!is.na(rec)) %>% 
  
  ## Determine if correct decision was made ##
  ## For "toss-ups", assign 0 ##
  mutate(correct_num = case_when(
    rec_num < 0 & go == 0 ~ 1,
    rec_num < 0 & go == 100 ~ -1,
    rec_num > 0 & go == 100 ~ 1,
    rec_num > 0 & go == 0 ~ -1,
    rec_num == 0 ~ 0
  ),
  correct = case_when(
    rec_num < 0 & go == 0 ~ 'correct',
    rec_num < 0 & go == 100 ~ 'incorrect',
    rec_num > 0 & go == 100 ~ 'correct',
    rec_num > 0 & go == 0 ~ 'incorrect',
    rec_num == 0 ~ 'tossup'
  ))


fourth %>% write.csv("nfl4th_all_4th_downs_2014_2020.csv")



## View Chicago Bears Data ##
fourth %>% 
  filter(posteam == 'CHI') %>% 
  select(c(season,go_boost,rec,rec_num,go,go_type,wp_succeed,go_wp,correct,desc)) %>% 
  arrange(desc(go_boost)) %>% 
  View()



#### Calculate Decisions made when model suggests to go for it ####
all_dec <- fourth %>%
  filter(rec_num >= 1) %>% 
  count(season,posteam,go_type) %>% 
  pivot_wider(names_from=go_type, values_from=n,values_fill = 0) %>% 
  mutate(total = Go + NoGo,
         go_rate = Go / total) %>% 
  left_join(
    nflfastR::teams_colors_logos,
    by = c("posteam" = "team_abbr")
  )
  


all_dec %>% write.csv("nfl4th_all_dec_2014_2020.csv")




#### Plot Go Rate on Toss-up --> Definitley Go For It ####
# 
# TEAM <- 'CLE'
# 
# plt <- all_dec %>% 
#   filter(posteam == TEAM)
# 
# plt %>% 
#   ggplot(aes(x = season, y = go_rate)) +
#   geom_path(aes(x = season, y = go_rate)) + 
#   geom_point(aes(x = season, y = go_rate),
#              size = plt$total / 10) +
#   #geom_text_repel(label = ifelse(plt$season == 2020,plt$rec,''),
#   #                size = 2) + 
#   #geom_text_repel(label = plt$total) +
#   geom_hline(yintercept = .5,color='red',linetype='dotted') + 
#   labs(
#     title = 'Fourth Down Go Rate',
#     subtitle = paste(TEAM,', nfl4th Go Boost > 1, 2015 - 2020'),
#     x = 'Season',
#     y = 'Go Rate',
#     caption = "@data_bears (Data from nflfastR + nfl4th)"
#   ) 
# 




##### 4th Down Go Rate Spaghetti Graphs #####
TEAM <- 'CHI'

all_dec %>% 
  ggplot(aes(x = season, y = go_rate, group = posteam)) +
  geom_smooth(group=1,
              color = 'red',
              linetype = 'dashed',
              size = 1) +
  geom_path(aes(x = season, y = go_rate), 
            color = all_dec$team_color,
            alpha = ifelse(all_dec$posteam == TEAM,4/5,1/5),
            #size = all_dec$total / 20
            size = ifelse(all_dec$posteam == TEAM,1,.5)
            ) + 
  geom_hline(yintercept = .5,color='red',linetype='dotted') + 
  labs(
    title = 'Fourth Down Go Rate',
    subtitle = paste0(TEAM,', 2015 - 2020, Go Boost > 1'),
    x = 'Season',
    y = 'Go Rate',
    caption = "@data_bears (Data from nflfastR + nfl4th)"
  ) 





