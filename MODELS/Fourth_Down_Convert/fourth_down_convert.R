rm(list = ls())

library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(ggimage)
library(ggrepel)
library(h2o)
library(zoo)



source('https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R')

#### Connect to nflfastR database ####
path <- "/Users/joeybloom/Desktop/NFLScrapR/database/pbp_db"

connection <- dbConnect(SQLite(), path)

pbp_db <- tbl(connection, "nflfastR_pbp")
colors <- tbl(connection,"team_color_logos")
rosters <- tbl(connection,"team_rosters")





#### Get 3rd / 4th downs where a rush/pass occurred (IE, no kick) ####
##
## Note:   Most of this section here is pulled straight from Ben Baldwins fourth down "go_for_it" model. 
##         There are a few slight modifications, but this is largely the same as what is done in his model.
##
## Source:
## https://github.com/guga31bb/fourth_calculator/blob/main/scripts/_go_for_it_model.R 


last_play_drive_t1 <- pbp_db %>% 
  ## src - @benbaldwin
  filter(
    down %in% c(3,4), 
    qb_kneel == 0,
    rush == 1 | pass == 1, 
    !is.na(posteam),
    !is.na(yardline_100),
    !is.na(score_differential)
  ) %>% 
  ## src - @benbaldwin
  make_model_mutations()


#### Attributes from 3rd/4th down to use as input ####


last_play_drive_t2 <- last_play_drive_t1 %>% 
  mutate(yards_gained = 
          
           # src - @benbaldwin
           # we need a way to account for defensive penalties that give auto first downs
           # hacky "solution" is saying here that a penalty that gives a first down goes for the yards to go
           # unless the actual penalty yardage is higher
           
           # the drawback is that a defensive holding on eg 4th and 8 is coded as an 8 yard gain
           # the alternative is to estimate a separate model for penalties or have their own category
           # but first down penalties on 4th and long are very rare:
           # https://twitter.com/benbbaldwin/status/1322530446371074050
           
           case_when(
             first_down_penalty == 1 & penalty_yards < ydstogo ~ ydstogo, 
             first_down_penalty == 1 & penalty_yards >= ydstogo ~ penalty_yards, 
             TRUE ~ yards_gained
           ),
         
         # truncate to make model training easier -- add capping at -10 and 65 yards (src - @benbaldwin) #
         yards_gained = if_else(yards_gained < -10, -10, yards_gained),
         yards_gained = if_else(yards_gained > 65, 65, yards_gained),
         
         # Vegas expected points to be scored (src - @benbaldwin) #
         home_total = (spread_line + total_line) / 2,
         away_total = (total_line - spread_line) / 2,
         
         # Determine total/spread for possession team (src - @benbaldwin) #
         posteam_total = if_else(posteam == home_team, home_total, away_total),
         posteam_spread = dplyr::if_else(posteam == home_team, spread_line, -1 * spread_line)
  ) %>%
  
  # look at when an actual play is run or a defensive penalty gives a first down (src - @benbaldwin)
  filter(play_type_nfl %in% c("RUSH", "PASS", "SACK") | first_down_penalty == 1) %>%

  # Select fields for training #  
  select(
    
    ## Match Back/ID Fields 
    play_id,game_id,posteam,season,
    first_down,
    
    ## @benbaldwin Selected Fields ##
    down,
    ydstogo,
    yardline_100,
    #era3, era4,
    outdoors, retractable, dome,
    posteam_spread, total_line, posteam_total,
    
    ## JB Selected Fields ##
    #era,
    week,yardline_100,game_seconds_remaining,season_type,
    qtr,wp,ep,ydstogo,shotgun,no_huddle,score_differential,
    desc
  )



last_play_drive_t2 %>% 
  count(first_down)

# Groups:   first_down
# first_down  .add     n
# <dbl> <int> <int>
# 1          0     1 44552
# 2          1     1 35809





#### <> ####

#### Get Game cum avg/total ####

## Pass Plays ##
cum_fields <- c('epa','yards_gained','air_yards','yards_after_catch')

p_game_summary_t1 <- pbp_db %>% 
  filter(qb_kneel == 0,
         pass == 1, 
         !is.na(posteam),
         !is.na(yardline_100),
         !is.na(score_differential)) %>% 
  select(season,week,posteam,pass,game_id,play_id,epa,yards_gained,air_yards,yards_after_catch) %>% 
  collect() 

p_game_summary_t2 <-  p_game_summary_t1 %>% 
  filter(season == 2020 ) %>% 
  group_by(game_id,posteam)



p_game_summary_t3 <- p_game_summary_t2 %>% 
  mutate_at(
    vars(cum_fields),funs("cum_game_avg_PASS"= lag(rollapply(.,seq_along(p_game_summary_t2), mean,align='right',fill=NA)),
                          "cum_game_tot_PASS"= lag(cumsum(.))
    )
  ) 



p_game_summary_t3 %>% 
  select(play_id,posteam,season,week,pass,yards_gained,yards_gained_cum_game_avg_PASS,yards_gained_cum_game_tot_PASS) %>% 
  filter(posteam=='CHI') %>% 
  view()
  





## Pass Plays ##
cum_fields <- c('epa','yards_gained')


r_game_summary_t1 <- pbp_db %>% 
  filter(qb_kneel == 0,
         rush == 1, 
         !is.na(posteam),
         !is.na(yardline_100),
         !is.na(score_differential)) %>% 
  select(season,week,posteam,rush,game_id,play_id,epa,yards_gained) %>% 
  collect() 

r_game_summary_t2 <-  r_game_summary_t1 %>% 
  filter(season == 2020 ) %>% 
  group_by(game_id,posteam)



r_game_summary_t3 <- r_game_summary_t2 %>% 
  mutate_at(
    vars(cum_fields),funs("cum_game_avg_RUSH"= lag(rollapply(.,seq_along(p_game_summary_t2), mean,align='right',fill=NA)),
                          "cum_game_tot_RUSH"= lag(cumsum(.))
    )
  ) 



r_game_summary_t3 %>% 
  select(play_id,posteam,season,week,yards_gained,yards_gained_cum_game_avg_RUSH,yards_gained_cum_game_tot_RUSH) %>% 
  filter(posteam=='CHI') %>% 
  view()




## Rush + Pass Summary ##


p_game_summary_t3
r_game_summary_t3


#### <> ####

#### Join Cum Game Fields ####

p_game_summary_t3
r_game_summary_t3

model_data <- last_play_drive_t2 %>% 
  filter(season == 2020) %>% 
  collect() %>% 
  left_join(
    p_game_summary_t3 %>% ungroup() %>% select(game_id,play_id,c(ends_with("_PASS"))), by = c("play_id" = "play_id", "game_id" = "game_id")
  ) %>% 
  left_join(
    r_game_summary_t3 %>% ungroup() %>% select(game_id,play_id,c(ends_with("_RUSH"))), by = c("play_id" = "play_id", "game_id" = "game_id")
  )


model_data %>% 
  select(game_id,play_id,wp,ep
         ,starts_with('epa')
         ,starts_with('yards_gained')
         ,starts_with('yards_after_catch')
         ,starts_with('air_yards')) %>% 
  fill(starts_with('epa')
       ,starts_with('yards_gained')
       ,starts_with('yards_after_catch')
       ,starts_with('air_yards'), .direction = "down") %>% 
  View()




model_data %>% 
  filter(season == 2020,
         posteam == 'CHI') %>% 
  select(game_id,play_id,qtr,ydstogo,score_differential,wp,ep,rush,
         ,starts_with('epa')
         ,starts_with('yards_gained')
         ,starts_with('yards_after_catch')
         ,starts_with('air_yards')) %>% 
  View()



model_data %>%
  ungroup() %>% 
  select(ends_with("_RUSH"),ends_with("_PASS"),
         
         ydstogo,yardline_100,outdoors, retractable, dome,
         posteam_spread, total_line, posteam_total,
         
         week,yardline_100,game_seconds_remaining,season_type,
         qtr,wp,ep,ydstogo,shotgun,no_huddle,score_differential
  )
#### <> ####

#### H2O Model Execution ####

h2o.init()
h2o.no_progress()


## Split train and test data - test on 2018 - 2020 ##
train <- model_data %>% filter(season <= 2017) %>% as.h2o()
test <- model_data %>% filter(season > 2017) %>% as.h2o()

## Make prediction variable factor ##
train$first_down <- as.factor(train$first_down)

#### <> ####
#### <> ####

#### GLM ####
#### BASE GLM ####
vars <- setdiff(names(train),c("play_id","first_down","posteam","season","desc","season_type","down","total_line","outdoors","no_huddle"))

cvrt_glm <- h2o.glm(x = vars, 
                   y = "first_down", 
                   training_frame = train,
                   model_id = "4th_cvrt_glm_fit",
                   nfolds = 10,
                   keep_cross_validation_predictions = TRUE,
                   family = 'binomial',
                   seed=1985)  



## training metrics ##
summary(cvrt_glm)

# ** 10-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
# MSE:  0.2253897
# RMSE:  0.4747523
# LogLoss:  0.6406377
# Mean Per-Class Error:  0.4066028
# AUC:  0.6697238
# AUCPR:  0.5987209
# Gini:  0.3394475
# R^2:  0.08889338
# Residual Deviance:  73696.4
# AIC:  73724.4



## LogLoss - 0.6406377
## AUC     - 0.6697238

## testing metrics ##
glm_base_cvrt <- h2o.performance(cvrt_glm,newdata = test)

glm_base_cvrt

#### BASE GLM Test Results ####
# H2OBinomialMetrics: glm
# 
# MSE:  0.2208503
# RMSE:  0.4699471
# LogLoss:  0.6303643
# Mean Per-Class Error:  0.3970774
# AUC:  0.6842987
# AUCPR:  0.6005364
# Gini:  0.3685974
# R^2:  0.1025802
# Residual Deviance:  28798.82
# AIC:  28826.82



## LogLoss - 0.6303643
## AUC     - 0.6842987



#### TUNED GLM ####

vars <- setdiff(names(train),c("play_id","first_down","posteam","season","desc","season_type","down","total_line","outdoors"))


hyper_params <- list( 
  alpha = c(0, .25, .5, .75, .1), 
  lambda = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0)  
)

glm_grid <- h2o.grid(x = vars, 
                     y = "first_down", 
                     training_frame = train, 
                     algorithm = "glm", 
                     grid_id = "glm_grid1", 
                     hyper_params = hyper_params, 
                     nfolds = 10,
                     family="binomial",
                     search_criteria = list(strategy = "Cartesian"),
                     seed=1985)

sortedGridGLM <- h2o.getGrid("glm_grid1", sort_by = "logloss", decreasing = FALSE)
sortedGridGLM


cvrt_glm_tuned <- h2o.getModel(sortedGridGLM@model_ids[[1]])
cvrt_glm_tuned



## training metrics ##
# ** 10-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
# MSE:  0.2254073
# RMSE:  0.4747708
# LogLoss:  0.6406732
# Mean Per-Class Error:  0.4110204
# AUC:  0.6696128
# AUCPR:  0.5988148
# Gini:  0.3392256
# R^2:  0.08882243
# Residual Deviance:  73700.49
# AIC:  73724.49


## LogLoss - 0.6406732
## AUC     - 0.6696128



## testing metrics ##
glm_tuned_cvrt <- h2o.performance(cvrt_glm_tuned,newdata = test)

glm_tuned_cvrt

#### TUNED GLM Test Results ####
# H2OBinomialMetrics: glm
# 
# MSE:  0.2208059
# RMSE:  0.4698999
# LogLoss:  0.6302859
# Mean Per-Class Error:  0.4036597
# AUC:  0.6845941
# AUCPR:  0.6012661
# Gini:  0.3691882
# R^2:  0.1027605
# Residual Deviance:  28795.24
# AIC:  28817.24

## LogLoss - 0.6302859
## AUC     - 0.6845941

#### GLM RES - TUNED > BASE ####
## 
## Log loss + AUC are slightly better (several decimals deep) on TUNED model
## Pick TUNED GLM model for later comparisons


### Tuned:

# Train:
## LogLoss - 0.6406732
## AUC     - 0.6696128

# Test:
## LogLoss - 0.6302859
## AUC     - 0.6845941



### Base:

# Train:
## LogLoss - 0.6406377
## AUC     - 0.6697238

# Test:
## LogLoss - 0.6303643
## AUC     - 0.6842987



#### <> ####
#### <> ####

#### RF / Random Forest ####
#### BASE Random Forest ####

vars <- setdiff(names(train),c("play_id","first_down","posteam","season","desc","season_type","down"))

cvrt_rf <- h2o.randomForest(x = vars, 
                            y = "first_down", 
                            training_frame = train,
                            model_id = "4th_cvrt_rf_fit1",
                            nfolds = 10,
                            keep_cross_validation_predictions = TRUE,
                            seed=1985)  



## training metrics ##

summary(cvrt_rf)

# ** 10-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
# MSE:  0.2282612
# RMSE:  0.4777669
# LogLoss:  0.6479429
# Mean Per-Class Error:  0.4160097
# AUC:  0.6620898
# AUCPR:  0.590855
# Gini:  0.3241797
# R^2:  0.07728571



## LogLoss - 0.6479429
## AUC     - 0.6620898

## testing metrics ##
rf_base_cvrt <- h2o.performance(cvrt_rf,newdata = test)

rf_base_cvrt

#### BASE RF Test Results ####
# H2OBinomialMetrics: drf
# 
# MSE:  0.2252955
# RMSE:  0.4746531
# LogLoss:  0.640564
# Mean Per-Class Error:  0.4107254
# AUC:  0.6690426
# AUCPR:  0.582047
# Gini:  0.3380853
# R^2:  0.08451727


## LogLoss - 0.640564
## AUC     - 0.6690426



#### TUNED RF ####

vars <- setdiff(names(train),c("play_id","first_down","posteam","season","desc","season_type","down"))


# hyper_params <- list(#
#   ntrees = seq(50, 100, by = 25),
#   mtries = seq(3, 5, by = 1),
#   #max_depth = seq(10, 30, by = 10),
#   #min_rows = seq(1, 3, by = 1),
#   #nbins = seq(20, 30, by = 10),
#   sample_rate = c(0.55, 0.632, 0.75))


hyper_params <- list(
  mtries = c(3,4,5),
  #min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30)
  #sample_rate = c(.55, .632, .70, .80)
)

search_criteria <- list(strategy = "RandomDiscrete", max_models = 10, seed = 1985)

rf_grid <- h2o.grid(x = vars, 
                    y = "first_down", 
                    training_frame = train, 
                    algorithm = "randomForest", 
                    grid_id = "rf_grid", 
                    hyper_params = hyper_params, 
                    nfolds = 10,
                    search_criteria = search_criteria,
                    #search_criteria = list(strategy = "Cartesian"),
                    seed=1985)

sortedGridRF <- h2o.getGrid("rf_grid", sort_by = "logloss", decreasing = FALSE)
sortedGridRF


cvrt_rf_tuned <- h2o.getModel(sortedGridRF@model_ids[[1]])



## training metrics ##
cvrt_rf_tuned

# ** 10-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
# MSE:  0.2228377
# RMSE:  0.4720568
# LogLoss:  0.6353183
# Mean Per-Class Error:  0.4083846
# AUC:  0.680128
# AUCPR:  0.6148861
# Gini:  0.3602561
# R^2:  0.09920983


## testing metrics ##
rf_tuned_cvrt <- h2o.performance(cvrt_rf_tuned,newdata = test)

rf_tuned_cvrt

#### TUNED RF Test Results ####
# H2OBinomialMetrics: drf
# 
# MSE:  0.2200219
# RMSE:  0.4690649
# LogLoss:  0.6288
# Mean Per-Class Error:  0.386387
# AUC:  0.6882507
# AUCPR:  0.6048617
# Gini:  0.3765014
# R^2:  0.1059465



#### RF RES - TUNED > BASE ####
## Tuned RF model has better logloss + AUC compared to base model. Use tuned RF for further model comparisions ##

### Tuned:

# Train:
## LogLoss - 0.640564
## AUC     - 0.6690426

# Test:
## LogLoss - 0.6288
## AUC     - 0.6882507



### Base:

# Train:
## LogLoss - 0.6406377
## AUC     - 0.6697238

# Test:
## LogLoss - 0.6303643
## AUC     - 0.6842987

#### <> ####
#### <> ####


#### Compare Top Models ####

#### RES - RF > GLM ####
## Comparable training results. GLM has slightly better AUC, RF has slightly better LogLoss
## RF test results outperform GLM test results slightly. LogLoss and AUC are better on RF than GLM


### TUNED GLM ###
glm_tuned_cvrt

h2o.varimp(cvrt_glm_tuned)
# Train:
## LogLoss - 0.6406732
## AUC     - 0.6696128

# Test:
## LogLoss - 0.6302859
## AUC     - 0.6845941



### TUNED RF ###
rf_tuned_cvrt

h2o.varimp(cvrt_rf_tuned)
# Train:
## LogLoss - 0.640564
## AUC     - 0.6690426

# Test:
## LogLoss - 0.6288
## AUC     - 0.6882507




#### View Predictions ####

test_preds <- test %>% as_tibble() %>% 
  bind_cols(
    h2o.predict(cvrt_rf_tuned,test) %>% as_tibble() %>% 
      mutate(prob_fail = p0,prob_succeed = p1) %>% 
      select(prob_fail,prob_succeed,predict)
  )



test_preds %>% 
  View()

test_preds %>% 
  filter(qtr %in% c(1,2,3,4),
         season %in% c(2018,2019,2020)) %>% 
  group_by(posteam) %>% 
  summarise(
    total_fdoe = sum(fdoe),
    avg_fdoe =  mean(fdoe)
  ) %>% 
  arrange(desc(total_fdoe)) %>% 
  View()


#### All 4th Downs ####
all_fourth_down_t1 <- pbp_db %>% 
  ## src - @benbaldwin
  filter(
    down %in% c(4), 
    qb_kneel == 0,
    !is.na(posteam),
    !is.na(yardline_100),
    !is.na(score_differential)
  ) %>% 
  ## src - @benbaldwin
  make_model_mutations()



all_fourth_down_t2 <- all_fourth_down_t1 %>% 
  mutate(yards_gained = 
           
           # src - @benbaldwin
           # we need a way to account for defensive penalties that give auto first downs
           # hacky "solution" is saying here that a penalty that gives a first down goes for the yards to go
           # unless the actual penalty yardage is higher
           
           # the drawback is that a defensive holding on eg 4th and 8 is coded as an 8 yard gain
           # the alternative is to estimate a separate model for penalties or have their own category
           # but first down penalties on 4th and long are very rare:
           # https://twitter.com/benbbaldwin/status/1322530446371074050
           
         case_when(
           first_down_penalty == 1 & penalty_yards < ydstogo ~ ydstogo, 
           first_down_penalty == 1 & penalty_yards >= ydstogo ~ penalty_yards, 
           TRUE ~ yards_gained
         ),
         
         # truncate to make model training easier -- add capping at -10 and 65 yards (src - @benbaldwin) #
         yards_gained = if_else(yards_gained < -10, -10, yards_gained),
         yards_gained = if_else(yards_gained > 65, 65, yards_gained),
         
         # Vegas expected points to be scored (src - @benbaldwin) #
         home_total = (spread_line + total_line) / 2,
         away_total = (total_line - spread_line) / 2,
         
         # Determine total/spread for possession team (src - @benbaldwin) #
         posteam_total = if_else(posteam == home_team, home_total, away_total),
         posteam_spread = dplyr::if_else(posteam == home_team, spread_line, -1 * spread_line)
  ) %>%
  
  # Select fields for training #  
  select(
    
    ## Match Back/ID Fields 
    play_id,posteam,season,
    first_down,
    
    ## @benbaldwin Selected Fields ##
    down,
    ydstogo,
    yardline_100,
    #era3, era4,
    outdoors, retractable, dome,
    posteam_spread, total_line, posteam_total,
    
    ## JB Selected Fields ##
    #era,
    week,yardline_100,game_seconds_remaining,season_type,
    qtr,wp,ep,ydstogo,shotgun,no_huddle,score_differential,
    desc
  )


fourth_down_test <- all_fourth_down_t2 %>% 
  filter(season >= 2020) %>% 
  collect()


fourth_down_test %>% View()
#h2o.shutdown(FALSE)




fourth_preds <- fourth_down_test %>% 
  bind_cols(
    h2o.predict(cvrt_rf_tuned,fourth_down_test %>% as.h2o()) %>% as_tibble() %>% 
      mutate(prob_fail = p0,prob_succeed = p1) %>% 
      select(prob_fail,prob_succeed,predict)
  )



fourth_preds %>% 
  View()






fourth_preds %>% 
  filter(rush == 1 || pass ==1)

























avg %>% 
  ggplot(aes(x = avg_yds_togo, y = avg_pass)) + 
  geom_image(aes(x = avg_yds_togo, y = avg_pass,image = team_logo_espn),asp = 16/9,size=.027) +
  geom_vline(xintercept = mean(avg$avg_pass), color = 'red', linetype = 'dotted') +
  geom_hline(yintercept = mean(avg$avg_yds_togo), color = 'red', linetype = 'dotted') +
  labs(
    y = "Pass Rate",
    x = 'Average Yards to Go',
    title = 'Average Pass Rate vs Average Yards to Go',
    subtitle = '2018 - 2020, Weeks 1-17, Quarters 1-5, 3rd/4th Downs, Rush + Pass Plays, FDOE calculated from Generalized Linear Model',
    caption = "Joey Bloom - @data_bears (Data - nflfastR)"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.caption = element_text(size = 7),
        legend.position = "none") 






avg %>% 
  ggplot(aes(x = total_fdoe, y = avg_total_rush_yds_gained)) + 
  geom_image(aes(x = total_fdoe, y = avg_total_rush_yds_gained,image = team_logo_espn),asp = 16/9,size=.027) +
  geom_hline(yintercept = mean(avg$avg_total_rush_yds_gained), color = 'red', linetype = 'dotted') +
  geom_vline(xintercept = mean(avg$total_fdoe), color = 'red', linetype = 'dotted') +
  labs(
    y = "Pass Rate",
    x = 'Total FDOE',
    title = 'Average Total Rush Yards Gained vs Total FDOE',
    subtitle = '2018 - 2020, Weeks 1-17, Quarters 1-5, 3rd/4th Downs, Rush + Pass Plays, FDOE calculated from Generalized Linear Model',
    caption = "Joey Bloom - @data_bears (Data - nflfastR)"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.caption = element_text(size = 7),
        legend.position = "none") 









avg %>% 
  ggplot(aes(x = avg_pass, y = avg_total_rush_yds_gained)) + 
  geom_image(aes(x = avg_pass, y = avg_total_rush_yds_gained,image = team_logo_espn),asp = 16/9,size=.027) +
  geom_hline(yintercept = mean(avg$avg_total_rush_yds_gained), color = 'red', linetype = 'dotted') +
  geom_vline(xintercept = mean(avg$avg_pass), color = 'red', linetype = 'dotted') +
  labs(
    x = "Pass Rate",
    y = 'Average Total Rush Yards Gained',
    title = 'Average Total Rush Yards Gained vs Pass Rate',
    subtitle = '2018 - 2020, Weeks 1-17, Quarters 1-5, 3rd/4th Downs, Rush + Pass Plays, FDOE calculated from Generalized Linear Model',
    caption = "Joey Bloom - @data_bears (Data - nflfastR)"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.caption = element_text(size = 7),
        legend.position = "none") 





avg %>% 
  ggplot(aes(x = avg_pass, y = total_fdoe)) + 
  geom_image(aes(x = avg_pass, y = total_fdoe,image = team_logo_espn),asp = 16/9,size=.027) +
  geom_hline(yintercept = mean(avg$total_fdoe), color = 'red', linetype = 'dotted') +
  geom_vline(xintercept = mean(avg$avg_pass), color = 'red', linetype = 'dotted') +
  labs(
    x = "Pass Rate",
    y = 'Total FDOE',
    title = 'Total FDOE vs Pass Rate',
    subtitle = '2018 - 2020, Weeks 1-17, Quarters 1-5, 3rd/4th Downs, Rush + Pass Plays, FDOE calculated from Generalized Linear Model',
    caption = "Joey Bloom - @data_bears (Data - nflfastR)"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.caption = element_text(size = 7),
        legend.position = "none") 
