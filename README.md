# Mixed_effects_QBs (Code Example)
**Mixed effects model using R** 

*The purpose of this is to show how to build easy and simple mixed-effects models. However, the model used in this example is a work in progress, so any feedback will be greatly appreciated*. 


**Packages**

- ```lme4``` is the package we will use for the mixed-effects model
- With ```plm``` we will create a fixed-effects just to analyze variables and models of interest. Both dplyr and plm have a function named lag() include ```dplyr::lag()``` <-the one we'll use
- ```broom.mixed``` allows us to extract info from the model

```
library(dplyr)
library(na.tools)
library(lme4) 
library(broom.mixed) 
library(ggplot2)
library(tidyr) 
library(plm) 
```
**Using nflfastR and nflscrapR data**

Data comes from nflfastR: https://github.com/mrcaseb/nflfastR. 

EPA models come from nflscrapR: https://github.com/maksimhorowitz/nflscrapR

**Get data**

I got this code from nflfastR's documentation. I also added variables: ```start``` and ```end``` to be used later in the graph. Other ways to get data is using the SQLite package as shown in Tom Mock's (@thomas_mock) awesome #HANIC x NFL panel presentation: https://jthomasmock.github.io/nfl_hanic/#1


``` 
start<-2016
end<-2019

seasons <- start:end
data <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})
```
**Pre-processing**

I wanted to add "previous play type" as a variable so I took this element from Tom's code to generate this feature. *The variable was not relevant* but I recommend using this code to generate other interesting lagged-variables or if you believe previous play might be useful for your analysis.

```
dataraw <- data  %>% 
  filter(play_type %in% c("run", "pass"), 
         penalty == 0,
         season_type == "REG", 
         !is.na(down), 
         !is.na(yardline_100)) %>% 
  mutate(in_red_zone = if_else(yardline_100 <= 20, 1, 0),
         in_fg_range = if_else(yardline_100 <= 35, 1, 0),
         run = if_else(play_type == "run", 1, 0),
         pass = if_else(play_type == "pass", 1, 0)) 

prev_play <- dataraw %>%
  group_by(game_id, posteam) %>%
  mutate(
    total_runs = if_else(play_type == "run",
                         cumsum(run) - 1, cumsum(run)
    ),
    total_pass = if_else(play_type == "pass",
                         cumsum(pass) - 1, cumsum(pass)
    ),
    previous_play = if_else(posteam == dplyr::lag(posteam),
                            dplyr::lag(play_type), "First play of Drive"
    ),
    previous_play = if_else(is.na(previous_play),
                            replace_na("First play of Drive"), previous_play
    )
  ) 
```
  Now we filter some stuff out
```
  data_filt <- prev_play %>%
  filter(!is_na(epa),
         !is.na(down),
         !is_na(passer_player_name),
         play_type=='pass')
```
Next, I selected and engineered a bunch of variables that I thought might be relevant. **This model is not yet finished**. Most probably I'll use or create more variables (most of the ones shown here won't make it). **Feedback is very much appreciated**. I will explain my YAC and QBYards variable once I publish the finished model.
```
epa_data<-data_filt %>% 
  select(game_id, play_id, epa,yac_epa, air_epa,ep,posteam,defteam,season,
         passer_player_name,complete_pass,yardline_100,ydstogo,down,quarter_seconds_remaining,
         half_seconds_remaining,game_seconds_remaining,yards_after_catch,air_yards,qb_hit,
         home_team, away_team,roof_type,desc,sack,qb_hit,yards_gained,previous_play) %>% 
  mutate(
    down = as.factor(down),
    under2minute = if_else(half_seconds_remaining < 120, 1, 0),
    half_seconds_remaining = scale(half_seconds_remaining),
    t=paste(season,game_id,play_id,sep=''),
    posid = paste(home_team,away_team,as.character(season),posteam,sep=''),
    defid = paste(home_team,away_team,as.character(season),defteam,sep=''),
    t = paste(season,game_id,play_id),
    away = if_else(home_team == posteam, 0,1),
    outdoor = if_else(roof_type=='OUTDOOR',1,0),
    roof = as.factor(roof_type),
    log_ydstogo = log(ydstogo),
    t = paste(play_id,game_id,season),
    converted = if_else(yards_gained - ydstogo>0,1,0),
    YAC = if_else(is.na(yards_after_catch),0,yards_after_catch),
    QBYards = if_else( is.na(air_yards) & yards_gained < 0 , yards_gained , if_else(is.na(air_yards),0,air_yards)),
    prev_play_run = if_else(previous_play=='run',1,0),
    first_play_drive = if_else(previous_play=='First play of Drive',1,0)
  ) 
```
This will come handy when filtering for players with n number of plays, you don't want guys with 5 career passes outperforming Payton Manning in your model:
```
plays_qb <- epa_data %>%
  group_by(passer_player_name) %>% 
  summarise(
    num_plays = n()
  );colnames(plays_qb)[1] <- 'level'
```
**And THIS is the mixed-effects model**. The random variable is specified this way:```+(1|passer_player_name)```, the other are fixed effects. To add more random-effects just add another ```+(1|name_of_variable)``` before ```,data=```. 
```
mixed<-lmer(epa ~ 
               yardline_100 +
               log_ydstogo*down +
               half_seconds_remaining +
               YAC +
               QBYards +
               away + 
               roof +
               (1|passer_player_name), data=epa_data)
```
Now let's: 
- Use ```broom.mixed``` to get our coefficients (random intercepts)
- Create ```QB``` factor
- Calculate t-statistics 

T-stat? Yes, you'll want to filter at a specific level of significance, just like when we filtered for n number of plays. You don't want one-hit wonders to outperform consistent-good players in your model. Here we want to make sure the intercept is statistically significant at a specific level. For this example I'll use 95% level which is z-score 1.96 (you will have to play around with this a lot, this is a good source: http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_HypothesisTest-Means-Proportions/BS704_HypothesisTest-Means-Proportions3.html) 
```
tt<-broom.mixed::tidy(mixed,effects="ran_vals") %>% 
  merge(plays_qb,by='level',all.x = TRUE,no.dups=TRUE) %>%
  filter(num_plays >500) %>%
  mutate(
    QB = as.factor(level),
    t_stat = estimate/std.error
  )
```
Here is where we filter based on significance, but first, we re-order and get n-numbers of players, just for graphing purposes. Finally change the ```z``` variable to select significance, as explained before.
```
tt<-tt[order(-tt$estimate),]
tt<-head(tt,n=32)
z <- 1.96
tt<-tt %>% filter(
  abs(t_stat) > z
) 
```
Finally we re-order again and graph. *If you are using only one season, change the code for ```subtitle=```*
```
tt<-tt[order(tt$estimate),]
tt %>%
  ggplot(aes(x=factor(QB, level = QB),estimate)) + 
  geom_point()+
  geom_pointrange(aes(ymin=estimate - z*std.error,
                      ymax=estimate + z*std.error))+
  coord_flip() +theme_bw() +
  labs(x = "Quarterback",
       y = "Random-effects estimate | EPA/play",
       caption = "Data from nflfastR | EPA models from nflscrapR 
       by Adrian Cadena @adrian_cadem",
       title = "Extra EPA/play generated by QB on Passing Plays 
Controlling for Home/Away and Roof Type",
       subtitle = paste(start,'to',end,"- Regular Season")) 
```
I didn't want this to be that long (it kinda is), so I'll make another repo with how to create fixed-effects models, and ways to test models using summary(), AIC, and anova testing. (Be careful with R^2 is good but can be misleading as hell)

For questions message me @adrian_cadem on Twitter. Thanks! 
