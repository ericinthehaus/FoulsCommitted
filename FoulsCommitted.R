library(tidyverse)
library(StatsBombR)

all_matches = FreeMatches( FreeCompetitions() )

# Womens world cup game ----

events = get.matchFree(matches)

fouls_events = events %>%
  filter( !is.na(foul_committed.card.name) ) %>%
  unnest_wider(col=location, names_sep = "_")

fouls_events 

fouls_events %>%
  ggplot( aes( x=location_1, y=location_2, label= minute) ) +
  theme_void() +
  annotate_pitchSB() +
  geom_label( color = "red") +
  coord_flip(xlim = c(-1,122) ) +
  annotate(geom="label", label = "possessing team going down ", x= 100, y=10)


# all free events ----

all_free_events <- StatsBombFreeEvents(MatchesDF = "ALL")

all_fouls <- all_free_events %>%
  filter( type.name== "Foul Committed") %>%
  unnest_wider(col= location, names_sep = "_") %>%
  replace_na(replace = list(foul_committed.type.name="other") ) 


## plot of red cards ----

view(
  all_free_events %>%
    filter(!is.na(bad_behaviour.card.name) ) %>%
    group_by(bad_behaviour.card.name) %>%
    summarise(n = n()),
  title = "cards for bad behavior"
) # how many cards for bad behavior


# all handballs ----
all_handballs <- all_free_events %>%
  unnest_wider(col= location, names_sep = "_")  %>%
  filter(foul_committed.type.name== "Handball") %>%
  group_by(foul_committed.card.name) %>%
  summarise(
    count = n(),
    avg_x = median(location_1),
    avg_y = median(location_2)
  )

all_handballs <- all_handballs %>% 
  ungroup() %>%
  replace_na(list(foul_committed.card.name="No card"))

all_handballs %>%
  ggplot( aes(avg_x, avg_y, label= foul_committed.card.name)) +
  theme_void( ) +
  annotate_pitchSB() +
  geom_text( color="red") +
  coord_flip(xlim = c(-1,120)) 

# all dangerous plays ----

all_dangerous <- all_free_events %>%
  unnest_wider(col= location, names_sep = "_")  %>%
  filter(foul_committed.type.name== "Dangerous Play") %>%
  group_by(foul_committed.card.name) %>%
  summarise(
    count = n(),
    avg_x = median(location_1),
    avg_y = median(location_2)
  )

all_dangerous <- all_dangerous %>% 
  ungroup() %>%
  replace_na(list(foul_committed.card.name="No card"))

all_dangerous %>%
  ggplot( aes(avg_x, avg_y, label= foul_committed.card.name)) +
  theme_void( ) +
  annotate_pitchSB() +
  geom_text( color="red") +
  coord_flip(xlim = c(-1,120)) 

# all red cards ----

red_card_fouls <- all_fouls %>%
  filter(foul_committed.card.name=="Red Card")

red_card_fouls %>%
  ggplot ( aes( x=location_1, y=location_2, 
              color=foul_committed.type.name,
              shape=foul_committed.type.name,
              label= season_id
              )
           ) +
  theme_void( ) +
  annotate_pitchSB() +
  geom_point(size=3) +
  geom_text(color= "black", size=7) +
  coord_flip(xlim = c(-1,120)) +
  labs(
    color= "Red Card Type",
    shape= "Red Card Type"
  ) +
  theme(
    legend.text.align = 0
  ) +
  scale_color_manual(
    values = c("red","green","gray")
  )

view(red_card_fouls %>%
       filter(foul_committed.type.name=="other") %>%
       select(period, minute, location_1,location_2, possession_team.name,
              team.name, position.name, match_id, season_id ), title = "other red cards" )


matches_redcards <- red_card_fouls %>%
  filter(foul_committed.type.name=="other") %>%
  select(period, minute, location_1,location_2, possession_team.name,
         team.name, position.name, match_id, season_id,
         foul_committed.type.name
         ) %>%
  inner_join(all_matches %>%
              select(match_date, match_id, competition.country_name, 
                     home_team.home_team_name, away_team.away_team_name
              ),
             by = join_by(match_id)
            )


