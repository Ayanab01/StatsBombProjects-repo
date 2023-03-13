install.packages("devtools")
install.packages("remotes")
install.packages("cli")
install.packages("purrr")
install.packages("extrafont")

remotes::install_version("SDMTools", "1.1-221")
devtools::install_github("statsbomb/StatsBombR")
devtools::install_github("FCrSTATS/SBpitch")
my_packages <- library()$results[,1]
head(my_packages, 10)

library(tidyverse)
library(devtools)
library(ggplot2)
library(StatsBombR)
library(SBpitch)

FAWSL <- FreeCompetitions()%>%
  filter(competition_id==37 & season_name=="2020/2021")

Matches <- FreeMatches(FAWSL)

StatsBombData_FAWSL <- free_allevents(MatchesDF = Matches,Parallel = T)

StatsBombData_FAWSL = allclean(StatsBombData_FAWSL)

names(StatsBombData_FAWSL)

shots_goals = StatsBombData_FAWSL %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name == "Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name == "Goal", na.rm = TRUE))


shots_goals_pg = StatsBombData_FAWSL %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name == "Shot", na.rm = TRUE)/n_distinct(match_id),
            goals = sum(shot.outcome.name == "Goal", na.rm = TRUE)/n_distinct(match_id))


ggplot(data = shots_goals_pg,
       aes(x = reorder(team.name,shots), y = shots)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(y = "Shots") +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_SB()


player_shots = StatsBombData_FAWSL %>%
  group_by(player.name, player.id) %>%
  summarise(shots = sum(type.name == "Shot", na.rm = TRUE))


player_minutes = get.minutesplayed(StatsBombData_FAWSL)

player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed))

player_shots = left_join(player_shots, player_minutes)

player_shots = player_shots %>% mutate(nineties = minutes/90)

player_shots = player_shots %>% mutate(shots_per90 = shots/nineties)






