#devtools::install_github("abresler/nbastatR")
library(nbastatR)
library(tidyverse)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

gamedata <- game_logs(seasons = 2024)
# Vector containing names of NBA teams in 2024 for each conference
eastern_conference <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
                        "Chicago Bulls", "Cleveland Cavaliers", "Detroit Pistons", "Indiana Pacers", 
                        "Miami Heat", "Milwaukee Bucks", "New York Knicks", "Orlando Magic", 
                        "Philadelphia 76ers", "Toronto Raptors", "Washington Wizards")

western_conference <- c("Dallas Mavericks", "Denver Nuggets", "Golden State Warriors", 
                        "Houston Rockets", "LA Clippers", "Los Angeles Lakers", 
                        "Memphis Grizzlies", "Minnesota Timberwolves", "New Orleans Pelicans", 
                        "Oklahoma City Thunder", "Phoenix Suns", "Portland Trail Blazers", 
                        "Sacramento Kings", "San Antonio Spurs", "Utah Jazz")

season_average_data <- gamedata |> 
  select(idGame, nameTeam, fgm, fga, fg3m, fg3a, fg2m, fg2a,tov, 
         pts,blk, stl, ast, treb, fta, ftm, outcomeGame) |>
  mutate(count_wins = if_else(outcomeGame == "W", 
                               1,
                               0),
         count_wins = as.numeric(count_wins)) |> 
  select(-outcomeGame) |> 
  group_by(idGame, nameTeam) |> 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(count_wins = if_else(count_wins > 0,
                              1,
                              0)) |> 
  select(-idGame) |> 
  group_by(nameTeam) |> 
  mutate(n_games = length(nameTeam)) |> 
  summarise(across(everything(), ~mean(.x, na.rm = TRUE))) |> 
  mutate(pct3 = 100*fg3m/fg3a,
         pct2 = 100*fg2m/fg2a,
         pctfg = 100*fgm/fga,
         pctft = 100*ftm/fta,
         ratio_ast_tov = ast/tov,
         count_wins = count_wins*n_games) |> 
  mutate(conference = case_when(nameTeam %in% eastern_conference ~ "Eastern Conference",
                                nameTeam %in% western_conference ~ "Western Conference"))


## 1. Distribution plots ----

#### a) Three pointers percentages ----

mean_pct3 <- mean(season_average_data$pct3)
median_pct3 <- median(season_average_data$pct3)

three_pointers_perc_hist <- ggplot(data = season_average_data, aes(x = pct3))+
  geom_histogram(fill = "#e75719", color = "#08052f", bins = 10)+
  geom_vline(xintercept = mean_pct3, 
             linetype = "dashed",
             color = "#08052f")+
  geom_vline(xintercept = median_pct3,
           linetype = "dotted",
           color = "#241aa5")

three_pointers_perc_hist

three_pointers_perc_density <- ggplot(data = season_average_data, aes(x = pct3))+
  geom_density(fill = "#e75719", color = "#08052f", alpha = 0.6)+
  geom_vline(xintercept = mean_pct3, 
             linetype = "dashed",
             color = "#08052f")+
  geom_vline(xintercept = median_pct3,
             linetype = "dotted",
             color = "#241aa5")
three_pointers_perc_density

#### b) field goals ----

field_goal_attempts_hist <- ggplot(data = season_average_data, aes(x = fga))+
  geom_histogram(fill = "#e75719", color = "#08052f", bins = 8, alpha = 0.67)+
  labs(x = "Field goal attempts",
       y = "Frequency",
       title = "Distribution of Field Goal Attempts per game",
       subtitle = "NBA Season 2023-2024 (60 games)",
       caption = "Source: Basketball Reference")+
  theme_bw()

field_goal_attempts_hist  

#### c) Field goal percentages all types ----


fg_pct_3 <- ggplot(data = season_average_data, aes(x = pct3))+
  geom_histogram(fill = "#8D5268", color = "#60615B", bins = 8, alpha = 0.67)+
  labs(x = "% 3's",
       y = "Frequency",
       caption = " ")+
  theme_bw()

fg_pct_2 <- ggplot(data = season_average_data, aes(x = pct2))+
  geom_histogram(fill = "#C16B76", color = "#60615B", bins = 8, alpha = 0.67)+
  labs(x = "% 2's",
       y = NULL,
       caption = " ")+
  theme_bw()

fg_pct_ft <- ggplot(data = season_average_data, aes(x = pctft))+
  geom_histogram(fill = "#D8988F", color = "#60615B", bins = 8, alpha = 0.67)+
  labs(x = "% Free Throws",
       y = NULL,
       caption = "Source: Basketball Reference")+
  theme_bw()

library(cowplot)
my_title <- ggdraw()+
  draw_label("Distribution of Field Goal Attempts per game",
             fontface = 'bold')
my_subtitle <- ggdraw()+
  draw_label("NBA Season 2023-2024 (60 games)", fontface = "plain")

plots_row <- plot_grid(fg_pct_3, fg_pct_2, fg_pct_ft, nrow = 1)

pct_all <- plot_grid(my_title,
                     my_subtitle,
                     plots_row,
                     nrow = 3, rel_heights = c(0.1, 0.1, 1))
pct_all

## 2. Comparative plots ----

#### a) Teams classification based on wins count ----

ppg_teams_barplot <- ggplot(data = season_average_data, aes(x = nameTeam,
                                                            y = count_wins))+
  geom_bar(stat = "identity")

ppg_teams_barplot

#let's order it with forcats::fct_reorder
ppg_teams_barplot_desc <- ggplot(data = season_average_data, aes(x = fct_reorder(nameTeam, count_wins, #.desc = T
                                                                                 ),
                                                            y = count_wins))+
  geom_bar(stat = "identity")

ppg_teams_barplot_desc

#let's exchange the axis to read the names of the teams: coord_flip()

clas_teams <- ggplot(data = season_average_data, aes(x = fct_reorder(nameTeam, count_wins),
                                                                 y = count_wins))+
  geom_bar(stat = "identity")+
  coord_flip()

clas_teams

clas_teams_color <- ggplot(data = season_average_data, aes(x = fct_reorder(nameTeam, count_wins),
                                                          y = count_wins))+
  geom_bar(stat = "identity",
           aes(fill = count_wins))+
  scale_fill_viridis_c()+
  coord_flip()

clas_teams_color

clas_teams_color_threes <- ggplot(data = season_average_data, aes(x = fct_reorder(nameTeam, count_wins),
                                                           y = count_wins))+
  geom_bar(stat = "identity",
           aes(fill = fg3a))+
  scale_fill_viridis_c()+
  coord_flip()

# divide it by conference 
clas_teams_color_threes <- ggplot(data = season_average_data, aes(x = fct_reorder(nameTeam, count_wins),
                                                                  y = count_wins))+
  geom_bar(stat = "identity",
           aes(fill = fg3a))+
  scale_fill_viridis_c()+
  coord_flip()+
  facet_wrap(~conference, scales = "free_y")+
  theme(legend.position = "bottom")+
  labs(x = "Number of wins",
       y = "Teams Ranking",
       fill = "Number of 3's attempts",
       caption = "Source: Basketball Reference")

clas_teams_color_threes 

