#devtools::install_github("abresler/nbastatR")
library(nbastatR)
library(tidyverse)
library(here)
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
write_rds(season_average_data, here("season_average_data.rds"))
write_csv(season_average_data, here("season_average_data.csv"))

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

## 2. Comparative barplots ----

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

#### b) Classified by conferences ----

# divide it by conference 
clas_teams_color_threes <- ggplot(data = season_average_data, aes(x = fct_reorder(nameTeam, count_wins),
                                                                  y = count_wins))+
  geom_bar(stat = "identity",
           aes(fill = fg3a))+
  scale_fill_viridis_c()+
  labs(y = "Number of wins",
       x = "Teams Ranking",
       fill = "Number of 3's attempts",
       caption = "Source: Basketball Reference")+
  coord_flip()+
  facet_wrap(~conference, scales = "free_y")+
  theme(legend.position = "bottom")
  

clas_teams_color_threes 

## 3. Comparative distributions ----

#### a) boxplots ----

all_teams_averages <- season_average_data |> 
  pivot_longer(cols = 2:22,
               names_to = "metric",
               values_to = "score")
write_rds(all_teams_averages, here("all_teams_averages.rds"))
write_csv(all_teams_averages, here("all_teams_averages.csv"))

# |> 
#   group_by(metric) |> 
#   summarise(mean = mean(score),
#             st_dev = sd(score),
#             se = st_dev/sqrt(30))

## boxplots of percentages
all_percentages <- all_teams_averages |>
  filter(metric %in% c("pct3", "pct2", "pctfg", "pctft"))

shooting_percentages_boxplots <- ggplot(all_percentages, aes(x = metric,
                                                             y = score))+
  geom_boxplot()

# let's customize a little bit

shooting_percentages_boxplots <- ggplot(all_percentages, aes(x = metric,
                                                             y = score))+
  geom_boxplot(aes(fill = metric), color = "gray37")+
  ggthemes::theme_clean()+
  scale_fill_manual(values = c("#C16B76", "#8D5268", "#749DA3","#D8988F"))+
  theme(legend.position = "none")+
  labs(x = "Shooting metric",
       y = "Percentage",
       title = "Shooting Percentages NBA Teams",
       subtitle = "Season 23/24 (60 games played)",
       caption = "Source: Basketball Reference")

shooting_percentages_boxplots

#### b) violinplots ----

shooting_percentages_violin <- ggplot(all_percentages, aes(x = metric,
                                                             y = score))+
  geom_violin(aes(fill = metric), color = "gray37", trim = F,
              draw_quantiles = T)+
  ggthemes::theme_clean()+
  scale_fill_manual(values = c("#C16B76", "#8D5268", "#749DA3","#D8988F"))+
  theme(legend.position = "none")+
  labs(x = "Shooting metric",
       y = "Percentage",
       title = "Shooting Percentages NBA Teams",
       subtitle = "Season 23/24 (60 games played)",
       caption = "Source: Basketball Reference")
shooting_percentages_violin

#### c) raincloud plots ----
shooting_percentages_raincloud<- ggplot(all_percentages, aes(x = metric,
                                                           y = score, fill = metric))+
  ggdist::stat_dist_halfeye(adjust = .5, width = .9, 
                            .width = 0.01,
                            justification = -0.05,
                            point_color = NA)+
  ggdist::stat_dots(
    side = "left", 
    justification = 1.01,
    binwidth = .3)+ 
  geom_boxplot(width = .2,
               outlier.shape = NA,
               color = "gray32",
               fill = "gray99", 
               alpha = .7)+
  ggthemes::theme_clean()+
  scale_fill_manual(values = c("#C16B76", "#8D5268", "#749DA3","#D8988F"))+
  theme(legend.position = "none")+
  labs(x = "Shooting metric",
       y = "%",
       title = "Shooting Percentages NBA Teams",
       subtitle = "Season 23/24 (60 games played)",
       caption = "Source: Basketball Reference")+
  coord_flip()
shooting_percentages_raincloud

## same but divide by conference
shooting_percentages_raincloud<- ggplot(all_percentages, aes(x = metric,
                                                             y = score, fill = metric))+
  ggdist::stat_dist_halfeye(adjust = .5, width = .9, 
                            .width = 0.01,
                            justification = -0.05,
                            point_color = NA)+
  ggdist::stat_dots(
    side = "left", 
    justification = 1.01,
    binwidth = .3)+ 
  geom_boxplot(width = .2,
               outlier.shape = NA,
               color = "gray32",
               fill = "gray99", 
               alpha = .7)+
  ggthemes::theme_clean()+
  scale_fill_manual(values = c("#C16B76", "#8D5268", "#749DA3","#D8988F"))+
  theme(legend.position = "none")+
  labs(x = "Shooting metric",
       y = "%",
       title = "Shooting Percentages NBA Teams",
       subtitle = "Season 23/24 (60 games played)",
       caption = "Source: Basketball Reference")+
  coord_flip()+
  facet_wrap(~conference)
shooting_percentages_raincloud

shooting_percentages_raincloud_v2<- ggplot(all_percentages, aes(x = metric,
                                                             y = score, fill = conference))+
  ggdist::stat_dist_halfeye(adjust = .5, width = .9, 
                            .width = 0.01,
                            justification = -0.05,
                            point_color = NA,
                            alpha = 0.6)+
  ggdist::stat_dots(
    side = "left", 
    justification = 1.01,
    binwidth = .1)+ 
  geom_boxplot(width = .2,
               outlier.shape = NA,
               alpha = .6 )+
  theme_bw()+
  scale_fill_manual(values = c("#C16B76", "#749DA3"))+
  theme(legend.position = "none")+
  labs(x = "Shooting metric",
       y = "%",
       title = "Shooting Percentages NBA Teams",
       subtitle = "Season 23/24 (60 games played)",
       caption = "Source: Basketball Reference")+
  coord_flip()+
  facet_wrap(~metric, scales = "free")
shooting_percentages_raincloud_v2

## 4. Scatterplots ----

#### a) ¿Is there a relationship between % field goals and count_wins?----

ggplot(data = season_average_data,
                                  aes(x = pctfg,
                                      y = count_wins))+
  geom_point()
  
pctfg_countwins_scatter <- ggplot(data = season_average_data,
                                  aes(x = pctfg,
                                      y = count_wins))+
  geom_point(aes(size = fg3a), alpha = .7)+
  geom_smooth(method = "lm")+
  ggrepel::geom_label_repel(aes(label = nameTeam))
pctfg_countwins_scatter

#### b) ¿Is there a relationship between three attempts and count_wins?----

threes_countwins_scatter <- ggplot(data = season_average_data,
                                  aes(x = fg3a,
                                      y = count_wins))+
  geom_point()+
  geom_smooth(method = "lm")+
  ggthemes::theme_few()

threes_countwins_scatter

#### c) Which players have worst ratio assists/turnovers? ----

players_data_pergame <- gamedata |>
  select(namePlayer, fgm, fga, fg3m, fg3a, fg2m, fg2a,tov, 
         pts,blk, stl, ast, treb, fta, ftm, minutes) |>
  group_by(namePlayer) |> 
  summarise_all(mean)

write_rds(players_data_pergame, here("players_data_pergame.rds"))


# draw a line in slope = 1
ggplot(data = players_data_pergame,
                                   aes(x = tov,
                                       y = ast))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")+
  ggthemes::theme_few()

# how can we avoid overlapping?
turnovers_assists_scatter <- ggplot(data = players_data_pergame,
                                    aes(x = tov,
                                        y = ast))+
  geom_point(aes(size = pts),
             alpha = .45)+
  geom_smooth(method = "lm")+
  ggthemes::theme_few()+
  ggrepel::geom_label_repel(aes(label = namePlayer))

# how if we want to highlight the top ten?
mvp_tracker <- c("Nikola Jokic", "Shai Gilgeous-Alexander", "Giannis Antetokounmpo", 
                 "Luka Doncic", "Jayson Tatum", "Domantas Sabonis", "Donovan Mitchell",
                 "Tyrese Haliburton", "Anthony Davis", "Jalen Brunson")

players_data_pergame_highlight <-  players_data_pergame |> 
  mutate(colorado = if_else(namePlayer %in% mvp_tracker,
                            "#BB673A",
                            "#60615B"))

turnovers_assists_scatter <- ggplot(data = players_data_pergame_highlight,
                                    aes(x = tov,
                                        y = ast))+
  geom_point(aes(size = pts,
                 color = colorado),
             alpha = .35)+
  geom_smooth(method = "lm",
              color = "darkcyan", 
              fill = "turquoise3")+
  geom_abline(intercept = 0, slope = 1, color = "#C16B76")+
  ggthemes::theme_few()+
  ggrepel::geom_label_repel(aes(label = namePlayer,
                                color = colorado))+
  scale_color_identity()


turnovers_assists_scatter

#### d) is there a nonlinear relationship between minutes played and points made due to players getting tired? ----
## let's take four top scorer players:
per_game_scorers <- gamedata |> 
  filter(namePlayer %in% c("Joel Embiid", "Luka Doncic",
                           "Shai Gilgeous-Alexander","Giannis Antetokounmpo",
                           "Donovan Mitchell", "Kevin Durant", "Jalen Brunson",
                           "Stephen Curry", "Devin Booker", "Jayson Tatum")) |> 
  select(namePlayer, idGame, dateGame, pts, minutes)
write_csv(per_game_scorers, here("per_game_scorers.csv"))

minutes_points_scorers <- ggplot(data = per_game_scorers,
                                    aes(x = minutes,
                                        y = pts))+
  geom_point(alpha = .35)+
  geom_smooth(method = "loess",
              color = "darkcyan", 
              fill = "#BB673A")+
  ggthemes::theme_few()
minutes_points_scorers

minutes_points_scorers_facet <- ggplot(data = per_game_scorers,
                                       aes(x = minutes,
                                           y = pts))+
  geom_point(alpha = .35)+
  geom_smooth(method = "loess",
              color = "darkcyan", 
              fill = "#BB673A")+
  facet_wrap(~namePlayer, scales = "free")+
  theme_bw()

minutes_points_scorers_facet

## all players?
per_game_scorers_all <- gamedata |> 
  select(namePlayer, idGame, pts, minutes)
write_rds(per_game_scorers_all, here("per_game_scorers_all.rds"))

ggplot(data = per_game_scorers_all,
                                       aes(x = minutes,
                                           y = pts))+
  geom_point(alpha = .35)+
  geom_smooth(method = "loess",
              color = "darkcyan", 
              fill = "#BB673A")+
  theme_bw()
       

minutes_points_scorers_all <- ggplot(data = per_game_scorers_all,
                                     aes(x = minutes,
                                         y = pts))+
  geom_point(alpha = .05, position = position_jitter(width = 0.5, height = 0.5),
             color = "#E3AF4A")+
  geom_smooth(method = "loess",
              color = "#28798C", 
              fill = "#E1D7B4")+
  ggdark::dark_theme_bw()
minutes_points_scorers_all           

#### e) line points per game ----

per_game_scorers

ggplot(data = per_game_scorers, aes(y = pts, x  = dateGame))+
  geom_point(color = "darkcyan")+
  geom_line(color = "darkcyan", linetype = "dashed")+
  facet_wrap(~namePlayer)+
  theme_bw()

