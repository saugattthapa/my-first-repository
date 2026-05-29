
# COMP1013 Analytics Programming – Assignment
# Play Pulse Studios – Online Game Data Analysis
setwd("~/git-projects/my-first-repository")

# Required libraries
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(lubridate)

# Load data
players  <- read.csv("players.csv",  stringsAsFactors = FALSE)
sessions <- read.csv("sessions.csv", stringsAsFactors = FALSE)
games    <- read.csv("games.csv",    stringsAsFactors = FALSE)

# Quick sanity checks
cat("players rows:", nrow(players),  "| NAs:", sum(is.na(players)),  "\n")
cat("sessions rows:", nrow(sessions), "| NAs:", sum(is.na(sessions)), "\n")
cat("games rows:",    nrow(games),    "| NAs:", sum(is.na(games)),    "\n")


## Assumption: players with NA signup dates are excluded from analysis

# PART 1 – Step 1: Parse dates and assign experience levels
players$signup_date <- as.Date(players$signup_date)
players$signup_year <- year(players$signup_date)

players$experience_level <- case_when(
  players$signup_year <  2023 ~ "Veteran",
  players$signup_year == 2023 ~ "Intermediate",
  players$signup_year >  2023 ~ "New",
  TRUE                        ~ NA_character_
)

# Join sessions to players, drop any unclassifiable players
sessions_exp <- sessions %>%
  inner_join(players %>% select(player_id, experience_level),
             by = "player_id") %>%
  filter(!is.na(experience_level))

# PART 1 – Step 2: Summary table
part1_summary <- sessions_exp %>%
  group_by(experience_level) %>%
  summarise(
    num_players       = n_distinct(player_id),
    avg_play_time_min = round(mean(play_time_minutes, na.rm = TRUE), 2),
    avg_score         = round(mean(score,             na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(experience_level = factor(experience_level,
                                   levels = c("Veteran", "Intermediate", "New"))) %>%
  arrange(experience_level)

part1_summary %>%
  kable(col.names = c("Experience Level", "# Players",
                      "Avg Play Time (min)", "Avg Score"),
        caption = "Table 1: Player Engagement by Experience Level") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# PART 1 – Step 3: Visualisation
ggplot(part1_summary,
       aes(x = experience_level, y = avg_play_time_min, fill = experience_level)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = avg_play_time_min), vjust = -0.4, size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Average Play Time by Player Experience Group",
       x     = "Experience Level",
       y     = "Average Play Time (minutes)") +
  theme_minimal(base_size = 13)


# PART 2 – Step 1: Join sessions with games and group by genre
sessions_genre <- sessions %>%
  inner_join(games %>% select(game_id, genre), by = "game_id") %>%
  filter(!is.na(genre))

# PART 2 – Step 2: Summary table
part2_summary <- sessions_genre %>%
  group_by(genre) %>%
  summarise(
    avg_play_time_min  = round(mean(play_time_minutes, na.rm = TRUE), 2),
    avg_score          = round(mean(score,             na.rm = TRUE), 2),
    num_sessions       = n(),
    num_unique_players = n_distinct(player_id),
    .groups = "drop"
  ) %>%
  arrange(desc(num_unique_players))

part2_summary %>%
  kable(col.names = c("Genre", "Avg Play Time (min)", "Avg Score",
                      "# Sessions", "# Unique Players"),
        caption = "Table 2: Player Engagement by Game Genre") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# PART 2 – Step 3: Visualisation
ggplot(part2_summary,
       aes(x = reorder(genre, num_unique_players),
           y = num_unique_players, fill = genre)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = num_unique_players), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Number of Unique Players by Game Genre",
       x     = "Genre",
       y     = "Number of Unique Players") +
  theme_minimal(base_size = 13) +
  expand_limits(y = max(part2_summary$num_unique_players) * 1.1)


# PART 3 – Step 1: Identify top 10 players by total play time
player_total_time <- sessions %>%
  group_by(player_id) %>%
  summarise(total_play_time = sum(play_time_minutes, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(total_play_time)) %>%
  slice_head(n = 10)

top10_ids <- player_total_time$player_id

# PART 3 – Step 2: Summary table for top 10 players
part3_summary <- sessions %>%
  filter(player_id %in% top10_ids) %>%
  group_by(player_id) %>%
  summarise(
    total_sessions    = n(),
    avg_play_time_min = round(mean(play_time_minutes, na.rm = TRUE), 2),
    avg_score         = round(mean(score,             na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  left_join(player_total_time, by = "player_id") %>%
  arrange(desc(total_play_time))

part3_summary %>%
  select(player_id, total_sessions, avg_play_time_min,
         avg_score, total_play_time) %>%
  kable(col.names = c("Player ID", "Total Sessions",
                      "Avg Play Time (min)", "Avg Score",
                      "Total Play Time (min)"),
        caption = "Table 3: Top 10 Players by Total Play Time") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# PART 3 – Step 3: Boxplot of score distribution for top 10 players
sessions %>%
  filter(player_id %in% top10_ids, !is.na(score)) %>%
  mutate(player_id = factor(player_id)) %>%
  ggplot(aes(x = reorder(player_id, score, FUN = median),
             y = score, fill = player_id)) +
  geom_boxplot(outlier.size = 1.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title    = "Score Distribution for Top 10 Players",
       subtitle = "Players ordered by median score",
       x        = "Player ID",
       y        = "Score") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# PART 4 – Step 1: Create age groups and join with sessions
players_valid_age <- players %>%
  filter(!is.na(age) & age >= 16)

players_valid_age$age_group <- cut(
  players_valid_age$age,
  breaks = c(15, 25, 35, 45, Inf),
  labels = c("16-25", "26-35", "36-45", "46+"),
  right  = TRUE
)

sessions_age <- sessions %>%
  inner_join(players_valid_age %>% select(player_id, age_group),
             by = "player_id") %>%
  filter(!is.na(age_group))

# PART 4 – Step 2: Summary table
part4_summary <- sessions_age %>%
  group_by(age_group) %>%
  summarise(
    avg_play_time_min = round(mean(play_time_minutes, na.rm = TRUE), 2),
    avg_score         = round(mean(score,             na.rm = TRUE), 2),
    num_sessions      = n(),
    .groups = "drop"
  )

part4_summary %>%
  kable(col.names = c("Age Group", "Avg Play Time (min)",
                      "Avg Score", "# Sessions"),
        caption = "Table 4: Gaming Behaviour by Age Group") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# PART 4 – Step 3: Visualisation
ggplot(part4_summary,
       aes(x = age_group, y = avg_play_time_min, fill = age_group)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = avg_play_time_min), vjust = -0.4, size = 3.5) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  labs(title    = "Average Play Time by Age Group",
       subtitle = "Comparing engagement across player age brackets",
       x        = "Age Group",
       y        = "Average Play Time (minutes)") +
  theme_minimal(base_size = 13)