##### EDA

library(tidyverse)
library(tidymodels)
library(lubridate)
library(fitzRoy)
theme_set(theme_bw())

afl <- fetch_player_stats_fryzigg(season = 2011:2021)


# who finished in the top 5?
# what are their scores

aggregated_votes <- afl %>% 
  mutate(year = year(match_date)) %>% 
  filter(year != 2021) %>% 
  group_by(year, player_id, player_last_name) %>% 
  summarize(total = sum(brownlow_votes)) %>% 
  ungroup()
  
library(tidytext)
  
aggregated_votes %>% 
  group_by(year) %>% 
  slice_max(n = 5, order_by = total) %>% 
  mutate(year = as.factor(year),
         player_last_name = reorder_within(player_last_name, total, year)) %>% 
  ggplot(aes(total, player_last_name, fill = year)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ year, scales = "free", ncol = 3) +
  geom_text(aes(label = total), hjust = 1.2, size = 3)

# heat map for winner
# week by week votes for winner

winners <- aggregated_votes %>% 
  group_by(year) %>% 
  slice_max(order_by = total) %>% 
  ungroup() %>% 
  select(-total)

afl %>% 
  mutate(year = year(match_date)) %>% 
  inner_join(winners) %>% 
  filter(!str_detect(match_round, "Final")) %>% 
  mutate(match_round = as.numeric(match_round)) %>%
  ggplot(aes(match_round, year, fill = factor(brownlow_votes))) +
  geom_tile(color = "white") +
  scale_y_continuous(breaks = 2011:2020) +
  scale_x_continuous(breaks = 1:24, position = "top") +
  scale_fill_brewer(palette = "PuOr") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  labs(x = NULL,
       y = NULL,
       fill = "Brownlow Votes")

# position

afl %>% 
  mutate(voted = ifelse(brownlow_votes == 0, "no", "yes")) %>% 
  group_by(player_position) %>% 
  summarize(pct_voted = sum(voted == "yes") / n()) %>% 
  drop_na() %>% 
  mutate(player_position = fct_reorder(player_position, pct_voted)) %>% 
  ggplot(aes(pct_voted, player_position)) +
  geom_col()
  
afl %>% 
  mutate(
    position_group = case_when(
      player_position %in% c("C", "RR", "R", "WL", "WR") ~ "midfield",
      player_position %in% c("HBFR", "HBFL", "BPR", "CHB", "FB", "BPL") ~ "defense",
      player_position %in% c("FF", "CHF", "HFFR", "HFFL", "FPL", "FPR") ~ "forward",
      player_position == "RK" ~ "ruck",
      player_position %in% c("INT", "SUB") ~ "bench",
      TRUE ~ as.character(player_position)),
    voted = ifelse(brownlow_votes == 0, "no", "yes")) %>% 
  group_by(position_group) %>% 
  summarize(pct_voted = sum(voted == "yes") / n()) %>% 
  drop_na() %>% 
  mutate(position_group = fct_reorder(position_group, pct_voted)) %>% 
  ggplot(aes(pct_voted, position_group)) +
  geom_col()
