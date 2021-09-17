###### XGBOOST

afl <- fetch_player_stats_fryzigg(season = 2015:2021) %>% 
  filter(match_round %in% as.character(1:23)) %>% 
  mutate(outcome = ifelse(match_winner == player_team, "win", "loss"),
         match_margin = ifelse(player_team == match_winner, match_margin, -match_margin)) %>% 
  select(match_id, match_date, player_id, player_first_name, player_last_name, match_margin,
         kicks:spoils, player_position, outcome)

train <- afl %>% 
  filter(year(match_date) < 2021) %>% 
  mutate(brownlow_votes = as.factor(brownlow_votes)) %>% 
  select(-supercoach_score)

test <- afl %>% filter(year(match_date) == 2021) %>% 
  select(-brownlow_votes, -supercoach_score)

set.seed(100)
afl_splits <- initial_split(train)
afl_train <- training(afl_splits)
afl_test <- testing(afl_splits)
afl_cv <- vfold_cv(afl_train, v = 5)

xg_spec <-  boost_tree(mode = "classification",
                       engine = "xgboost",
                       learn_rate = 0.02,
                       mtry = tune(),
                       trees = tune())

xg_rec <- recipe(brownlow_votes ~ ., data = afl_train) %>% 
  step_rm(match_id, match_date, player_id, player_first_name, player_last_name) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_nzv(all_predictors())

xg_wf <- workflow() %>% 
  add_model(xg_spec) %>% 
  add_recipe(xg_rec)

doParallel::registerDoParallel()

xg_tune <- xg_wf %>%
  tune_grid(resamples = afl_cv,
            metrics = metric_set(mn_log_loss),
            control = control_grid(save_pred = TRUE),
            grid = crossing(mtry = 3:6,
                            trees = seq(300, 800, 50)))

xg_tune %>% 
  autoplot()

xg_tune %>% 
  collect_metrics() %>% 
  arrange(mean)

xg_last <- xg_wf %>%
  finalize_workflow(select_best(xg_tune, "mn_log_loss")) %>%
  last_fit(afl_splits)

library(vip)
xg_last %>% 
  extract_workflow() %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)

xg_fit_final <- xg_wf %>%
  finalize_workflow(select_best(xg_tune)) %>%
  fit(train)

xg_pred <- xg_fit_final %>% 
  augment(test)

xg_pred %>% 
  select(contains("_name"), contains(".pred")) %>% 
  mutate(exp_votes = 0 * .pred_0 + 1 *.pred_1 + 2 * .pred_2 + 3 * .pred_3) %>% 
  group_by(player_first_name, player_last_name) %>% 
  summarize(exp_total_votes = sum(exp_votes)) %>% 
  arrange(-exp_total_votes)
