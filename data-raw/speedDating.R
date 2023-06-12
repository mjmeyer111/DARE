df <- read_csv("https://query.data.world/s/se3m2zybopwyhzuu7rj3pcdbhjrmlb", show_col_types = FALSE)
goal <- df %>%
  select(goal) %>%
  mutate(goal.f = factor(
    goal,
    labels = c("fun night out", "meet new people", "get date", "serious relationship", "to say I did", "other"))
  ) %>%
  na.omit() %>%
  arrange(goal)
speedDating <- goal %>% group_by(goal.f) %>%
  summarize(n = n()) %>%
  mutate(prop = n/sum(n))
usethis::use_data(speedDating, overwrite = TRUE)
