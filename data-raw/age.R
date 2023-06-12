data(sat.act, package = "psych")
age <- sat.act %>%
  as_tibble() %>%
  select(age) %>%
  group_by(age) %>%
  summarize(freq = n()) %>%
  mutate(prop = freq/sum(freq))
usethis::use_data(age, overwrite = TRUE)
