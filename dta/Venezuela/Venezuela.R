mi <- read_csv(file = "dta/Venezuela/Venezuela.csv")
miMod <- lm(inflation ~ money_supply_growth, data = mi) %>%
  augment()
miMod
mi1 <- miMod %>% filter(.cooksd < 1) %>%
  mutate(group = 1)
miMod1 <- lm(inflation ~ money_supply_growth, data = mi1) %>%
  augment()
mi2 <- miMod1 %>% filter(.cooksd < 1) %>%
  mutate(group = 2)
miMod2 <- lm(inflation ~ money_supply_growth, data = mi2) %>%
  augment()
mi3 <- miMod2 %>% filter(.cooksd < 1) %>%
  mutate(group = 3)
miAll <- bind_rows(mi, mi1, mi2, mi3) %>%
  select(inflation, money_supply_growth, group)
ggplot(data = miAll, aes(y = inflation, x = money_supply_growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(
    xlim = c(0, 200),
    ylim = c(0, 260)
  ) +
  labs(x = "annual money supply growth (%)",
       y = "annual inflation (%)") +
  facet_grid(. ~ group)
