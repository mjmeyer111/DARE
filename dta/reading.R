reading <- tibble(
  n = 1:16,
  grade = rep(7:10, each = 4),
  ability = rep(1:4, 4),
  bb = c(59, 78, 83, 90, 70, 84, 91, 99, 62, 81, 83, 94, 72, 78, 88, 96),
  gt = c(40, 60, 69, 71, 46, 72, 79, 85, 53, 66, 78, 89, 47, 59, 75, 88),
  ti = c(19, 31, 42, 55, 31, 41, 54, 64, 30, 46, 56, 73, 39, 47, 66, 85),
  ld = c(26, 25, 39, 47, 30, 39, 53, 67, 26, 39, 48, 71, 29, 44, 55, 67),
  ta = c(16, 40, 46, 47, 30, 39, 53, 67, 26, 39, 48, 71, 29, 44, 55, 67),
  m = c(5, 13, 10, 27, 23, 17, 15, 31, 18, 11, 24, 41, 12, 10, 27, 47)
)
reading <- reading %>%
  rename("Black Beauty" = bb, "Gulliver's Travels" = gt, "The Iliad" = ti,
         "Lorna Doone" = ld, "The Aeneid" = ta, "Milton" = m) %>%
  pivot_longer(cols = 4:9, names_to = "books", values_to = "score") %>%
  mutate(books = factor(books,
                        levels = c("Milton", "The Aeneid", "Lorna Doone",
                                   "The Iliad", "Gulliver's Travels",
                                   "Black Beauty")),
         ability = factor(ability, levels = 4:1)) %>%
  select(-n)
ggplot(data = reading, aes(x = grade, y = score, color = ability)) +
  geom_line() +
  facet_grid(. ~ books) +
  theme(legend.justification = c(0.975, 0.025), legend.position = c(0.975, 0.025))

ggplot(data = reading, aes(x = grade, y = score, color = ability)) +
  geom_line() +
  facet_grid(books ~ .) +
  theme(legend.justification = c(0.975, 0.025), legend.position = c(0.975, 0.025))

