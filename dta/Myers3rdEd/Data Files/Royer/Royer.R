path <- "/Users/mk/Dropbox/StatsBook/data/Myers3rdEd/Data Files/math/math Data.sav"
library(tidyverse)
library(haven)
math.raw <- read_sav(path)
math <- math.raw %>%
  rename(
    sex = GENDER,
    grade = GRADE,
    subtract_accuracy = SUBACC,
    subtract_rt = SUBRT,
    multiply_accuracy = MULTACC,
    multiply_rt = MULTRT,
    add_accuracy = ADDACC,
    add_rt = ADDRT,
    student = ID
  ) %>%
  select(-ACC, -GROUP, -SPEEDMRT) %>%
  mutate(
    sex = factor(sex),
    student = factor(student)
  ) %>%
  relocate(student, .before = 1)
mathLong <- math %>%
  pivot_longer(
   cols = subtract_accuracy:add_rt,
   names_to = c("task", "measure"),
   names_sep = "_"
  ) %>%
  drop_na()
mathLong <- mathLong %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  mutate(
    task = factor(task, levels = c("add", "subtract", "multiply")),
    task = fct_recode(
      task, Add = "add", Subtract = "subtract", Multiply = "multiply"
      )
  )
mathLong
anova(lm(rt ~ sex * grade * task, data = mathLong))
