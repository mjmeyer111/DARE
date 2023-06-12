# Requires tidyverse and janitor
searchConjunction <- read_tsv("./dta/Wolfedata/RVvRHGV.tsv",
                              show_col_types = FALSE) %>%
  select(-Condition) %>%
  janitor::clean_names() %>%
  rename(
    trial = trial_number,
    setSize = setsize,
    target = targ_pres,
    sdt = message,
    rTime = rt_ms
  ) %>%
  mutate(
    subject = factor(subject),
    setSize = as.integer(setSize),
    target = factor(if_else(target == 1, "Present", "Absent")),
    error = factor(if_else(error == 1, "Yes", "No")),
    sdt = factor(
      case_when(
        sdt == "HIT" ~ "H",
        sdt == "TNEG" ~ "CR",
        sdt == "FA" ~ "FA",
        sdt == "MISS" ~ "M"
      )
    ),
    setSize.f = factor(setSize)
  ) %>%
  relocate(
    setSize.f,
    .after = setSize
  )
usethis::use_data(searchConjunction, overwrite = TRUE)
