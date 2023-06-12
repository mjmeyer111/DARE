library(tidyverse)
library(readxl)
library(car)
library(effects)
path <- "/Users/mk/Dropbox/StatsBook/data/Myers3rdEd/Data Files/AgauasEtAl2020/ReedRepExp1_alldatashare.xlsx"
reed <- read_xlsx(path, range = cell_cols("A:K"))
reed <- reed %>%
  rename(
    near = `Near/Far`,
    up = `Up/Down`,
    rt = RT
    ) %>%
  mutate(
    near = factor(ifelse(near == 1, "Yes", "No")),
    up = factor(ifelse(up == 1, "Yes", "No"))
  ) %>%
  filter(CatchError == 0) %>%
  filter(Validity == 1) %>%
  select(-(Subject:TargSide), -(RespHand:CatchError))
reed
anova(reed.m1 <- lm(rt ~ near * up, data = reed))
boxCox(reed.m1)
reed <- reed %>%
  mutate(
    lrt = log(rt),
    lrtc = 1000 * (lrt - mean(lrt))
    )
anova(reed.m2 <- lm(lrtc ~ near * up, data = reed))
nearEffect <- as.numeric(Effect("near", reed.m2)$fit)
upEffect <- as.numeric(Effect("up", reed.m2)$fit)
reed.res <- residuals(reed.m2)
reed <- reed %>%
  mutate(nearEffect = ifelse(near == "No", nearEffect[1], nearEffect[2]),
         upEffect = ifelse(up == "No", upEffect[1], upEffect[2]),
         comparison = nearEffect * upEffect,
         residuals = residuals(reed.m2)
         )
anova(lm(data = reed, residuals ~ comparison))
