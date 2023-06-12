setwd("~/Dropbox/Stats Book/aggression")
library("ggplot2")
library("tidyverse")
library(MASS)
library(robustbase)
summary(agg <- read.table("agg.txt", header = TRUE))
ggplot(data = agg, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm", se = FALSE, col = "grey50", lty = 2) +
  geom_smooth(method = "ltsReg", se = FALSE, col = "red")
summary(ltsAgg <- ltsReg(data = agg, y ~ .))
plot(ltsAgg)
