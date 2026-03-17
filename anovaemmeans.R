bb <- ardata::beethoven_tempos
library(tidyverse)
bb <- bb |>
  as.tibble()
bb |>
  group_by(movement, mark, symphony, conductor, ptype) |>
  summarize(tempo = mean(tempo, na.rm = T))

bb <- bb |>
  mutate(symphony = factor(symphony),
         movement = factor(movement))

bb <- bb |>
  ungroup() |>
  mutate(markc = mark - median(mark))

library(lme4)
mod1 <- lmer(markc ~ symphony + ptype + tempo + (1|conductor), data = bb)
summary(mod1)
mod2 <- lmer(tempo ~ markc + ptype + (1|conductor), data = bb)
anova(mod2, mod1)
mod3 <- lmer(tempo ~ symphony * markc + ptype + (1|conductor), data = bb)
summary(mod3)
anova(mod3, mod1)

mod4 <- lmer(tempo ~ symphony * movement + ptype + markc + (1|conductor), data = bb)
summary(mod4)
anova(mod4, mod1)

emmeans::emmeans(mod4, ~ symphony * movement + markc, at = list(markc = 6))
emmeans::emmeans(mod4, ~ markc + ptype, at = list(markc = marks))

marks <- unique(bb$mark)
modmix <- lme4::lmer(tempo ~ mark * ptype + (1|conductor) + (1|symphony), data = bb)
edf <- emmeans::emmeans(modmix, ~ mark * ptype, at = list(mark = marks))
edf <- as.data.frame(edf)
ggplot(edf, aes(x = mark, y = emmean, color = ptype)) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 1, intercept = 0)

emmeans::emmeans(modmix, ~ mark, at = list(mark = marks))

