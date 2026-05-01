library(fpp3)
library(tidyverse)
library(ggmosaic)

spanish <- read.csv("data/Spanish_Survey_Clean.csv")

t4 <- spanish |> mutate(masculinity = 7 - fem)
t4 <- t4 |> select(-fem)
t4 <- t4 |> mutate(speakerID = Speaker, 
                   speaker_gender = ifelse(str_starts(Speaker, 'F'), 'female', 'male'))
t4 <- t4 |> select(-Speaker)

# picking only numeric variables
fadata <- t4 |> select(masculinity, nice, class, urban, edu, age)
str(fadata)
colSums(is.na(fadata))
summary(fadata)

# dropping na
fadata <- fadata |> drop_na()

fa <- factanal(fadata, factors = 2, rotation = 'varimax')
fa
# class, urban, and edu are a status factor 
# higher masculinity = lower nice
# more feminine = higher nice
# age isn't strong 

# try new model with confidence 
fconf <- t4 |> select(masculinity, conf, nice, class, urban, edu, age)
fconf <- fadata |> drop_na()
cfa <- factanal(fconf, factors = 2, rotation = 'varimax')

# Factor 1: Status
# Factor 2: "Gendered Affect" or something 

t4 <- t4 |> mutate(status = rowMeans(scale(select(t4, class, urban, edu)), na.rm = TRUE))

# mixed effect/ random effect 
# linear model (status ~ trill + listener_id)
# 2 level logistic regression
# glm(response ~ predictors, family = 'binomial')
# create new variable that says if respondent correctly assigned speaker's origin 

# Simpler dataset with base columns
t5 <- t4 %>% select(c(Gender, trill, nice, conf, status, age, 
                      predictedOrigin, masculinity, speakerID, speaker_gender))

# A simple model describing nice in terms of trill, conf, and status and the interactions
mod <- lm(nice ~ (trill + conf + status)^2 , data = t5)
summary(mod)

# Ran stepAIC to improve the model.
mod2 <- MASS::stepAIC(mod, direction = "backward")
summary(mod2)

mod <- lm(nice ~ (trill * speakerID), data = t4)
summary(mod)

mod2 <- MASS::stepAIC(mod, direction = "backward")
summary(mod2)


mod <- lm(nice ~ trill, data = t5)
summary(mod)

library(lme4)

# get scores from fa back into data 
fa_scores <- factanal(fadata, factors = 2, rotation = 'varimax', scores = 'regression')
scores_df <- as_tibble(fa_scores$scores) |> rename(fa_status = Factor1, fa_gendered = Factor2)

# t6 new dataset with fa scores
t6 <- t4 |> drop_na(masculinity, nice, class, urban, edu, age) |> bind_cols(scores_df)
t6 <- t6 |> mutate(status = rowMeans(scale(select(t6, class, urban, edu)), na.rm = TRUE))

# mixed effects regression modeling, corrected with scores update
trillmod <- glmer(trill ~ 
                    status + 
                    masculinity + 
                    nice + speaker_gender + 
                    Gender + Region + 
                    (1 | RespondentID) + 
                    (1 | speakerID), data = t6, family = binomial)
n_distinct(t6$RespondentID)
summary(trillmod)
drop1(trillmod, test = "Chisq")

# reduced model
trillmod1 <- glmer(trill ~ status + (1 | RespondentID) + (1 | speakerID), data = t6, family = binomial)
summary(trillmod1)

# status predicted by trill 
# linear fixed effects model with status
# personality + status 
# plots: percieved origin, choose significant variables 
# box plots for linear models (percieved status, trill vs assibilated, maybe by speaker sex)

# compare full vs reduced trillmod
anova(trillmod1, trillmod)

# odds 
exp(fixef(trillmod1))
exp(confint(trillmod1, parm = "beta_", method = "Wald"))

# status ~ trill linear mixed model 
statusmod <- lmer(status ~ trill + (1 | RespondentID) + (1 | speakerID), data = t6)
summary(statusmod)
confint(statusmod, method = "Wald")

# personality ~ trill 
nicemod <- lmer(nice ~ trill + status + (1 | RespondentID) + (1 | speakerID), data = t6)
summary(nicemod)
confint(nicemod, method = "Wald")

mascmod <- lmer(masculinity ~ trill + status + (1 | RespondentID) + (1 | speakerID), data = t6)
summary(mascmod)
confint(mascmod, method = "Wald")

# seeing if participants can correctly identify the speaker origin 
# new data set with guessed quito variable 
t7 <- t6 |> mutate(guessed_quito = as.integer(predictedOrigin == "Quito"))

# proportion guessing quito by trill condition
t7 |>
  group_by(trill) |>
  summarize(n = n(), pct_guessed_quito = mean(guessed_quito, na.rm = TRUE))

# does trill affect guessing quito?
quitomod <- glmer(
  guessed_quito ~ trill + status + Gender + Region +
    (1 | RespondentID) + (1 | speakerID),
  data = t7, family = binomial)
summary(quitomod)
drop1(quitomod, test = "Chisq")

exp(fixef(quitomod))
exp(confint(quitomod, parm = "beta_", method = "Wald"))

# Does trill help listeners correctly identify Loja?
t8 <- t7 |> mutate(guessed_loja = as.integer(predictedOrigin == "Loja"))
guessed_loja = as.integer(t7$predictedOrigin == "Loja")
t8 |>
  group_by(trill) |>
  summarize(n = n(), pct_guessed_loja = mean(guessed_loja, na.rm = TRUE))

lojamod <- glmer(
  guessed_loja ~ trill + status + Gender + Region +
    (1 | RespondentID) + (1 | speakerID),
  data = t8, family = binomial)
summary(lojamod)
drop1(lojamod, test = "Chisq")

exp(fixef(lojamod))
exp(confint(lojamod, parm = "beta_", method = "Wald"))


# Region effect: do Quito listeners more readily map trill to Quito?
# Which listener regions most associate trill with Quito?
origin_by_region <- t7 |>
  group_by(Region, trill) |>
  summarize(
    n                = n(),
    pct_quito        = mean(guessed_quito, na.rm = TRUE),
    mean_status      = mean(status, na.rm = TRUE),
    mean_nice        = mean(nice, na.rm = TRUE),
    mean_masculinity = mean(masculinity, na.rm = TRUE),
    .groups          = "drop"
  )
print(origin_by_region)

# More questions: if not already answered
# What proportion of listeners accurately identify where the speakers are from?

# Do listeners from different regions (Loja vs.non-Loja) evaluate trilled vs. assibilated speakers differently?

# STATUS attitudes by trill × region
statusmod_int <- lmer(
  status ~ trill * Region + (1 | RespondentID) + (1 | speakerID),
  data = t7)
summary(statusmod_int)
confint(statusmod_int, method = "Wald")

# NICENESS attitudes by trill × region
nicemod_int <- lmer(
  nice ~ trill * Region + (1 | RespondentID) + (1 | speakerID),
  data = t7)
summary(nicemod_int)
confint(nicemod_int, method = "Wald")

# MASCULINITY attitudes by trill × region
mascmod_int <- lmer(
  masculinity ~ trill * Region + (1 | RespondentID) + (1 | speakerID),
  data = t7)
summary(mascmod_int)
confint(mascmod_int, method = "Wald")

# CORRECT LOJA IDENTIFICATION by trill × region
# (lojamod already has Region as main effect — now test interaction)
lojamod_int <- glmer(
  guessed_loja ~ trill * Region + status +
    (1 | RespondentID) + (1 | speakerID),
  data = t7, family = binomial)
summary(lojamod_int)
drop1(lojamod_int, test = "Chisq")

# SUMMARY TABLE: mean attitude ratings by region × trill condition
# This feeds directly into your visualizations
attitude_by_region <- t7 |>
  group_by(Region, trill) |>
  summarize(
    n                = n(),
    mean_status      = mean(status, na.rm = TRUE),
    mean_nice        = mean(nice, na.rm = TRUE),
    mean_masculinity = mean(masculinity, na.rm = TRUE),
    pct_guessed_loja = mean(guessed_loja, na.rm = TRUE),
    pct_guessed_quito = mean(guessed_quito, na.rm = TRUE),
    .groups = "drop"
  )
print(attitude_by_region)

# Save objects needed for visualization script
save(t7, fa, attitude_by_region, file = "data/viz_objects.RData")
