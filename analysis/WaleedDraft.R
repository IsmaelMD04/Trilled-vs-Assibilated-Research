library(tidyverse)
library(ggmosaic)
library(fpp3)
library(nnet)
library(dplyr)
library(lme4)

t3 <- read.csv("data/Spanish_Survey_Clean.csv")

# Begin factor analysis 
# Apply regression on all dependent variables (fem - age, origin is kept categorical), 
# group/average based on sway towards the other variables, and assess the effect of trill or not 

# Invert femininity rating 
# Split speaker into speaker and speaker gender variable 

t4 <- t3 |> mutate(masculinity = 7 - fem)
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

# Factor 1: Status
# Factor 2: "Gendered Affect" or something 

t4 <- t4 |> mutate(status = rowMeans(scale(select(t4, class, urban, edu)), na.rm = TRUE))

# mixed effect/ random effect 
# linear model (status ~ trill + listener_id)
# 2 level logistic regression
# glm(response ~ predictors, family = 'binomial')
# create new variable that says if respondent correctly assigned speaker's origin 

#Created a new, simplier dataset with base columns
t5 <- t4 %>% select(c(Gender, trill, nice, conf, status, age, 
                      predictedOrigin, masculinity, speakerID, speaker_gender))

#A simply model describing nice in terms of trill, conf, and status and the interactions
mod <- lm(nice ~ (trill * conf + trill * status + trill * age + trill *masculinity), data = t5)
summary(mod)

#Ran stepAIC to improve the model.
mod2 <- MASS::stepAIC(mod, direction = "backward")
summary(mod2)
car::vif(mod2)

mod <- lm(nice ~ (trill * speakerID), data = t4)
summary(mod)

mod2 <- MASS::stepAIC(mod, direction = "backward")
summary(mod2)
car::vif(mod2)


mod <- lm(nice ~ trill, data = t5)
summary(mod)
