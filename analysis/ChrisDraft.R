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

#PCA Analysis
ratings <- t5[, c("trill", "nice", "conf", "status", "age", "masculinity")]
ratings_pca <- princomp(ratings, cor = TRUE)
summary(ratings_pca, loadings =  T)
screeplot(ratings_pca, type = 'lines', main = "Ratings Scree Plot")

mod <- glm(trill ~ nice + conf + status + masculinity, family = "binomial", data = t5)
summary(mod)


t5$PC1 <- ratings_pca$scores[,1]
t5$PC2 <- ratings_pca$scores[,2]

mod2 <- glm(trill ~ PC1 + PC2, family = "binomial", data = t5)
summary(mod2)

##Analysis:
# PC2 had a much strong effect on trill, PC1 is small, but significant
# Trill is strongly associated with youth/style, and only weakly associated with prestige.
# PC2, which corresponds to a youth/style-related dimension of social perception, was the strongest predictor of trill realization. This suggests that trill is primarily associated with stylistic or age-related social meaning.
# PC1, representing a prestige/competence dimension, showed a smaller positive effect, indicating that trill may also carry a weak association with prestige-related evaluations.

#What to work on:

