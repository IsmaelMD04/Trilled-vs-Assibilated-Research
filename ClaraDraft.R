library(fpp3)
library(tidyverse)
library(ggmosaic)

### Cleaning


m <- read.csv('data/Ecuador2017Results(in).csv')

t <- m %>% 
  select(-c(Start.Date, End.Date, Order, Origin, Finished, Speaker, IP.Address, Duration..in.seconds., 
            Location.Latitude, Location.Longitude, ends_with(".eth"))) %>% 
  filter(Progress >= 74)

#The "." character is associated as a character space in string notation, so a more distinct "_" is better
names(t) <- str_replace_all(names(t), '[.]', "_")

#"[a-z]_[a-z]" = select columns that contains this pattern: lowerChar_lowerChar (like Daniel*a*_*f*em)
#Speaker = The speakers (Daniela, Diego...)
#.value = take the second part of the column name and use that as the value column(s)
#names_sep = "_" = divides column names into Talker and .value --> "Daniela_fem" --> Talker: Daniela / Value: fem

t2 <- t %>% pivot_longer(
  cols = matches("[a-z]_[a-z]", ignore.case = FALSE),
  names_to = c("Speaker",".value"),
  names_sep = "_") %>%
  rename(c(RespondentID = Response_ID, predictedOrigin = origin)) %>%
  mutate(RespondentID = match(RespondentID, RespondentID %>% unique())) %>%
  relocate(predictedOrigin, .after = last_col()) %>%
  mutate(predictedOrigin = str_replace_all(predictedOrigin %>% as.character(), 
                                           c("1" = "Quito", "2" = "Cuenca", "3" = "Loja", "4" = "Other")),
         trill = (str_replace_all(Speaker, c(
           "Sofia" = "0", "Andrea" = "0", "Andres" = "0", "Diego" = "0",
           "Isabel" = "1", "Daniela" = "1", "Carlos" = "1", "Pablo" = "1")
         )) %>% as.integer(),
         Speaker = str_replace_all(Speaker, c(
           "Sofia" = "F1", "Andrea" = "F2", "Andres" = "M1", "Diego" = "M2",
           "Isabel" = "F1", "Daniela" = "F2", "Carlos" = "M1", "Pablo" = "M2"))
  ) %>% 
  relocate(trill, .before = Speaker)

Ecuador_lang_cols <- read.csv("data/Ecuador_language_cols.csv")

#first parameter of setNames represent the content, second represents the value you want it to link to 
lang_replace <- setNames(Ecuador_lang_cols$Languages_Excluding_Spanish, Ecuador_lang_cols$language_old)

#create copy of t3 in case I mess up badly in previous attempts 
t3 <- t2

#imply that t3$language holds the names from lang_replace
t3$Languages_excluding_Spanish <- as.character(lang_replace[t3$Language])


#same steps but with quantity values
lang_replace_num <- setNames(Ecuador_lang_cols$Num_Additional_Languages, Ecuador_lang_cols$language_old)
t3$Num_Additional_Languages <- as.character(lang_replace_num[t3$Language])

t3 <- t3 %>% select(-Language) %>% relocate(Languages_excluding_Spanish, .after = Region) %>% 
  relocate(Num_Additional_Languages, .after = Languages_excluding_Spanish)


## Visualization 
#Dumb down respondents and what region they're from so that it's not 8 sets for each respondent
respondents <- t3 %>% group_by(RespondentID) %>% summarize(Region = Region) %>% unique()

#install.packages("scales") <- used to convert proportions to percentages
#calculate how many speakers from each region + proportions
respondent_regions <- respondents %>% group_by(Region) %>% summarize(num_respondents = n()) %>% 
  mutate(proportion = (num_respondents/sum(num_respondents))) %>% arrange(proportion) %>%
  mutate(percentage = scales::percent(proportion))

#Pie chart
respondent_regions %>% ggplot(aes(x = '', y = proportion, fill = Region)) +
  geom_col() +
  geom_text(aes(label = percentage),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = 'y')

# View unique respondents and gender
respondents_gender <- t3 %>%
  group_by(RespondentID) %>%
  summarize(Gender = dplyr::first(Gender), Region = dplyr::first(Region)) %>%
  ungroup()

gender_count <- respondents_gender %>%
  group_by(Gender) %>%
  summarize(num_respondents = n()) %>%
  mutate(proportion = num_respondents / sum(num_respondents), percentage = scales::percent(proportion))

# Simple bar chart to show comparison
gender_count %>% 
  ggplot(aes(x = Gender, y = num_respondents, fill = Gender)) + 
  geom_col() + 
  labs(title = "Respondents by gender", x = "Gender", y = "Number")

# Look at gender by region
gender_by_region <- respondents_gender %>%
  group_by(Region, Gender) %>%
  summarize(num_respondents = n(), .groups = "drop") %>%
  group_by(Region) %>%
  mutate(region_total = sum(num_respondents),
         proportion = num_respondents / region_total,
         percentage = scales::percent(proportion))

gender_by_region %>%
  ggplot(aes(x = Region, y = proportion, fill = Gender)) +
  geom_col(position = 'fill') +
  labs(title = "Gender distribution by region", x = "Region", y = "Proportion")

# Unique respondents and age 
respondents_age <- t3 %>%
  group_by(RespondentID) %>%
  summarize(age_group = dplyr::first(age), Region = dplyr::first(Region)) %>%
  ungroup() 

age_count <- respondents_age %>%
  group_by(age_group) %>%
  summarize(num_respondents = n()) %>%
  mutate(proportion = num_respondents / sum(num_respondents),
         percentage = scales::percent(proportion)) %>%
  arrange(age_group)


age_count %>% ggplot(aes(x = age_group, y = num_respondents, fill = age_group)) +
  geom_col() + labs(title = 'Respondents by Age', x = 'Age group', y = 'Count')
# Adjust chart so that each age group is visible on the axis

# Age distribution by region
age_by_region <- respondents_age %>%
  group_by(Region, age_group) %>%
  summarize(num_respondents = n(), .groups = "drop") %>%
  group_by(Region) %>%
  mutate(region_total = sum(num_respondents),
         proportion = num_respondents / region_total,
         percentage = scales::percent(proportion))

age_by_region %>% ggplot(aes(x = Region, y = proportion, fill = age_group)) + 
  geom_col(position = "fill") + labs(title = "Age distribution by region", x = "Region", y = "Proportion")

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

# Simpler dataset with base columns
t5 <- t4 %>% select(c(Gender, trill, nice, conf, status, age, 
                      predictedOrigin, masculinity, speakerID, speaker_gender))

# A simple model describing nice in terms of trill, conf, and status and the interactions
mod <- lm(nice ~ (trill + conf + status)^2 , data = t5)
summary(mod)

# Ran stepAIC to improve the model.
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
MASS::stepAIC(trillmod)

# reduced model
trillmod1 <- glmer(trill ~ status + (1 | RespondentID) + (1 | speakerID), data = t4, family = binomial)
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
