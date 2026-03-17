library(tidyverse)
library(ggmosaic)
library(fpp3)
library(nnet)
library(dplyr)

spanish <- read.csv("data/Spanish_Survey_Clean.csv")

## Visualization 
#Dumb down respondents and what region they're from so that it's not 8 sets for each respondent
respondents <- spanish %>% group_by(RespondentID) %>% reframe(Region = Region) %>% unique()

#install.packages("scales") <- used to convert proportions to percentages
#calculate how many speakers from each region + proportions
respondent_regions <- respondents %>% group_by(Region) %>% summarize(num_respondents = n()) %>% 
  mutate(proportion = (num_respondents/sum(num_respondents))) %>% arrange(proportion) %>%
  mutate(percentage = scales::percent(proportion))

#Pie chart
plotPie <- respondent_regions %>%
  ggplot(aes(x = "", y = proportion, fill = Region)) +
  geom_col() +
  geom_text(aes(label = percentage),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")
ggsave("images/Pie_chart_of_region.png", plot = plotPie)

# View unique respondents and gender
respondents_gender <- spanish %>%
  group_by(RespondentID) %>%
  summarize(Gender = dplyr::first(Gender), Region = dplyr::first(Region)) %>%
  ungroup()

gender_count <- respondents_gender %>%
  group_by(Gender) %>%
  summarize(num_respondents = n()) %>%
  mutate(proportion = num_respondents / sum(num_respondents), percentage = scales::percent(proportion))


# Simple bar chart to show comparison
plotGender <- gender_count %>% 
  ggplot(aes(x = Gender, y = num_respondents, fill = Gender)) + 
  geom_col() + 
  labs(title = "Respondents by gender", x = "Gender", y = "Number")
ggsave("images/Gender_distribution.png", plot = plotGender)


# Look at gender by region
gender_by_region <- respondents_gender %>%
  group_by(Region, Gender) %>%
  summarize(num_respondents = n(), .groups = "drop") %>%
  group_by(Region) %>%
  mutate(region_total = sum(num_respondents),
         proportion = num_respondents / region_total,
         percentage = scales::percent(proportion))


plotGenderByRegion <- gender_by_region %>%
  ggplot(aes(x = Region, y = proportion, fill = Gender)) +
  geom_col(position = 'fill') +
  labs(title = "Gender distribution by region", x = "Region", y = "Proportion")
ggsave("images/Gender_distribution_by_region.png", plot = plotGenderByRegion)


# Unique respondents and age 
respondents_age <- spanish %>%
  group_by(RespondentID) %>%
  summarize(age_group = dplyr::first(age), Region = dplyr::first(Region)) %>%
  ungroup() 

age_count <- respondents_age %>%
  group_by(age_group) %>%
  summarize(num_respondents = n()) %>%
  mutate(proportion = num_respondents / sum(num_respondents),
         percentage = scales::percent(proportion)) %>%
  arrange(age_group)


plotAge <- age_count %>% ggplot(aes(x = age_group, y = num_respondents, fill = age_group)) +
  geom_col() + labs(title = 'Respondents by Age', x = 'Age group', y = 'Count')
ggsave("images/Histogram_age_range.png", plot = plotAge)

# Adjust chart so that each age group is visible on the axis

# Age distribution by region
age_by_region <- respondents_age %>%
  group_by(Region, age_group) %>%
  summarize(num_respondents = n(), .groups = "drop") %>%
  group_by(Region) %>%
  mutate(region_total = sum(num_respondents),
         proportion = num_respondents / region_total,
         percentage = scales::percent(proportion))


plotAgeByRegion <- age_by_region %>% ggplot(aes(x = Region, y = proportion, fill = age_group)) + 
  geom_col(position = "fill") + labs(title = "Age distribution by region", x = "Region", y = "Proportion")
ggsave("images/Histogram_age_by_region.png", plot = plotAgeByRegion)


mosaicOrigin <- spanish %>% 
  ggplot() + geom_mosaic(aes(x = product(Region), fill = predictedOrigin),
                         offset = 0.015) +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Listener Region Compared to Predicted Speaker Origin")
ggsave("images/Listener_Region_Compared_to_Predicted_Speaker_Origin.png", 
       plot = mosaicOrigin)
