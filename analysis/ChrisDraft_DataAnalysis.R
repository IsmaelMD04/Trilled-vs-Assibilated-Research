library(fpp3)
library(tidyverse)
library(ggmosaic)

###Data Cleaning (Wrangling)###


m <- read.csv("Ecuador2017Results(in).csv")

t <- m %>% 
  select(-c(Start.Date, End.Date, Order, Origin, Finished, Speaker, IP.Address, Duration..in.seconds., 
            Location.Latitude, Location.Longitude, ends_with(".eth"))) %>% 
  filter(Progress >= 100)

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

Ecuador_lang_cols <- read.csv("Ecuador_language_cols.csv")

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

#PCA Analysis
ratings <- t3[, c("fem", "nice", "conf", "class", "urban", "edu", "age")]
ratings_pca <- princomp(ratings, cor = TRUE)
summary(ratings_pca, loadings =  T)
screeplot(ratings_pca, type = 'lines', main = "Ratings Scree Plot")

