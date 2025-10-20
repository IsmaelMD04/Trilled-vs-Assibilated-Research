library(fpp3)
library(tidyverse)
library(stringr)
m <- read.csv("Ecuador2017Results(in).csv")

t <- m %>% select(-c(Start.Date, End.Date, Finished, IP.Address, Duration..in.seconds., Location.Latitude, 
                     Location.Longitude, ends_with(".eth"), Order)) %>% filter(Progress >= 74)
#The "." character is associated as a character space in string notation, so a more distinct "_" is better
names(t) <- str_replace_all(names(t), '[.]', "_")


#Makes 1,752 entries with pivoted values! Nice
#"[a-z]_[a-z]" = select columns that contains this pattern: lowerChar_lowerChar 
#(like Daniel*a*_*f*em but NOT Longtitud*e*_*L*atitude [case sensitive])
#Talker = The speakers (Daniela, Diego...)
#.value = take the second part of the column name and use that as the value column(s)
#names_sep = "_" = divides column names into Talker and .value --> "Daniela_fem" --> Talker: Daniela and value column: fem
t2 <- t %>% pivot_longer(
  cols = matches("[a-z]_[a-z]", ignore.case = FALSE),
  names_to = c("Talker",".value"),
  names_sep = "_") %>% 
  mutate(Response_ID = match(Response_ID, Response_ID %>% unique())) %>%
  relocate(Region, .after = last_col())

responder_Origins <- c("Espaí", "Cariamanga", "Loja", "Guayaquil", 
                       "Honduras", "Machala", "Ambato", "Arenillas",
                       "Huaquillas", "Argentina", "Cariamanga",
                       "Catacocha", "Catamayo", "Zamora", "Quito", 
                       "Cuenca", "Duran", "EEUU", "USA", "Puerto Rico",
                       "Sucia", "San Juan Bosco", "Mexico", "Chile",
                       "Inglaterra", "Colombia", "El Salvador",
                       "Nicaragua", "Bolivia", "Pií", "Portovelo", 
                       "Saraguro", "Esmeraldas", "Guachapala", "Palanda",
                       "Gualaceo", "Guaranda", "Coimbra", "Ibarra", "Lago",
                       "Agrio", "Santa Rosa", "El Oro", "Míçchala", 
                       "Londres", "Macara", "Macas", "San Lucas", "Madrid",
                       "Manta", "Portoviejo", "Jipijapa", "Canton",
                       "Tulciç", "Puyo", "Riobamba",
                       "Puerto el Carmen del Putumayo", "Gualaquiza",
                       "Galíçpagos", "Latacunga", "Changaimina", "Oriente",
                       "Tegucigalpa", "Alemania", "Holanda", 
                       "Santo Domingo de los Tsíçchila", "Espana", "Costa",
                       "Selva Alegre", "Shell", "Zapotillo", "Zaruma")
t3 <- t2 %>% mutate(Origin = str_extract_all(Origin, 
                                             regex(str_c(responder_Origins, collapse = "|"), 
                                                   ignore_case = TRUE)) %>% sapply((function(x) paste(unique(x),
                                                                                                      collapse = ", "))))
t3 <- t3 %>%
  mutate(Origin = na_if(Origin, ""))
t3 %>% View()


#writing csv for unique values of language since there's so much of them since questions are in free response form (running this code resets the csv file so I commented it out)
#write.csv(data.frame(language_old = unique(t3$Language)), "Educador_language_cols.csv", row.names = FALSE)
Ecuador_lang_cols <- read.csv("Educador_language_cols.csv")

#first parameter of setNames represent the content, second represents the value you want it to link to 
lang_replace <- setNames(Ecuador_lang_cols$Languages_Excluding_Spanish, Ecuador_lang_cols$language_old)

#create copy of t3 in case I mess up badly in previous attempts 
t4 <- t3 

#imply that t4$language holds the names from lang_replace
t4$Languages_excluding_Spanish <- as.character(lang_replace[t4$Language])


#same steps but with quantity values
lang_replace_num <- setNames(Ecuador_lang_cols$Num_Additional_Languages, Ecuador_lang_cols$language_old)
t4$Num_Additional_Languages <- as.character(lang_replace_num[t4$Language])
view(t4)