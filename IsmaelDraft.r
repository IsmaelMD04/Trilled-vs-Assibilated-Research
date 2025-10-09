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
  names_sep = "_"
)

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
t3 <- t2 %>% mutate(RespondentOrigin = str_extract_all(RespondentOrigin,
                   regex(str_c(responder_Origins, collapse = "|"), 
                   ignore_case = TRUE)) %>% sapply((function(x) paste(unique(x),
                   collapse = ", "))))
t3 <- t3 %>%
  mutate(RespondentOrigin = na_if(RespondentOrigin, ""))
t3 %>% View()
