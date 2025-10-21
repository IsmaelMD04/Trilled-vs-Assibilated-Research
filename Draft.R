library(fpp3)
library(tidyverse)
library(stringr)
m <- read.csv("Ecuador2017Results(in).csv")

t <- m %>% 
  select(-c(Start.Date, End.Date, Finished, Speaker, IP.Address, Duration..in.seconds., 
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
  rename(c(RespondentID = Response_ID, RespondentBackground = Origin), predictedOrigin = origin) %>%
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

t3 <- t2 %>% mutate(
  RespondentBackground = 
    str_extract_all(RespondentBackground, regex(str_c(responder_Origins, collapse = "|"), ignore_case = TRUE)) %>%
    sapply((function(x) paste(unique(x), collapse = ", ")))
)

t3 <- t3 %>%
  mutate(RespondentBackground = na_if(RespondentBackground, ""),
         RespondentBackground = coalesce(RespondentBackground, Region))

t3 %>% View()