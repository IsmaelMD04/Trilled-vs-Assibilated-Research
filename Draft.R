library(fpp3)
library(tidyverse)
m <- read.csv("Ecuador2017Results(in).csv")

t <- m %>% select(-c(Start.Date, End.Date, IP.Address, Duration..in.seconds., Location.Latitude, 
                     Location.Longitude, eth, Order))
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
t2 %>% View()

t2 %>% select(eth) %>% unique() %>% View()

#This is a test

