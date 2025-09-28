library(fpp3)
library(tidyverse)
m <- read.csv("Ecuador2017Results(in).csv")
View(m)

m2 <- m %>% select(Response.ID:Order, ends_with("fem"))
View(m2)

m3 <- m2 %>% pivot_longer(
  cols = ends_with("fem"),
  names_to = "Talker",
  values_to = "Fem"
)

#Look at final two columns
m3 %>% View()


#--------------

#Trying to use pivot_longer for every value column accept "eth" (It may cause problems as the only string value column)
t <- m %>% select(Response.ID:Order, ends_with(c("fem","nice","conf","class","urban","origin","edu","age")))
#The "." character is associated as a character space in string notation, so something more distinct like "_" is better
names(t) <- str_replace_all(names(t), '[.]', "_")
View(t)


#Makes 2,409 entries (wrong) instead of 1,752 entries (right)
#Likely because "origin" and "age" is found at the end of three columns: Language, Age, Origin
#The three columns because values in the "Talker" column --> 219 * 11 (8 values + 3 column names)
t2 <- t %>% pivot_longer(
  cols = ends_with(c("fem","nice","conf","class","urban","origin","edu","age")),
  names_to = c("Talker",".value"),
  names_sep = "[_]"
)
t2 %>% View()

#Task: Find a way to pick values columns in pivot_longer without picking other columns
  #Change column names?
  #Function/Specifier?
