library(fpp3)
library(RCurl)
m <- read.csv("Ecuador2017Results(in).csv")
 

View(m)

m2 <- m %>% select(Response.ID:Order, ends_with("fem"))
View(m2)

m3 <- m2 %>% pivot_longer(
  cols = ends_with("fem"),
  names_to = "Talker",
  values_to = "Fem"
)

m %>% pivot_longer(
  cols = ends_with("fem"),
  names_to = "Talker",
  values_to = "Fem"
) %>% View()

?pivot_longer()


View(m3)
view(m2)
m2 <- m %>% pivot_longer(
  cols = ends_with("fem"), 
  names_to = c("femininity","niceness","confidence","class","urban","origin","education"),
  )
                                                               
                                                               
                                                               
                                                               ,
                         names_prefix = "fem", values_to = "count")


m %>% pivot_longer(cols = Daniela.fem:Andres.edu)


m2 %>% select(c(femininity,count))

m2
pivot_longer(
  cols = starts_with("wk"),
  names_to = "week",
  names_prefix = "wk",
  values_to = "rank",
  values_drop_na = TRUE
)


relig_income %>% pivot_longer(!religion, names_to = "income", values_to = "count")
