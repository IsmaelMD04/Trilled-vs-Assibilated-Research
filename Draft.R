library(fpp3)
m <- read.csv("Ecuador2017Results(in).csv")
View(m)

m2 <- m %>% select(Response.ID:Order, ends_with("fem"))
View(m2)

m3 <- m2 %>% pivot_longer(
  cols = ends_with("fem",),
  names_to = "Talker",
  values_to = "Fem"
)

#Look at final two columns
m3 %>% View()


#--------------

#Trying to use pivot_longer for two value columns "fem" and "nice"
t <- m %>% select(Response.ID:Order, ends_with("fem"), ends_with("nice"))
View(t)


#Makes 3,504 entries (wrong) instead of 1,752 entries (right)
t2 <- t %>% pivot_longer(
  cols = ends_with(c("fem","nice")),
  names_to = c("Talker",".value"),
  names_pattern = "(.*).(....)$",
) %>% View()
