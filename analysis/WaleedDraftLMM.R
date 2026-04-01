library(car)
library(fpp3)


##### New Package!
#install.packages("brms")
library(brms)
##### New Package!

spanish <- read.csv("data/Spanish_Survey_Clean.csv")


model <- multinom(predictedOrigin ~ trill + fem + nice + conf + class +
           urban + edu + age + Speaker, data = spanish)



vif(model)
#Outcome:
#Working model, but multiple 10+ VIF values ---> Use Factanal Analysis to group similar variables.


# picking only numeric variables
fadata <- spanish |> select(fem, nice, conf, class, urban, edu, age)

#EDA (Exploratory Data Analysis)
str(fadata)
colSums(is.na(fadata))
summary(fadata)

#Dropping N/A Values
fadata <- fadata |> drop_na()

#Factor Analysis
fa <- factanal(fadata, factors = 2, rotation = 'varimax', scores = "regression")

#Outcomes:
#Factor1: class, urban, and edu ~> Prestige
#Factor2: fem, nice, and conf ~> Personality
#Age is completely unique.

#Replace relevant variables with factors
spanish <- spanish %>% 
  mutate(
    Prestige = fa$scores[,1],
    Personality = fa$scores[,2]
    ) %>%
  select(
    -c(fem, nice, conf, class, urban, edu, predictedOrigin),
    predictedOrigin #Ensures predictedOrigin is the last column
  )

#Try the model again
model <- multinom(predictedOrigin ~ trill + Prestige + Personality + age + Speaker, data = spanish)

vif(model)
#Outcomes:
#Great, age variable is somewhat concerning (VIF = 5.46)


mod2 <- MASS::stepAIC(model, direction = "both")
mod2
#Outcomes:
#Removes age
#Prestige has a HUGE impact, followed by trill (nice), then Personality and so on.


#Next steps: 
#Find interpretations from the summary below
summary(mod2)
#Build a multinominal linear model with more variables.
#Look into using brms to introduce random variables into the MLM.
