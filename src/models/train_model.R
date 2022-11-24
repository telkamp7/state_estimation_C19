# Load libraries
library(readr)
library(glmmTMB)

# Load processed data 
newly_admitted <- read_rds(file = "./data/processed/processed_data.rds")


# 
newly_admitted

# define the model function
# f <- function()
m1 <- glmmTMB(formula = Total ~ ar1(time + 0 | group),
              family = poisson(link = "log"),
              data = newly_admitted)
summary(m1)

tmp <- predict(m1)

plot(resid(m1))

acf(resid(m1))

formula(x = m1)


m2 <- update(m1, formula. = Total ~ Dato)

plot(resid(m2))
