# Load libraries
library(readr)
library(glmmTMB)
library(tidyr)
library(dplyr)

# Load processed data 
newly_admitted <- read_rds(file = "./data/processed/processed_data.rds")



test <- newly_admitted %>%
  select(Dato:`Ukendt Region`, time) %>%
  pivot_longer(cols = Hovedstaden:`Ukendt Region`, names_to = "region", values_to = "count") %>%
  mutate(region = as.factor(region))

m1 <- glmmTMB(formula = count ~ ar1(time | region),
              family = poisson(link = "log"),
              data = test)
summary(m1)

m1$sdr

plot(resid(m1))

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
