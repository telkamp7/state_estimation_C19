# Load libraries
library(readr)
library(KFAS)
library(glmmTMB)
library(tidyr)
library(dplyr)

# Load processed data 
newly_admitted <- read_rds(file = "./data/processed/processed_data.rds")

#### Total ####
# glmmTMB
m1.glmmTMB <- glmmTMB(formula = Total ~ 1 + ar1(drift + 0|group),
                      family = poisson(link = "log"),
                      data = newly_admitted)
write_rds(x = m1.glmmTMB, 
          file = "./src/models/m1.glmmTMB.rds", 
          compress = "xz")
## KFAS
mod1.KFAS <- SSModel(Total ~ 1 +
                      SSMarima(ar = 0, Q = 1),
                    data = newly_admitted, distribution = "poisson")
m1.out.KFAS <- KFS(model = mod1.KFAS,
                   filtering = c("state", "signal", "mean"),
                   nsim = 100)
write_rds(x = m1.out.KFAS,
          file = "./src/models/m1.out.KFAS.rds",
          compress = "xz")

#### Grouped by region ####
regional_newly_admitted <- read_rds(file = "./data/processed/processed_regional_data.rds")

# glmmTMB
m2.glmmTMB <- glmmTMB(formula = count ~ -1 + region + ar1(drift  + 0| region),
                      family = poisson(link = "log"),
                      data = regional_newly_admitted)
write_rds(x = m2.glmmTMB,
          file = "./src/models/m2.glmmTMB.rds",
          compress = "xz")

# KFAS
count_split <- split(regional_newly_admitted$count, regional_newly_admitted$region)
p <- length(count_split)
count <- matrix(unlist(count_split), ncol = p,
            dimnames = list(NULL, names(count_split)))

dataf <- split(regional_newly_admitted, regional_newly_admitted$region)

mod2.KFAS <- SSModel(formula = count ~ -1 +
                       SSMregression(rformula = rep(list(~ -1 + region),p),
                                     remove.intercept = FALSE,
                                     type = "common",
                                     data = dataf) +
                       SSMarima(ar = artransform(1)),
                     distribution = "poisson")

m2.out.KFAS <- KFS(model = mod2.KFAS,
                   nsim = 100)
m2.KFAS <- fitSSM(model = mod2.KFAS,
                  inits = rep(1, p+1),
                  method = "BFGS")
write_rds(x = m2.out.KFAS,
          file = "./src/models/m2.out.KFAS.rds",
          compress = "xz")
write_rds(x = m2.KFAS,
          file = "./src/models/m2.KFAS.rds",
          compress = "xz")
