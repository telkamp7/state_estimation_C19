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
                       SSMregression(rformula = rep(list(~ region),p),
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



tmp <- predict(m2.KFAS$model)
matplot(coef(object = KFS(model = m2.KFAS$model))[,7:12])
plot(tmp$Hovedstaden)
colMeans(count)


# P1 <- as.matrix(.bdiag(replicate(p, matrix(NA, 6, 6), simplify = FALSE)))

mod2.KFAS <- SSModel(formula = y ~ -1 + 
                       SSMregression(rep(list(~ region), p), type = "common", remove.intercept = FALSE, data = dataf) +
                       SSMarima(ar = 0, Q = diag(1, p)),
                     H = diag(NA, p))

update_mod2_KFAS <- function(pars, model){
  tmp <- SSMarima(ar = artransform(pars[1:p]), Q = diag(exp(pars[7+1:p]),p))
  model["R", states = "arima"] <- tmp$R
  model["Q", states = "arima"] <- tmp$Q
  model["P1", states = "arima"] <- tmp$P1
  model["H"] <- diag(exp(pars[13+1:p]), p)
  model
}

m2.KFAS <- fitSSM(model = mod2.KFAS,
                  inits = rep(1,p),
                  updatefn = update_mod2_KFAS,
                  method = "BFGS")
summary(m2.KFAS)
coef(object = m2.KFAS$model)



Zt <- diag(1,6)
Tt <- diag(0,6)
Qt <- matrix(NA, 6, 6)
P1inf <- matrix(NA, 6 , 6)

admissions <- newly_admitted  %>%
  select(Hovedstaden:`Ukendt Region`)
as.matrix(admissions)
mod2.KFAS <- SSModel(formula = as.matrix(admissions) ~ - 1 +
                      SSMcustom(Z = Zt, T = Tt, Q = Qt, P1 = P1inf),
                   distribution = "poisson")

updatefn <- function(pars, model){
  Q <- diag(exp(pars[1:6]))
  Q[upper.tri(Q)] <- pars[7:21]
  model["Q", etas = "level"] <- crossprod(Q)
  Q <- diag(exp(pars[22:27]))  
  Q[upper.tri(Q)] <- pars[28:42]
  model["Q", etas = "custom"] <- model["P1", states = "custom"] <- crossprod(Q)
  model
}

m2.KFAS <- fitSSM(model = mod2.KFAS,
                  inits = rep(0, 42),
                  updatefn = updatefn,
                  method = "BFGS")



diag(1:6)


init <- log(cov(as.matrix(admissions)))

m2.KFAS <- fitSSM(model = mod2.KFAS,
                  inits = rep(c(log(diag(init)), init[upper.tri(init)]),2),
                  method = "BFGS")



data("alcohol")

alcohol[, 1:4]
alcoholPred <- window(alcohol, start = 1969, end = 2007)
init <- chol(cov(log(alcoholPred[, 1:4] / alcoholPred[5:8]))/ 10)
plot(exp(tmp))




plot(tmp[[1]]$region)

hod <- matrix(nrow = length(tmp$cond$region), ncol = 6)

for(i in 1:nrow(hod)){
  hod[i,] <- tmp$cond$region[[i]]

}

matplot(hod[,1:5])

tmp[[1]]$region$time1


plot(resid(m1))

# 
newly_admitted

# define the model function
# f <- function()
m1 <- glmmTMB(formula = Total ~ ar1(drift + 0 | group),
              family = poisson(link = "log"),
              data = newly_admitted)
summary(m1)

tmp <- predict(m1)

plot(resid(m1))

acf(resid(m1))

formula(x = m1)


m2 <- update(m1, formula. = Total ~ Dato)

plot(resid(m2))



# m1.KFAS <- SSModel(formula = Total ~ drift + SSMarima(ar = 0, Q = 1), data = newly_admitted)
# 
# obj <- function(pars, model, estimate = TRUE){
#   armod <- try(SSMarima(ar = artransform(pars[1]), Q = exp(pars[2])), silent = TRUE)
#   if(class(armod) == "try-error"){
#     return(Inf)
#   } else {
#     model["T", states = "arima"] <- armod$T
#     model["Q", states = "arima"] <- armod$Q
#     model["P1", states = "arima"] <- armod$P1
#     if(estimate){
#       -logLik(model)
#     } else {
#       model
#     }
#   }
# }
# test.glmmTMB <- glmmTMB(formula = Total ~  diag(drift + 0| group),
#                         family = poisson(link = "log"),
#                         data = newly_admitted)
# 
# fixef(test.glmmTMB)
# ranef.test <- ranef(object = test.glmmTMB)
# plot(newly_admitted$drift, ranef.test$cond$group, type = "l")
# 
# plot(newly_admitted$Total)
# lines(exp(predict(test.glmmTMB)), col = "red")
# 
# plot(residuals(test.glmmTMB))
# acf(residuals(test.glmmTMB))
# data("alcohol")
# 
# deaths <- window(alcohol[, 2], end = 2007)
# population <- window(alcohol[, 6], end = 2007)
# ### Profile likelihood
# pl <- profile(fitted = m1.glmmTMB, parm = 1, trace = 1)
# library(ggplot2)
# ggplot(data = pl, mapping = aes(x = .focal, y = sqrt(value))) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~.par, scale = "free_x") +
#   geom_hline(yintercept = 1.96, linetype = 2)