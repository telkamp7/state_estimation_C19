# Load libraries
library(readr)
library(KFAS)
library(glmmTMB)
library(tidyr)
library(dplyr)

# Load processed data 
newly_admitted <- read_rds(file = "./data/processed/processed_data.rds")


# glmmTMB
test.glmmTMB <- glmmTMB(formula = Total ~  diag(drift + 0| group),
                        family = poisson(link = "log"),
                        data = newly_admitted)

fixef(test.glmmTMB)
ranef.test <- ranef(object = test.glmmTMB)
plot(newly_admitted$drift, ranef.test$cond$group, type = "l")

plot(newly_admitted$Total)
lines(exp(predict(test.glmmTMB)), col = "red")

plot(residuals(test.glmmTMB))
acf(residuals(test.glmmTMB))

m1.glmmTMB <- glmmTMB(formula = Total ~ -1 + ar1(drift + 0 | group),
                      family = poisson(link = "log"),
                      data = newly_admitted)
fixef(object = m1.glmmTMB)
ranef.glmmTMB <- ranef(object = m1.glmmTMB)


plot(newly_admitted$drift, ranef.glmmTMB$cond$group, type = "l")

plot(newly_admitted$Total)
lines(exp(predict(m1.glmmTMB)), col = "red")

plot(residuals(m1.glmmTMB))
acf(residuals(m1.glmmTMB))


### Profile likelihood
pl <- profile(fitted = m1.glmmTMB, parm = 1, trace = 1)
library(ggplot2)
ggplot(data = pl, mapping = aes(x = .focal, y = sqrt(value))) +
  geom_point() +
  geom_line() +
  facet_wrap(~.par, scale = "free_x") +
  geom_hline(yintercept = 1.96, linetype = 2)


## KFAS
Zt <- matrix(c(1,0), 1, 2)
Tt <- matrix(c(1,0,1,1), 2, 2)
Rt <- matrix(c(1,0), 2, 1)
Qt <- matrix(NA)
P1inf <- diag(2)

test.KFAS <- SSModel(formula = Total ~ -1 +
                       SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, P1inf = P1inf),
                     distribution = "poisson", data = newly_admitted)
test.fit <- fitSSM(test.KFAS, inits = c(1), method = "BFGS")
out.fit <- KFS(test.fit$model)

ranef.KFAS <- coef(out.fit)[,1]

lines(coef(out.fit)[,1], col = "red")

par(mfrow = c(1,1))

plot(unlist(ranef.glmmTMB$cond$group)-ranef.KFAS)




m1.KFAS <- SSModel(formula = Total ~ drift + SSMarima(ar = 0, Q = 1), data = newly_admitted)

obj <- function(pars, model, estimate = TRUE){
  armod <- try(SSMarima(ar = artransform(pars[1]), Q = exp(pars[2])), silent = TRUE)
  if(class(armod) == "try-error"){
    return(Inf)
  } else {
    model["T", states = "arima"] <- armod$T
    model["Q", states = "arima"] <- armod$Q
    model["P1", states = "arima"] <- armod$P1
    if(estimate){
      -logLik(model)
    } else {
      model
    }
  }
}


fit <- optim(p = c(0.5, 1), fn = obj, model = m1.KFAS, method = "BFGS")

fit.KFAS <- fitSSM(model = m1.KFAS, inits = c(0, 1), updatefn = update_model,
                   method = "L-BFGS-B", lower = c(-1,0), upper = c(1, 100))



test <- newly_admitted %>%
  select(Dato:`Ukendt Region`, drift) %>%
  pivot_longer(cols = Hovedstaden:`Ukendt Region`, names_to = "region", values_to = "count") %>%
  mutate(region = as.factor(region))

m1 <- glmmTMB(formula = count ~ region + ar1(drift  + 0| region),
              family = poisson(link = "log"),
              data = test)
summary(m1)

tmp <- ranef(m1)

str(tmp)

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

