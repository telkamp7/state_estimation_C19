# Load libraries
library(readr)
library(KFAS)
library(glmmTMB)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(RColorBrewer)
library(purrr)

# Set theme globally for total plots
theme_set(
  theme_bw() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(x = 2, units = "line"),
          legend.position = "top")
  )

#### Total ####
# Load processed data
newly_admitted <- read_rds(file = "./data/processed/processed_data.rds")

# Number of admissions in total
admitted_plot <- newly_admitted %>%
  ggplot(mapping = aes(x = Dato, y = Total)) +
  geom_point() +
  labs(x = "Date [Days]", y = "Newly admitted [#]")
ggsave(plot = admitted_plot,
       filename = "./reports/figures/newly_admitted_total.png",
       dpi = "retina", width = 8, height = 4, units = "in")

# Load trained models
m1.glmmTMB <- read_rds(file = "./src/models/m1.glmmTMB.rds")
m1.out.KFAS <- read_rds(file = "./src/models/m1.out.KFAS.rds")

# Predictions
mod.pred <- newly_admitted %>%
  mutate(glmmTMB = predict(object = m1.glmmTMB, type = "response"), KFAS = m1.out.KFAS$muhat[,1]) %>%
  select(Dato, glmmTMB, KFAS) %>%
  pivot_longer(cols = c(glmmTMB, KFAS), names_to = "Model", values_to = "Prediction")

admitted_pred_plot <- admitted_plot +
  geom_line(data = mod.pred, mapping = aes(x = Dato, y = Prediction, colour = Model, linetype = Model), linewidth = .8) +
  scale_color_brewer(palette = "Paired")
ggsave(plot = admitted_pred_plot, 
       filename = "./reports/figures/admitted_pred_plot.png",
       dpi = "retina", width = 8, height = 4, units = "in")


# Random effects
mod.ranef <- newly_admitted %>%
  mutate(glmmTMB = unlist(ranef(object = m1.glmmTMB)$cond$group),
         KFAS = m1.out.KFAS$alphahat[,2]) %>%
  select(Dato, glmmTMB, KFAS) %>%
  pivot_longer(cols = -Dato, names_to = "Model", values_to = "Ranef")


latent_state_plot <- ggplot(data = mod.ranef, mapping = aes(x = Dato, y = Ranef, colour = Model, linetype = Model)) +
  geom_line(linewidth = .8) +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Date [days]", y = expression(paste("Random effect, ", u[t])))
ggsave(plot = latent_state_plot,
       filename = "./reports/figures/latent_state_plot.png",
       dpi = "retina", width = 8, height = 4, units = "in")

# Residuals
mod.res <- newly_admitted %>%
  mutate(glmmTMB = residuals(m1.glmmTMB, type = "response"), KFAS = residuals(m1.out.KFAS, type = "response")) %>%
  select(Dato, glmmTMB, KFAS) %>%
  pivot_longer(cols = -Dato, names_to = "Model", values_to = "Res")

residual_plot <- ggplot(data = mod.res, mapping = aes(x = Dato, y = Res, colour = Model, pch = Model)) +
  geom_point() +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Date [days]", y = expression(epsilon[t])) +
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave(plot = residual_plot,
       filename = "./reports/figures/residual_plot.png",
       dpi = "retina", width = 8, height = 4, units = "in")


#### Regional ####
# Set theme for regional plots
theme_set(
  theme_bw() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 15),
          strip.text = element_text(size = 18),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          legend.key.size = unit(x = 2, units = "line"),
          legend.position = "top")
)

# Load processed data
regional_newly_admitted <- read_rds(file = "./data/processed/processed_regional_data.rds")

# Number of admissions separated into regions
admitted_reg_plot <- ggplot(data = regional_newly_admitted, mapping = aes(x = Dato, y = count)) +
  geom_point() +
  facet_wrap(facets = vars(region), nrow = 2, ncol = 3) +
  scale_x_date(name = "Date [Days]", date_breaks = "9 months", date_labels = "%Y-%m") +
  labs(y = "Newly admitted [#]")
ggsave(plot = admitted_reg_plot, 
       filename = "./reports/figures/newly_admitted_regions.png",
       dpi = "retina", width = 12, height = 8, units = "in")

m2.glmmTMB <- read_rds(file = "./src/models/m2.glmmTMB.rds")
m2.KFAS <- read_rds(file = "./src/models/m2.KFAS.rds")
m2.out.KFAS <- read_rds(file = "./src/models/m2.out.KFAS.rds")

# Collect predictions from KFAS
mod.pred.regional.KFAS <- newly_admitted %>%
  mutate(KFAS.Hovedstaden = m2.out.KFAS$muhat[,1],
         KFAS.Midtjylland = m2.out.KFAS$muhat[,2],
         KFAS.Nordjylland = m2.out.KFAS$muhat[,3],
         KFAS.Sjælland = m2.out.KFAS$muhat[,4],
         KFAS.Syddanmark = m2.out.KFAS$muhat[,5],
         `KFAS.Ukendt Region` = m2.out.KFAS$muhat[,6]) %>%
  select(Dato, KFAS.Hovedstaden:`KFAS.Ukendt Region`) %>%
  pivot_longer(cols = KFAS.Hovedstaden:`KFAS.Ukendt Region`,
               names_sep = "\\.",
               names_to = c("Model", "region"),
               values_to = "count")

# Collect prediction from glmmTMB
mod.pred.regional.glmmTMB <- regional_newly_admitted %>%
  select(Dato, region) %>%
  mutate(glmmTMB = predict(object = m2.glmmTMB, type = "response")) %>%
  pivot_longer(cols = c(glmmTMB), names_to = "Model", values_to = "count") %>%
  select(Dato, Model, region, count)

# Gather all in one tibble
mod.pred.regional <- bind_rows(mod.pred.regional.KFAS, mod.pred.regional.glmmTMB)

# Predictions
admitted_reg_pred_plot <- admitted_reg_plot +
  geom_line(data = mod.pred.regional, mapping = aes(x = Dato, y = count, colour = Model, linetype = Model), linewidth = .8) +
  facet_wrap(facets = vars(region), nrow = 2, ncol = 3) +
  scale_color_brewer(palette = "Paired")
ggsave(plot = admitted_reg_pred_plot,
       filename = "./reports/figures/admitted_reg_pred_plot.png",
       dpi = "retina", width = 12, height = 8, units = "in")

# Random effects
ranef.glmmTMB.regional <- t(ranef(object = m2.glmmTMB)$cond$region)

mod.ranef.regional <- newly_admitted %>%
  select(Dato) %>%
  mutate(glmmTMB.Hovedstaden = ranef.glmmTMB.regional[,1],
         glmmTMB.Midtjylland = ranef.glmmTMB.regional[,2],
         glmmTMB.Nordjylland = ranef.glmmTMB.regional[,3],
         glmmTMB.Sjælland = ranef.glmmTMB.regional[,4],
         glmmTMB.Syddanmark = ranef.glmmTMB.regional[,5],
         `glmmTMB.Ukendt Region` = ranef.glmmTMB.regional[,6],
         KFAS.Hovedstaden = m2.out.KFAS$alphahat[,7],
         KFAS.Midtjylland = m2.out.KFAS$alphahat[,8],
         KFAS.Nordjylland = m2.out.KFAS$alphahat[,9],
         KFAS.Sjælland = m2.out.KFAS$alphahat[,10],
         KFAS.Syddanmark = m2.out.KFAS$alphahat[,11],
         `KFAS.Ukendt Region` = m2.out.KFAS$alphahat[,12]) %>%
  pivot_longer(cols = -Dato,
               names_sep = "\\.",
               names_to = c("Model", "region"),
               values_to = "count")

# Latent state
latent_state_reg_plot <- ggplot(data = mod.ranef.regional, aes(x = Dato, y = count, colour = Model, linetype = Model)) +
  geom_line(linewidth = .8) +
  facet_wrap(facets = vars(region)) +
  scale_color_brewer(palette = "Paired") +
  scale_x_date(name = "Date [Days]", date_breaks = "9 months", date_labels = "%Y-%m") +
  labs(y = expression(paste("Random effect, ", u[t]^r)))
ggsave(plot = latent_state_reg_plot,
       filename = "./reports/figures/latent_state_reg_plot.png",
       dpi = "retina", width = 12, height = 8, units = "in")


# Residuals
m2.res.KFAS <- residuals(m2.out.KFAS, type = "response")

mod.res.regional.KFAS <- newly_admitted %>%
  mutate(KFAS.Hovedstaden = m2.res.KFAS[,1],
         KFAS.Midtjylland = m2.res.KFAS[,2],
         KFAS.Nordjylland = m2.res.KFAS[,3],
         KFAS.Sjælland = m2.res.KFAS[,4],
         KFAS.Syddanmark = m2.res.KFAS[,5],
         `KFAS.Ukendt Region` = m2.res.KFAS[,6]) %>%
  select(Dato, KFAS.Hovedstaden:`KFAS.Ukendt Region`) %>%
  pivot_longer(cols = KFAS.Hovedstaden:`KFAS.Ukendt Region`,
               names_sep = "\\.",
               names_to = c("Model", "region"),
               values_to = "residual")

# Collect residuals from glmmTMB
mod.res.regional.glmmTMB <- regional_newly_admitted %>%
  select(Dato, region) %>%
  mutate(glmmTMB = residuals(m2.glmmTMB, type = "response")) %>%
  pivot_longer(cols = c(glmmTMB), names_to = "Model", values_to = "residual") %>%
  select(Dato, Model, region, residual)

# Gather all in one tibble
mod.res.regional <- bind_rows(mod.res.regional.glmmTMB,mod.res.regional.KFAS)

# Residuals
residual_reg_plot <- ggplot(data = mod.res.regional, mapping = aes(x = Dato, y = residual, colour = Model, pch = Model)) +
  geom_point() +
  facet_wrap(facets = vars(region)) +
  scale_color_brewer(palette = "Paired") +
  scale_x_date(name = "Date [Days]", date_breaks = "9 months", date_labels = "%Y-%m") +
  labs(y = expression(epsilon[t]))  +
    guides(color = guide_legend(override.aes = list(size = 3)))
ggsave(plot = residual_reg_plot,
       filename = "./reports/figures/residual_reg_plot.png",
       dpi = "retina", width = 12, height = 8, units = "in")


