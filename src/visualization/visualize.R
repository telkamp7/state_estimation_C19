# Load libraries
library(broom)
library(broom.mixed)
library(readr)
library(KFAS)
library(glmmTMB)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(RColorBrewer)
library(purrr)
library(dotwhisker)

# Load processed data
newly_admitted <- read_rds(file = "./data/processed/processed_data.rds")
regional_newly_admitted <- read_rds(file = "./data/processed/processed_regional_data.rds")

# Number of admissions in total
admitted_plot <- newly_admitted %>%
  ggplot(mapping = aes(x = Dato, y = Total)) +
  geom_point() +
  labs(x = "Date [Days]", y = "Newly admitted [#]") +
  theme_bw()
ggsave(plot = admitted_plot,
       filename = "./reports/figures/newly_admitted_total.png",
       dpi = "retina", width = 8, height = 4, units = "in")

# Number of admissions separated into regions
admitted_reg_plot <- ggplot(data = regional_newly_admitted, mapping = aes(x = Dato, y = count)) +
  geom_point() +
  facet_wrap(facets = vars(region), nrow = 2, ncol = 3) +
  labs(x = "Date [Days]", y = "Newly admitted [#]") +
  theme_bw()
ggsave(plot = admitted_reg_plot, 
       filename = "./reports/figures/newly_admitted_regions.png",
       dpi = "retina", width = 12, height = 8, units = "in")

# Load trained models

# Total
m1.glmmTMB <- read_rds(file = "./src/models/m1.glmmTMB.rds")
m1.KFAS <- read_rds(file = "./src/models/m1.KFAS.rds")

mod.pred <- newly_admitted %>%
  mutate(glmmTMB = exp(predict(m1.glmmTMB)), KFAS = predict(m1.KFAS$model)[,"fit"]) %>%
  select(Dato, glmmTMB, KFAS) %>%
  pivot_longer(cols = c(glmmTMB, KFAS), names_to = "Model", values_to = "Prediction")

admitted_pred_plot <- admitted_plot +
  geom_line(data = mod.pred, mapping = aes(x = Dato, y = Prediction, colour = Model, linetype = Model), size = .8) +
  scale_color_brewer(palette = "Paired")
ggsave(plot = admitted_pred_plot, 
       filename = "./reports/figures/admitted_pred_plot.png",
       dpi = "retina", width = 8, height = 4, units = "in")

# Random effects

ranef.mod <- newly_admitted %>%
  mutate(glmmTMB = unlist(ranef(object = m1.glmmTMB)$cond$group),
         KFAS = coef(object = KFS(model = m1.KFAS$model))[,1]) %>%
  select(Dato, glmmTMB, KFAS) %>%
  pivot_longer(cols = -Dato, names_to = "Model", values_to = "Ranef")

latent_state_plot <- ggplot(data = ranef.mod, mapping = aes(x = Dato, y = Ranef, colour = Model, linetype = Model)) +
  geom_line(size = .8) +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Date [days]", y = expression(paste("Latent intensity, ", lambda))) +
  theme_bw()
ggsave(plot = latent_state_plot,
       filename = "./reports/figures/latent_state_plot.png",
       dpi = "retina", width = 8, height = 4, units = "in")

# Regional
m2.glmmTMB <- read_rds(file = "./src/models/m2.glmmTMB.rds")
m2.KFAS <- read_rds(file = "./src/models/m2.KFAS.rds")

dates <- regional_newly_admitted$Dato

reg_pred_KFAS <- predict(object = m2.KFAS$model)

regional_pred_KFAS <- reg_pred_KFAS %>%
  map_dfr(~ .x %>% as_tibble(), .id = "region") %>%
  rename(count = fit) %>%
  mutate(Dato = rep(unique(dates),6), Model = "KFAS") %>% 
  select(Dato, region, Model, count)

regional_pred_glmmTMB <- regional_newly_admitted %>%
  select(Dato, region) %>%
  mutate(glmmTMB = exp(predict(m2.glmmTMB))) %>%
  pivot_longer(cols = c(glmmTMB), names_to = "Model", values_to = "count")

regional_pred <- bind_rows(regional_pred_KFAS, regional_pred_glmmTMB)

test <- regional_pred %>%
  pivot_wider(names_from = c(region, Model),
              names_sep = ".",
              values_from = count)

admitted_reg_pred_plot <- admitted_reg_plot +
  geom_line(data = regional_pred, mapping = aes(x = Dato, y = count, colour = Model, linetype = Model), size = .8) +
  facet_wrap(facets = vars(region), nrow = 2, ncol = 3) +
  scale_color_brewer(palette = "Paired")
ggsave(plot = admitted_reg_pred_plot,
       filename = "./reports/figures/admitted_reg_pred_plot.png",
       dpi = "retina", width = 12, height = 8, units = "in")

# Random effects

test <- t(ranef(object = m2.glmmTMB)$cond$region)

ranef.mod <- newly_admitted %>%
  mutate(glmmTMB = unlist(ranef(object = m2.glmmTMB)$cond$Hovedstaden)) %>%
  select(Dato, glmmTMB) %>%
  pivot_longer(cols = -Dato, names_to = "Model", values_to = "Ranef")




