# Load libraries
library(readr)
library(KFAS)
library(glmmTMB)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(RColorBrewer)

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

tmp <- predict(object = m2.KFAS$model)

regional_pred <- regional_newly_admitted %>%
  select(Dato, region) %>%
  mutate(glmmTMB = exp(predict(m2.glmmTMB))) %>%
  pivot_longer(cols = c(glmmTMB), names_to = "Model", values_to = "count")

admitted_reg_pred_plot <- admitted_reg_plot +
  geom_line(data = regional_pred, mapping = aes(x = Dato, y = count, colour = Model, linetype = Model), size = .8) +
  facet_wrap(facets = vars(region), nrow = 2, ncol = 3) +
  scale_color_brewer(palette = "Paired")
ggsave(plot = admitted_reg_pred_plot,
       filename = "./reports/figures/admitted_reg_pred_plot.png",
       dpi = "retina", width = 12, height = 8, units = "in")







  



