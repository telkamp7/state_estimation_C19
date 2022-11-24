# Load libraries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

# Load processed data
newly_admitted <- read_rds(file = "./data/processed/processed_data.rds")

# Number of admissions separated into regions
newly_admitted %>%
  select(-Total) %>%
  pivot_longer(cols = -1,
               names_to = "Region",
               values_to = "Admissions") %>%
  ggplot(mapping = aes(x = Dato, y = Admissions)) +
  geom_line() +
  facet_wrap(facets = vars(Region), nrow = 2, ncol = 3) +
  labs(x = "Date", y = "Newly admitted [#]") +
  theme_bw()
ggsave(filename = "./reports/figures/newly_admitted_regions.png",
       dpi = "retina", width = 12, height = 8, units = "in")

# Number of admissions in total
newly_admitted %>%
  ggplot(mapping = aes(x = Dato, y = Total)) +
  geom_line() +
  labs(x = "Date", y = "Newly admitted [#]") +
  theme_bw()
ggsave(filename = "./reports/figures/newly_admitted_total.png",
       dpi = "retina", width = 8, height = 8, units = "in")

