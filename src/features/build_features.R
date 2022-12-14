# Load libraries
library(readr)
library(dplyr)

# Load in the data
newly_admitted <- read_csv2(file = "./data/raw/Newly_admitted_over_time.csv")

# Extract full column specification for the data
spec(newly_admitted)

# Add drift column
newly_admitted <- newly_admitted %>%
  mutate(drift = factor(x = 1:n(), levels = 1:n()))

# Add group
newly_admitted <- newly_admitted %>%
  mutate(group = factor(rep(1,n())))

write_rds(x = newly_admitted, 
          file = "./data/processed/processed_data.rds", 
          compress = "xz")

# Make data for regional models
regional_newly_admitted <- newly_admitted %>%
  select(Dato:`Ukendt Region`, drift) %>%
  pivot_longer(cols = Hovedstaden:`Ukendt Region`, names_to = "region", values_to = "count") %>%
  mutate(region = as.factor(region))

write_rds(x = regional_newly_admitted,
          file = "./data/processed/processed_regional_data.rds",
          compress = "xz")

