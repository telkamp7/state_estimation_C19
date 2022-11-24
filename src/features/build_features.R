# Load libraries
library(readr)
library(dplyr)

# Load in the data
newly_admitted <- read_csv2(file = "./data/raw/Newly_admitted_over_time.csv")

# Extract full column specification for the data
spec(newly_admitted)

# Add time column
newly_admitted <- newly_admitted %>%
  mutate(time = factor(x = 1:n(), levels = 1:n()))

# Add group
newly_admitted <- newly_admitted %>%
  mutate(group = factor(rep(1,n())))

write_rds(x = newly_admitted, 
          file = "./data/processed/processed_data.rds", 
          compress = "xz")

