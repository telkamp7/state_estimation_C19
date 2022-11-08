# Load libraries
library(readr)

# Load in the data
newly_admitted <- read_csv2(file = "C:/GIT/state_estimation_C19/data/raw/Newly_admitted_over_time.csv")

# Extract full column specification for the data
spec(newly_admitted)

write_rds(x = newly_admitted, 
          file = "C:/GIT/state_estimation_C19/data/processed/processed_data.rds", 
          compress = "xz")

