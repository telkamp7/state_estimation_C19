# Load in libraries
library(rvest)

# Extract raw files to destination
exdir_raw <- "./data/raw/"

# URL for the data page
rvest_url <- "https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata"

# Instantiate website object
webpage <- read_html(rvest_url)

# Collect website attributes
col_attr <- html_attr(html_nodes(webpage, "a"), "href")

# Collect url files
url_files <- col_attr[grep(pattern = "https://files.ssi.dk/covid19/overvagning/data/overvaagningsdata-covid19", x = col_attr)]

# Find the newest data file
newest_file <- url_files[1]

# Create temporary file
temp <- tempfile(tmpdir = exdir_raw)
download.file(url = newest_file, destfile = temp, mode = "wb")
unzip(zipfile = temp, files = NULL, exdir = exdir_raw)
unlink(temp)
