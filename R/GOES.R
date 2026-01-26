# From FT climate
library(needs)
needs(tidyverse, readxl, googlesheets4, stringr, plotly, magrittr, janitor, RcppRoll, lubridate, ggrepel, zoo, scales, rvest, sf,raster)
options(scipen = 20)

# Specify the URL of the webpage
url <- "https://cdn.star.nesdis.noaa.gov/GOES19/ABI/SECTOR/car/GEOCOLOR/"

# Read the HTML content of the page
page <- read_html(url)

# Extract all hyperlinks from the page
links <- page %>%
  html_nodes("a") %>%       # Select all <a> nodes
  html_attr("href")         # Extract the 'href' attribute

# Filter links that end with '2000x2000.jpg' and start with at least 7 digits >= '2025297', change this number depending on what day you want to start from
filtered_links <- links %>%
  # Keep links that end with '2000x2000.jpg'
  str_subset("2000x2000\\.jpg$") %>%
  # Keep links that start with 7 or more digits
  str_subset("^\\d{7,}.*") %>%
  # Convert the first 7 characters to numeric and filter
  keep(~ as.numeric(substr(., 1, 7)) >= 2025297)

# Combine the base URL with the filtered links to form complete URLs
full_links <- paste0(url, filtered_links)

# Create a directory to save the downloaded images
dir.create("downloaded_images", showWarnings = FALSE)

# Download each image
for (link in full_links) {
  # Extract the filename from the link
  destfile <- file.path("downloaded_images", basename(link))
  # Download the file
  download.file(link, destfile, mode = "wb")
  # Print progress
  message("Downloaded: ", destfile)
}
