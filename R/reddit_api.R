# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(jsonlite)
library(tidyverse)

# Data Import and Cleaning
# use jsonlite to convert json to R list 
rstat_list <- fromJSON("https://www.reddit.com/r/rstats/.json", flatten = TRUE)

# extract post information (children), convert to tibble
rstats_original_tbl <- as_tibble(rstat_list$data$children)

# Create tbl to only include post title, # up votes and # comments
rstats_tbl <- rstats_original_tbl %>% 
  select(post = data.title, upvotes = data.ups, comments = data.num_comments)


