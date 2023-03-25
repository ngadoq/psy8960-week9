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

# Visualization
# Use ggplot to create a scatterplot to show relationship between upvotes and comments
(ggplot(rstats_tbl, aes(upvotes, comments))+
    geom_point() +
    xlab("Number of upvotes") +
    ylab("Number of comments") ) |> 
  ggsave(filename="../figs/fig1.png", units = "px", width = 1920, height = 1080)

# Analysis
# Use cor.test to calculate correlation between upvotes and comments and p-value
cor.test(rstats_tbl$upvotes, rstats_tbl$comments)

