# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(jsonlite)
library(tidyverse)

# use jsonlite to convert json to R list 
rstat_list <- fromJSON("https://www.reddit.com/r/rstats/.json", flatten = TRUE)

# extract post information (children), convert to tibble
rstats_original_tbl <- as_tibble(rstat_list$data$children)

# Create tbl to only include post title, # up votes and # comments
rstats_tbl <- rstats_original_tbl %>% 
  select(post = data.title, upvotes = data.ups, comments = data.num_comments)

# Use ggplot to create a scatterplot to show relationship between upvotes and comments
(ggplot(rstats_tbl, aes(upvotes, comments))+
    geom_point() +
    xlab("Number of upvotes") +
    ylab("Number of comments") ) |> 
  ggsave(filename="../figs/fig1.png", units = "px", width = 1920, height = 1080)

# Use cor.test to calculate correlation between upvotes and comments and p-value
cor.test(rstats_tbl$upvotes, rstats_tbl$comments)

# Publication 
# The correlation between upvotes and comments was r(23) = .42, p = .038. This test was statistically significant.
# function to display the output of the correlation
correlation_fct <- function(upvotes, comments) {
  # Calculate the correlation between upvotes and comments
  cor_result <- cor.test(upvotes, comments)
  corr <- cor_result$estimate
  df <- cor_result$parameter
  p_value <- cor_result$p.value
  
  # Format correlation and p-value to display only 2 digits after the decimal and remove leading zeros 
  corr_f <- corr %>% 
    formatC(digits=2) %>% 
    str_remove("^0+")
  p_value_f <- p_value %>% 
    formatC(digits=2) %>% 
    str_remove("^0+")
  # Construct the outputted line of text using the template
  if (p_value < 0.05) {
    output <- paste("The correlation between upvotes and comments was r(", df, ") = ", corr_f, ", p = ", p_value_f, ". This test was statistically significant.", sep="")
  } else {
    output <- paste("The correlation between upvotes and comments was r(", df, ") = ", corr_f, ", p = ", p_value_f, ". This test was not statistically significant.", sep="")
  }
  
  # Return the output
  return(output)
}

# Run the function with data
correlation_fct(rstats_tbl$upvotes, rstats_tbl$comments)
