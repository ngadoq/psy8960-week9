# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)

# Data Import and Cleaning

# Create a function with a delay of 2s
read_html_delayed <- slowly(read_html, 
                            rate = rate_delay(2))
# Use rvest to create an xml_document from url
rstats_html <- read_html_delayed("https://old.reddit.com/r/rstats/")

# Extract post titles, upvotes, and comments from rstats_html using xpath
post <- rstats_html %>% 
  html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "title", " " )) and contains(concat( " ", @class, " " ), concat( " ", "may-blank", " " ))]') %>% 
  html_text()

upvotes <- rstats_html %>% 
  html_elements(xpath = '//div[@class = "score unvoted"] ') %>% 
  html_text() %>%
  # convert to number for analysis
  as.integer() %>% 
  # replace NA with 0
  replace(is.na(.), 0)

comments <- rstats_html %>% 
  html_elements(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "comments", " " ))]') %>% 
  html_text() %>% 
  # remove character to retain only number
  str_extract("\\d+") %>% 
  # convert to number for analysis
  as.integer() %>% 
  # replace NA with 0
  replace(is.na(.), 0)

# Create rstats_tbl that contains post, upvotes and comments 
rstats_tbl <- tibble(post, upvotes, comments)

# Visualization
# Use ggplot to create a scatterplot to show relationship between upvotes and comments
(ggplot(rstats_tbl, aes(upvotes, comments))+
    geom_point() +
    xlab("Number of upvotes") +
    ylab("Number of comments") ) %>% 
  ggsave(filename="../figs/fig1_scrapped.png", units = "px", width = 1920, height = 1080)

# Analysis
# Use cor.test to calculate correlation between upvotes and comments and p-value
cor.test(rstats_tbl$upvotes, rstats_tbl$comments)

# Publication 
# The correlation between upvotes and comments was r(23) = .39, p = .052. This test was not statistically significant.
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
