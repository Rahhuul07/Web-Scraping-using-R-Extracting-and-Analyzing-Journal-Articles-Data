---
title: "Final Project 1"
author: "Rahul"
date: "2023-11-10"
output:
  word_document: default
  html_document: default
---

```{r}
# Load required libraries
library(rvest)
library(httr)
library(xml2)
```


<!-- ###  2 Scraping data -->
```{r}


# Define the CSS selectors for the different article elements
title_selector <- '.c-listing__title a'
authors_selector <- '.c-listing__authors-list'
correspondence_author_selector <- '.c-listing__metadata span:contains("Correspondence Author") + span'
correspondence_author_email_selector <- '.c-listing__metadata span:contains("Correspondence Author Email") + span a'
publish_date_selector <- '.c-listing__metadata span[itemprop="datePublished"]'
abstract_selector <- '.c-listing__content p'

# Create a function to scrape article data for the given year
scrape_article_data <- function(year) {
  articles <- list()
  page <- 1

  while (TRUE) {
    # Create a URL to scrape articles for the given year and page
    url <- paste0('https://molecular-cancer.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&page=', page, '&year=', year)

    # Make a GET request to the URL
    response <- httr::GET(url)

    # Parse the HTML response
    soup <- xml2::read_html(response$content)

    # Extract the article data for each article on the page
    article_nodes <- soup %>% rvest::html_nodes('.c-listing__item')

    # Break the loop if there are no more articles
    if (length(article_nodes) == 0) {
      break
    }

    # Extract the search keywords
    search_keywords <- soup %>% html_nodes('input[name="query"]') %>% html_attr("value")

    # Extract the article data for each article on the page
    for (article_node in article_nodes) {
      article <- list()

      # Extract the article title
      article$Title <- rvest::html_text(article_node %>% rvest::html_node(title_selector))

      # Extract the article authors
      article$Authors <- rvest::html_text(article_node %>% rvest::html_nodes(authors_selector))

      # Extract the correspondence author
      article$Correspondence_Author <- rvest::html_text(article_node %>% rvest::html_node(correspondence_author_selector))

      # Extract the correspondence author's email
      article$Correspondence_Author_Email <- rvest::html_text(article_node %>% rvest::html_node(correspondence_author_email_selector))

      # Extract the publish date
      article$Publish_Date <- rvest::html_text(article_node %>% rvest::html_node(publish_date_selector))

      # Extract the abstract
      abstract_node <- article_node %>% rvest::html_node(abstract_selector)
      abstract <- rvest::html_text(abstract_node)
      article$Abstract <- abstract

      # Add the search keywords to the article
      article$Keywords <- search_keywords

      # Add the article to the list of articles
      articles <- append(articles, list(article))
    }

    # Increment the page number for the next iteration
    page <- page + 1
  }

  # Return the list of articles with search keywords
  articles
}

# Scrape the article data for the year 2019
year <- 2023
articles <- scrape_article_data(year)

# Print all of the details for each article
for (article in articles) {
  cat('Title:', article$Title, '\n')
  cat('Authors:', article$Authors, '\n')
  cat('Correspondence Author:', article$Correspondence_Author, '\n')
  cat('Correspondence Author Email:', article$Correspondence_Author_Email, '\n')
  cat('Publish Date:', article$Publish_Date, '\n')
  cat('Abstract:', article$Abstract, '\n')
  cat('Keywords:', article$Keywords, '\n')
  cat('\n\n')
}

```

#2 Cleaning data

```{r}
## Load the necessary libraries
library(dplyr)
library(stringr)

# Convert the scraped data into a data frame
df <- data.frame(
  Title = sapply(articles, function(x) x$Title),
  Authors = sapply(articles, function(x) x$Authors),
  Correspondence_Author = sapply(articles, function(x) x$Correspondence_Author),
  Correspondence_Author_Email = sapply(articles, function(x) x$Correspondence_Author_Email),
  Publish_Date = sapply(articles, function(x) x$Publish_Date),
  Abstract = sapply(articles, function(x) x$Abstract),
  Keywords = unlist(lapply(articles, function(x) paste(x$Keywords, collapse = ", "))),
  stringsAsFactors = FALSE
)

# Remove any irrelevant information
df <- df %>%
  mutate(
    Authors = str_replace_all(Authors, "\\n", ""),
    Correspondence_Author = str_replace_all(Correspondence_Author, "\\n", ""),
    Correspondence_Author_Email = str_replace_all(Correspondence_Author_Email, "\\n", ""),
    Publish_Date = str_replace_all(Publish_Date, "\\n", ""),
    Abstract = str_replace_all(Abstract, "\\n", "")
  )

# Handle missing values
df <- df %>%
  mutate(
    Authors = ifelse(Authors == "", NA, Authors),
    Correspondence_Author = ifelse(Correspondence_Author == "", NA, Correspondence_Author),
    Correspondence_Author_Email = ifelse(Correspondence_Author_Email == "", NA, Correspondence_Author_Email),
    Publish_Date = as.Date(ifelse(Publish_Date == "", NA, Publish_Date), format = "%Y-%m-%d"),
    Abstract = ifelse(Abstract == "", NA, Abstract)
  )

# Print the organized and cleaned data
print(df)


```

### 3 Visualization and analysis
```{r}

# Assuming 'df' is your cleaned and organized data frame

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Visualization 1: Bar chart for Keyword Frequency
keyword_counts <- table(unlist(strsplit(df$Keywords, ", ")))
keyword_df <- data.frame(Keyword = names(keyword_counts), Frequency = as.numeric(keyword_counts))
keyword_df <- keyword_df[order(keyword_df$Frequency, decreasing = TRUE), ]

#Visualization 1: Bar chart
ggplot(keyword_df, aes(x = reorder(Keyword, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Keyword Frequency in Scraped Articles",
       x = "Keywords",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Visualization 2: Boxplot of Abstract Length
df$Abstract_Length <- nchar(df$Abstract)
ggplot(df, aes(x = "Abstract Length", y = Abstract_Length)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Abstract Length",
       x = "",
       y = "Abstract Length")


```