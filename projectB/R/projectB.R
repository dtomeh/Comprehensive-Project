#Rstudio API code 

# Libraries 
library(rvest)
library(httr)
library(ggplot2)
library(tidyverse)
library(stringi)
library(tibble)

#Data import and Cleaning 
pagenums <- c(seq(0,230, by=10))
urls <- paste0("https://scholar.google.com/scholar?start=", pagenums, "&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24")

##Creating a function to scrape the titles 
titlescrape<- function(url){
 Sys.sleep(5)
  read_html(url) %>% 
    html_nodes(".gs_rt") %>% 
    html_text()
}


##Creating a function to scrape the remainder of the information
infoscrape <- function(url){
  Sys.sleep(5)
    read_html(url) %>% 
    html_nodes(".gs_a") %>% 
    html_text()

}



##Actually Scraping the titles 
titles <- lapply(urls, titlescrape) 

titles_clean <- titles %>% 
  unlist() %>%
  enframe() %>%
  select(-name) %>% 
  rename(article_title=value) %>%
  mutate(article_title = str_match(article_title, pattern="((\\[[[:upper:]]+\\])?(\\[[[:upper:]]+\\])?)\\s?([[:print:]]+)")[,5])


##Actually Scraping and cleaning the rest of the information
info <- lapply(urls, infoscrape)

info_clean <- info %>%
  unlist() %>%
  enframe() %>% 
  rename(info=value) %>%
  select(-name) %>%
  mutate(author_list = str_match(info, pattern="(.*?)\\s-")[,2]) %>%
  mutate(year=str_match(info, pattern="([0-9]{4})")[,2]) %>%
  mutate(link=str_match(info, pattern="[0-9]{4}\\s-\\s([[:print:]]*)")[,2]) %>%
  mutate(journal_title = str_match(info, pattern = "\\s-\\s[[:punct:]]?\\s?[[:punct:]]?([[:print:]]+),")[,2])

##Merging the two datasets 
final_tbl <- cbind(titles_clean, info_clean) %>%
  select(article_title, author_list, journal_title, year, link)

#Analysis 
top10 <- final_tbl %>%
  filter(is.na(journal_title)==FALSE) %>%
  group_by(journal_title) %>%
  arrange(journal_title) %>%
  count(journal_title) %>%
  ungroup() %>%
  top_n(10) %>%
  arrange(desc(n))

#Visualization
ggplot(top10, aes(x=reorder(journal_title, n), n)) + 
  geom_bar(stat="identity", fill="light blue") +
  geom_text(aes(label=n), hjust=1) +
  coord_flip() +
  labs(x="Name of Journal", y="Count of Citations", title = "Top 10 Jounrnals cited for COVID-19 Research In Psychology")
