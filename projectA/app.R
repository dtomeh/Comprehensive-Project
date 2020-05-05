# Libraries
library(shiny)
library(tidyverse)
library(rtweet)
library(tm)
library(textstem)
library(wordcloud)
library(ggplot2)
library(tidytext)
library(RWeka)
library(qdap)

#Data Import and Cleaning
token <- create_token(
    app= "project A_DT",
    consumer_key = "0VgaAe09epqNuaxEzaPMbznNg", 
    consumer_secret = "sI5GHF0RM2nLjchIKBNfIV0T7aHGOJtGM1zhnVQexpVU3DgcOR",
    access_token = "48436651-18S7T4kBTtRTbkzazQiVFOdfQqJlkWVG7D3KVrPGX", 
    access_secret = "wfX0vj2GzeFw0RCU4sdkqnZ8d4JjHkeFdXGtUikOyh38b"
)

##Importing Tweets 

COVID <- search_tweets("COVID", 500, include_rts = F )
COVID_clean <- COVID %>%
    select(text) 
COVID_clean$text <- COVID_clean$text  %>% iconv("UTF-8", "ASCII", sub="")

COVID19 <- search_tweets("COVID19", 500, include_rts = F )
COVID19_clean <- COVID19 %>%
    select(text) 
COVID19_clean$text <- COVID19_clean$text  %>% iconv("UTF-8", "ASCII", sub="")

COVID.19  <- search_tweets("COVID-19", 500, include_rts = F )
COVID.19_clean <- COVID.19 %>%
    select(text)
COVID.19_clean$text <- COVID.19_clean$text  %>% iconv("UTF-8", "ASCII", sub="")

COVID_19 <- search_tweets("COVID_19", 500, include_rts = F )
COVID_19_clean <- COVID_19 %>% 
    select(text)
COVID_19_clean$text <- COVID_19_clean$text  %>% iconv("UTF-8", "ASCII", sub="")

##NLP
###COVID 
COVID_cp <-VCorpus(VectorSource(COVID_clean$text))
COVID_cp <- tm_map(COVID_cp, PlainTextDocument)
COVID_cp <- tm_map(COVID_cp, content_transformer(replace_abbreviation))
COVID_cp <- tm_map(COVID_cp, content_transformer(replace_contraction))
COVID_cp <- tm_map(COVID_cp, content_transformer(str_to_lower))
COVID_cp <- tm_map(COVID_cp, content_transformer(function(x) str_replace_all(x, "#\\w+", ""))) 
COVID_cp <- tm_map(COVID_cp, removeNumbers)
COVID_cp <- tm_map(COVID_cp, removePunctuation)
COVID_cp <- tm_map(COVID_cp, removeWords, c(stopwords("en"), stopwords("es")))
COVID_cp <- tm_map(COVID_cp, stripWhitespace)
COVID_cp <- tm_map(COVID_cp, content_transformer(lemmatize_strings))

###COVID19

COVID19_cp <-VCorpus(VectorSource(COVID19_clean$text))
COVID19_cp <- tm_map(COVID19_cp, PlainTextDocument)
COVID19_cp <- tm_map(COVID19_cp, content_transformer(replace_abbreviation))
COVID19_cp <- tm_map(COVID19_cp, content_transformer(replace_contraction))
COVID19_cp <- tm_map(COVID19_cp, content_transformer(str_to_lower))
COVID19_cp <- tm_map(COVID19_cp, content_transformer(function(x) str_replace_all(x, "#\\w+", "")))
COVID19_cp <- tm_map(COVID19_cp, removeNumbers)
COVID19_cp <- tm_map(COVID19_cp, removePunctuation)
COVID19_cp <- tm_map(COVID19_cp, removeWords, c(stopwords("en"), stopwords("es")))
COVID19_cp <- tm_map(COVID19_cp, stripWhitespace)
COVID19_cp <- tm_map(COVID19_cp, content_transformer(lemmatize_strings))

###Covid-19
COVID.19_cp <-VCorpus(VectorSource(COVID.19_clean$text))
COVID.19_cp <- tm_map(COVID.19_cp, PlainTextDocument)
COVID.19_cp <- tm_map(COVID.19_cp, content_transformer(replace_abbreviation))
COVID.19_cp <- tm_map(COVID.19_cp, content_transformer(replace_contraction))
COVID.19_cp <- tm_map(COVID.19_cp, content_transformer(str_to_lower))
COVID.19_cp <- tm_map(COVID.19_cp, content_transformer(function(x) str_replace_all(x, "#\\w+", "")))
COVID.19_cp <- tm_map(COVID.19_cp, removeNumbers)
COVID.19_cp <- tm_map(COVID.19_cp, removePunctuation)
COVID.19_cp <- tm_map(COVID.19_cp, removeWords, c(stopwords("en"), stopwords("es")))
COVID.19_cp <- tm_map(COVID.19_cp, stripWhitespace)
COVID.19_cp <- tm_map(COVID.19_cp, content_transformer(lemmatize_strings))

###Covid_19
COVID_19_cp <-VCorpus(VectorSource(COVID_19_clean$text))
COVID_19_cp <- tm_map(COVID_19_cp, PlainTextDocument)
COVID_19_cp <- tm_map(COVID_19_cp, content_transformer(replace_abbreviation))
COVID_19_cp <- tm_map(COVID_19_cp, content_transformer(replace_contraction))
COVID_19_cp <- tm_map(COVID_19_cp, content_transformer(str_to_lower))
COVID_19_cp <- tm_map(COVID_19_cp, content_transformer(function(x) str_replace_all(x, "#\\w+", "")))
COVID_19_cp <- tm_map(COVID_19_cp, removeNumbers)
COVID_19_cp <- tm_map(COVID_19_cp, removePunctuation)
COVID_19_cp <- tm_map(COVID_19_cp, removeWords, c(stopwords("en"), stopwords("es")))
COVID_19_cp <- tm_map(COVID_19_cp, stripWhitespace)
COVID_19_cp <- tm_map(COVID_19_cp, content_transformer(lemmatize_strings))

## Creating the final cleaned tibbles

myTokenizer <- function(x) { NGramTokenizer(x, Weka_control(min=1, max=2)) }


###COVID 
COVID_dtm <- DocumentTermMatrix(COVID_cp, control = list(tokenize = myTokenizer ))
COVID_dtm <-removeSparseTerms(COVID_dtm, .99)
tokenCounts <- apply(COVID_dtm, 1, sum)
COVID_dtm <- COVID_dtm[tokenCounts > 0, ]

###COVID19
COVID19_dtm <- DocumentTermMatrix(COVID19_cp, control = list(tokenize = myTokenizer ))
COVID19_dtm <-removeSparseTerms(COVID19_dtm, .99)
tokenCounts <- apply(COVID19_dtm, 1, sum)
COVID19_dtm <- COVID19_dtm[tokenCounts > 0, ]

###COVID-19
COVID.19_dtm <- DocumentTermMatrix(COVID.19_cp, control = list(tokenize = myTokenizer ))
COVID.19_dtm <-removeSparseTerms(COVID.19_dtm, .99)
tokenCounts <- apply(COVID.19_dtm, 1, sum)
COVID.19_dtm <- COVID.19_dtm[tokenCounts > 0, ]

###COVID_19
COVID_19_dtm <- DocumentTermMatrix(COVID_19_cp, control = list(tokenize = myTokenizer ))
COVID_19_dtm <-removeSparseTerms(COVID_19_dtm, .99)
tokenCounts <- apply(COVID_19_dtm, 1, sum)
COVID_19_dtm <- COVID_19_dtm[tokenCounts > 0, ]

###Final Token Tibbles
COVID_tbl <- as_tibble(as.matrix(COVID_dtm)) %>% 
    select(!covid) 
COVID19_tbl <- as_tibble(as.matrix(COVID19_dtm)) %>%
    select(!covid) 
COVID.19_tbl <- as_tibble(as.matrix(COVID.19_dtm)) %>%
    select(!covid)
COVID_19_tbl <- as_tibble(as.matrix(COVID_19_dtm)) %>%
    select(!covid) 

# Analysis

##Creating the data for the Frequency Table 

COVIDnames <- names(COVID_tbl[,colSums(COVID_tbl)>5])
COVID19names <- names(COVID19_tbl[,colSums(COVID19_tbl)>5])
COVID.19names <- names(COVID.19_tbl[,colSums(COVID.19_tbl)>5])
COVID_19names <- names(COVID_19_tbl[,colSums(COVID_19_tbl)>5])

##Creating the data for the Barplot 

COVID_counts <- data.frame(names(COVID_tbl), colSums(COVID_tbl))
colnames(COVID_counts) <-c("Token", "Count of Token")
COVID19_counts <- data.frame(names(COVID19_tbl), colSums(COVID19_tbl))
colnames(COVID19_counts) <-c("Token", "Count of Token")
COVID.19_counts <- data.frame(names(COVID.19_tbl), colSums(COVID.19_tbl))
colnames(COVID.19_counts) <-c("Token", "Count of Token")
COVID_19_counts <- data.frame(names(COVID_19_tbl), colSums(COVID_19_tbl))
colnames(COVID_19_counts) <-c("Token", "Count of Token")

full_data <- rbind(COVID_counts, COVID19_counts, COVID.19_counts, COVID_19_counts)

top20 <- full_data %>%
    arrange(Token) %>%
    group_by(Token) %>% 
    tally(`Count of Token`) %>%
    top_n(20) %>%
    ungroup() %>%
    arrange(desc(n)) 

#Visualization 
freq_table <- data.frame(as.matrix(c(sum(COVIDnames %in% COVID19names),sum(COVIDnames %in% COVID.19names), sum(COVIDnames %in% COVID_19names), sum(COVID19names %in% COVID.19names), sum(COVID19names %in% COVID_19names),sum(COVID.19names %in% COVID_19names))))
rownames(freq_table) <- c("#COVID and #COVID19", "#COVID and #COVID-19", "#COVID and #COVID_19", "#COVID19 and #COVID-19", "#COVID19 and #COVID_19", "#COVID-19 and #COVID_19")
colnames(freq_table) <-c("Number of Tokens appearing in both hashtags more than 5 times")

#creating the UI 

ui <- fluidPage(
    
    titlePanel("COVID 19 Tweet Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput("Hashtag",
                        "Select a Hashtag to see the most common words used in recent tweets:",
                        c("#COVID" ="COVID", 
                          "#COVID19"="COVID19",
                          "#COVID-19"="COVID.19",
                          "#COVID_19"="COVID_19"), 
                        selected = "All"),
        ),
        
        mainPanel(
            plotOutput("wordcloud", width = "500px", height="500px"),
            DT::dataTableOutput("freqtable"),
            plotOutput("topwords")
        )
    )
)

server <- function(input, output) {
    
    #Filter data so that plot and table will adjust accordingly
    filtered_data <- reactive({
        data <- COVID_tbl
        if (input$Hashtag=="COVID"){
            data <- COVID_tbl} else if (input$Hashtag=="COVID19"){
                data <-COVID19_tbl} else if (input$Hashtag=="COVID.19") { data <- COVID.19_tbl } else{ data <- COVID_19_tbl}
        data
    })
    
    output$wordcloud <-renderPlot({
        data <- filtered_data()
        wordCounts <- colSums(data)
        wordNames <- names(data)
        wordcloud(wordNames, wordCounts, max.words=50, colors = c("lightpink","darkgrey", "darkcyan", "coral", "maroon"))
        
    })
    
    output$freqtable <- DT::renderDataTable({
        freq_table
    })
    
    output$topwords <- renderPlot({
        top20 %>%
            ggplot(aes(x=Token, y=n)) + 
            geom_bar(stat="identity", fill="light blue") +
            geom_text(aes(label=n), hjust=1) +
            coord_flip() +
            labs(x="Token", y="Count of Token Across All Hashtags", title = "Top 20 most commonly mentioned Token's across all four Hashtags")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

#URL to live App
#https://dtomeh.shinyapps.io/projectA/?_ga=2.176046626.1026577531.1588695748-2138993899.1588695748