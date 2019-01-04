#global

library(twitteR)
library(tm)
library(wordcloud)
library(memoise)
library(shiny)
library(later)

# The list of twitter handles
handles <<- list("New York Times" = "nytimes",
               "The Globe and Mail" = "globeandmail",
               "Washington Post" = "washingtonpost")

#setup_twitter_oauth("API key", "API secret", "Access token", "Access secret")
#enter the info here


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(handle) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(handle %in% handles))
    stop("Unknown handle")
  tweets <- userTimeline(handle, n=50)
  tweets.df <- twListToDF(tweets)
  for(j in 1:nrow(tweets.df)){
    text=append(tweets.df[,1],text)
    
  }
  print(text)
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, function(x) iconv(enc2utf8(x), sub = "byte"))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                   c(stopwords("SMART")))
  toSpace = content_transformer(function(x, pattern) gsub(pattern,"",x) )
  
  myCorpus = tm_map(myCorpus, toSpace, "https.*")
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

#server
server=function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}

#UI

ui=fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a twitter handle:",
                  choices = handles),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)


# Run the application 
shinyApp(ui = ui, server = server)

