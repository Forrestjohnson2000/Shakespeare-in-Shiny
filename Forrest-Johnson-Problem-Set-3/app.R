library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")

  text <-  tibble(text = readLines(sprintf("data/%s.txt", book), encoding="UTF-8"))

  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE)

  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }

  return(text)
}

## UI ##########################################################################
ui <- fluidPage(
  theme = shinytheme("spacelab"),


  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title

  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'books', label = "Choose a play", choices = books),
      checkboxInput(inputId = "stopwords", label = "Stop words:", value = TRUE),
      actionButton(inputId = "go", label = "Run"),

  #Word Cloud Settings ###################################################
      hr(style = "border-top: 1px solid #000000;"),
      h3("Word Cloud Settings"),
      sliderInput(inputId = "maxwords", label = "Max # of Words:",
                  min = 10, max = 200, value = 100, step = 10),
      sliderInput(inputId = 'large', label = 'Size of largest words:', min = 1, max = 8, value = 4),
      sliderInput(inputId = 'small', label = 'Size of smallest words:', min = 0.1, max = 4, value = 0.5),

  #Word Frequency Settings ###################################################
      hr(style = "border-top: 1px solid #000000;"),
      h3("Word Count Settings"),
      sliderInput(inputId = 'minwords', label = "Minimum words for Count Chart:", min = 10, max = 100, value = 25),
      sliderInput(inputId = 'font', label = "Word size for Count Chart:", min = 8, max = 30, value = 14)
    ),

  #Main Panel ##############################
    mainPanel(

      # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
      tabsetPanel(
        tabPanel(title = "Word Cloud",
                 plotOutput(outputId = 'cloud',
                            height = "600px",
                            width = "600px")),

        tabPanel(title = "Word Counts",
                 plotOutput(outputId = 'freq',
                            height = "600px"))
        ) #End of Tabset panel

      ) #End of main panel

    ) # End of sidebar Layout

) # END OF UI ######

server <- function(input, output) {

  # Reactivity Function ################
  freq = reactive({
    eventReactive(input$go, {
      withProgress({
        setProgress(message = "Processing corpus...")
        getFreq(input$books, input$stopwords)
      })

    })
  }) # End of Reactive function

  # Output for Word Cloud ################################
  output$cloud = renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")

    v() %>%
      with(
        wordcloud(
          word,
          n,
          scale = c(input$large, input$small),
          random.order = FALSE,
          max.words = input$maxwords,
          colors=pal))
    })

  # Output for Word Frequency Graph ########################
  output$freq = renderPlot({
    v = freq()
    
    v() %>%
      filter(n > input$minwords) %>%
      ggplot(mapping = aes(x = n, y = reorder(word, n))) + 
      geom_col() + 
      theme(text = element_text(size = input$font),
            axis.title = element_blank())
    })
}

shinyApp(ui = ui, server = server)
