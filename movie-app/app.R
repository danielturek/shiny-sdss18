# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)

editLabel <- function(x) {
  x <- tools::toTitleCase(gsub('_', ' ', x))
  x <- gsub('Mpaa', 'MPAA', x)
  x <- gsub('Imdb', 'IMBD', x)
  x <- gsub('Num', 'Number', x)
}

# Load data ---------------------------------------------------------
load("data/movies.Rdata")

# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  titlePanel(title = "Movies"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("IMDB Rating"="imdb_rating", "IMDB Number Votes"="imdb_num_votes", "Critics Score"="critics_score", "Audience Score"="audience_score", "Runtime"="runtime"), 
                  selected = "audience_score"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("IMDB Rating"="imdb_rating", "IMDB Number Votes"="imdb_num_votes", "Critics Score"="critics_score", "Audience Score"="audience_score", "Runtime"="runtime"), 
                  selected = "critics_score"),
      
      selectInput(inputId = "color",
                  label = "Color by:",
                  choices = c("Title Type"="title_type", "Genre"="genre", "MPAA Rating"="mpaa_rating", "Critics Rating"="critics_rating", "Audience Rating"="audience_rating"),
                  selected = "mpaa_rating"),
      
      hr(),
      
      # Enter text for plot title ---------------------------------------------
      textInput(inputId = "plot_title", 
                label = "Plot title:", 
                placeholder = "Enter text to be used as plot title"),
      
      checkboxGroupInput(inputId = "selected_type",
                         label = "Select movie type(s):",
                         choices = c("Documentary", "Feature Film", "TV Movie"),
                         selected = "Feature Film"),
      
      hr(),
      
      sliderInput(inputId = "alpha", label = "Alpha:", min = 0, max = 1, value = 1),
      
      sliderInput(inputId = "pointSize", label = "Size of points:", min = 0, max = 5, value = 1, step = 1),
      
      checkboxInput(inputId = "includeDataTable", label = "Show Data Table?", value = FALSE),
      
      conditionalPanel(condition = "input.includeDataTable == true",
                       numericInput(inputId = "numRows", label = "Number of rows in data table:", value = 10, min = 1, max = 50, step = 1))
      ),
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      br(),
      uiOutput(outputId = "n"),
      br(),
      br(),
      DT::dataTableOutput(outputId = "datatable")
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
  # Create a subset of data filtering for selected title types ------
  movies_subset <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(movies, title_type %in% input$selected_type)
  })

  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = movies_subset(), aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(alpha = input$alpha, size = input$pointSize) +
      labs(x = editLabel(input$x),
           y = editLabel(input$y),
           color = editLabel(input$color),
           title = editLabel(input$plot_title)) +
      theme(plot.title = element_text(size=22, face = "bold"))
  })
  
  # Print number of movies plotted ----------------------------------
  output$n <- renderUI({
    types <- movies_subset()$title_type %>% factor(levels = input$selected_type) 
    counts <- table(types)
    HTML(paste("There are", counts, input$selected_type, "movies in this dataset. <br>"))
  })
  
  # Create a datatable with the data
  output$datatable <- DT::renderDataTable({
    if(input$includeDataTable) {
      DT::datatable(movies_subset()[, 1:7],
                    options = list(pageLength = input$numRows),
                    rownames = FALSE)
    }
  })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
