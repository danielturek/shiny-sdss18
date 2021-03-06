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
      
      sliderInput(inputId = "alpha", label = "Alpha:", min = 0, max = 1, value = 1),
      
      checkboxInput(inputId = "includeDataTable", label = "Show Data Table?", value = FALSE),
      
      numericInput(inputId = "numRows", label = "Number of rows in data table:", value = 10, min = 1, max = 50, step = 1)
    ),
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      DT::dataTableOutput(outputId = "datatable")
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {

  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(alpha = input$alpha) +
      labs(x = editLabel(input$x),
           y = editLabel(input$y),
           color = editLabel(input$color))
  })
  
  # Create a datatable with the data
  output$datatable <- DT::renderDataTable({
    if(input$includeDataTable) {
      DT::datatable(movies[, 1:7],
                    options = list(pageLength = input$numRows),
                    rownames = FALSE)
    }
  })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
