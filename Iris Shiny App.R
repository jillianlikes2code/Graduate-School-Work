library(ggplot2)
library(shiny)
library(dplyr)
data("iris")
library(rsconnect)
library(shinyWidgets)

iris_summary <- iris %>%
  group_by(Species) %>%
  summarise("Average sepal length" = mean(Sepal.Length),
            "Average sepal width" = mean(Sepal.Width),
            "Average petal length" = mean(Petal.Length),
            "Average petal length" = mean(Petal.Width))

ui <- fluidPage(
  
  # creating a gradient background
  setBackgroundColor(
    color = c("#FFC0CB", "#FFCCFF"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  # creating the title for my shiny app
  titlePanel("Iris Data"),
  sidebarLayout(
    sidebarPanel(
      # creating the variable for my y-axis
      selectInput(
        inputId = "y",
        label = "y-axis",
        choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
        selected = "Sepal.Length"
      ),
      
      # creating the variable for my x-axis
      selectInput(
        inputId = "x",
        label = "x-axis",
        choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
        selected = "Sepal.Width"
      ),
      
      # select species to be dynamically displayed in graph, by default all species are selected
      checkboxGroupInput(
        inputId = "species",
        label = "Select Species", 
        choices = c("setosa", "versicolor", "virginica"),
        selected = c("setosa", "versicolor", "virginica")
      )
    ),
    
    
    mainPanel(
      tags$h3("Scatter Plot"), 
      plotOutput(outputId = "scatterplot"),
      
    )
    
    
  )
)


server <- function(input, output) {
  # create a reactive dataframe to help with visualization
  df <- reactive({
    iris %>%
      filter(Species %in% input$species)
  })
  
  # scatter plot of species data
  output$scatterplot <- renderPlot({
    ggplot(
      df(), 
      aes_string(x = input$x, y = input$y)) + 
      geom_point(aes(col = df()$Species), size=3) + scale_color_discrete(name ="Species") +
      geom_smooth(aes(group=df()$Species, color = df()$Species), method='lm')
    
  })
  
  
}
shinyApp(ui = ui, server = server)