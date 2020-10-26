library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)
getwd()
# reads in data 
Crime_DC <-read_csv("Crime_Incidents_in_2019.csv")

ui = fluidPage(theme = shinytheme("cerulean"),
               tags$head(
                   
                   
                   titlePanel("Crime in DC 2019"),         
               ),
               navbarPage("Crime_DC",id='nav',
                          tabPanel("Interactive Map",
                                   fluidRow(
                                       column(4,plotOutput("OFFENSE",height=300)),
                                       column(4,plotOutput("REPORT_DAT",height=300)),
                                       column(4,plotOutput("SHIFT",height=300))
                                   ),
                                   leafletOutput("mymap", width = '100%', height = '600px'),
                                   br(),
                                   absolutePanel(id = "controls",class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                                 top = 500, left = "auto", right = 20, bottom = "auto",
                                                 width = 330, height = "auto", style = "opacity: .75",
                                                 
                                                 a("data source: http://opendata.dc.gov/datasets",href="http://opendata.dc.gov/datasets"),
                                                 br(),
                                                 
                                                 
                                                 
                                   )
                          ),
                          tabPanel("Data Explorer",
                                   dataTableOutput("table1")
                          )
               )
)

server = function(input, output) {
    
    output$mymap <- renderLeaflet({
        # reactive expression code required here to connect with ui selection?
        leaflet(Crime_DC) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>% 
            setView(-77.931180, 38.9072, zoom = 7) %>% 
            addCircleMarkers(~LONGITUDE, ~LATITUDE, popup=Crime_DC$OFFENSE, weight = 3, radius=4, 
                             stroke = F, fillOpacity = 0.5)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
