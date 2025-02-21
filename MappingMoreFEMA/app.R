library(shiny)
library(knitr)
library(tidyverse)
library(magrittr)
library(drat)
library(maps)
library(ggplot2)
library(dplyr)
library(rgdal)
library(geojsonio)
library(tigris)
library(usmap)
library(leaflet)
library(grid)
library(kableExtra)
library(gridExtra)

hrcc <- read.csv("hrc_county.csv", head = TRUE)
hrcctable <- hrcc[,c(1:3,7,9:12)]
colnames(hrcctable) <-  c("DisasterNum","Year","Incident","State","County","ProjectAmount","FederalShareObligated","ToatlObligated")


loadJ <- geojsonio::geojson_read("gz_2010_us_050_00_5m.json", what = "sp")
loadJ$GEO_ID <- loadJ$GEO_ID %>% substr(start = 10, stop = 14)


ui <- fluidPage(
    
    titlePanel("MappingFEMA"),
    
    sidebarLayout(
        tabsetPanel(
            conditionalPanel(
                'input.dataset === "hrcctable"'),
            
            conditionalPanel(
                'input.dataset === "hrcc" ',
            )
        ),
        
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("Hurricane and Storms Data",
                         fluidRow(
                             column(4,
                                    selectInput("DisasterNum",
                                                "DisasterNum: ",
                                                c("All",
                                                  unique(hrcctable$DisasterNum)))
                             ),
                             column(4,
                                    selectInput("Year",
                                                "Year:",
                                                c("All",
                                                  unique(hrcctable$Year)))
                             ),
                             column(4,
                                    selectInput("Incident",
                                                "Incident Type:",
                                                c("All",
                                                  unique(hrcctable$Incident)))
                             ),
                             column(4,
                                    selectInput("State",
                                                "State:",
                                                c("All",
                                                  unique(hrcctable$State)))
                             ),
                             column(4,
                                    selectInput("County",
                                                "County:",
                                                c("All",
                                                  unique(hrcctable$County)))
                             ),
                         ),
                         DT::dataTableOutput("table")
                ),
                
                tabPanel("LeafletPlot",
                         
                         fluidRow(
                             column(4,
                                    selectInput("Year2",
                                                "Year:",
                                                c("All",
                                                  unique(hrcc$declarationYear)))
                             ),
                             column(4,
                                    selectInput("Incident2",
                                                "Incident Type:",
                                                c("All",
                                                  unique(hrcc$incidentType)))
                             ),
                             column(4,
                                    selectInput("State2",
                                                "State:",
                                                c("All",
                                                  unique(hrcc$state)))
                             ),
                             column(4,
                                    selectInput("Amount",
                                                "Select a View:",
                                                c("ProjectAmount","FederalShareObligated", "ToatlObligated"))
                             ),
                         ),
                         leafletOutput("LeafletPlot"),
                )
            )
        )
    )
)

server <- function(input, output) {
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- hrcctable
        if (input$DisasterNum != "All") {
            data <- data[data$DisasterNum == input$DisasterNum,]
        }
        if (input$Year != "All") {
            data <- data[data$Year == input$Year,]
        }
        if (input$Incident != "All") {
            data <- data[data$Incident == input$Incident,]
        }
        if (input$State != "All") {
            data <- data[data$State == input$State,]
        }
        if (input$County != "All") {
            data <- data[data$County == input$County,]
        }
        data
    }))
    
    output$LeafletPlot <- renderLeaflet({
        data <- hrcc
        loadJ <- loadJ
        if (input$Year2 != "All") {
            data <- data[data$declarationYear == input$Year2,]
        }
        if (input$Incident2 != "All") {
            data <- data[data$incidentType == input$Incident2,]
        }
        if (input$State2 != "All") {
            data <- data[data$state == input$State2,]
        }
       
        dataplot <- data %>% group_by(Fips)
        if (input$Amount == "ProjectAmount"){
            dataJ <-dataplot %>% 
                    summarise(state = unique(state), county = unique(county), 
                          PlotAmount = sum(projectAmount)/1000)
            
        }
        else if (input$Amount == "FederalShareObligated"){
            dataJ <-dataplot %>% 
                summarise(state = unique(state), county = unique(county), 
                          PlotAmount = sum(federalShareObligated)/1000)
        }
        else if (input$Amount == "ToatlObligated"){
            dataJ <-dataplot %>% 
                summarise(state = unique(state), county = unique(county), 
                          PlotAmount = sum(totalObligated)/1000)
        }
        
        dataJ <- dataJ %>% rename(GEO_ID = Fips)
        outValue <- boxplot(dataJ$PlotAmount)$out
        dataJ <- dataJ %>% filter(!(PlotAmount %in% outValue))
        dataJoin <- geo_join(loadJ, dataJ, by = "GEO_ID", how = "inner")
        pal <- colorNumeric("Blues", domain = dataJoin$PlotAmount)
        # popup 
        i_popup <- paste0("<strong>State: </strong>", 
                          dataJoin$state, "<br>", 
                          "<strong>County: </strong>", 
                          dataJoin$county, "<br>", 
                          "<strong>Amount: </strong>",
                          round(dataJoin$PlotAmount,2) ,
                          " ","thousands") 
        LeafletMap <- leaflet() %>% 
                        addProviderTiles("CartoDB.Positron") %>%
                        addProviderTiles(providers$Stamen.TonerLines,
                                         options = providerTileOptions(opacity = 0.75)) %>%
                        setView(-89.275673, 37.098, zoom = 4) %>%
                        addPolygons(data = dataJoin,
                                    fillColor = ~pal(PlotAmount), 
                                    color = "#BDBDC3",
                                    fillOpacity  = 0.5, 
                                    smoothFactor = 0.2,
                                    weight = 1,
                                    popup = i_popup) %>%
                        addLegend(pal = pal,
                                  values = dataJoin$PlotAmount,
                                  position="bottomright",
                                  title = paste0("Amount in thousands"))
        LeafletMap
    }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
