#######################################################################################
#
#                 Author: Jack Wychor
#
#######################################################################################

####Packages####
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(readr)
library(tidyverse)
library(DT)
library(geosphere)
library(geodist)
library(leaflet)
library(leaflet.extras)
library(shinycssloaders)

source("R/dropdown-module.R")
source("R/leaflet-helper-functions.R")

options(shiny.sanitize.errors = TRUE,
        
        semantic.themes = TRUE,
        spinner.color = "#0275D8", 
        spinner.color.background="#ffffff", 
        spinner.size=2)

####Application####
###UI####
ui<- shinyUI(semanticPage(
  title = "Shipping Dashboard",
  theme = "lumen",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  #Map and datatable
  segment(class = "black",
          h1("Shipping Dashboard"),
    tabset(
           tabs =
             list(
               list(menu = "Map", content = leafletOutput("map"), id = "leafMap"),
               list(menu = "AIS Data", content = DT::DTOutput("AISDataTable"))
             ),
           active = "leafMap",
           id = "topTabset"
           )
  ),
  
  #Dropdowns
  segment(class = "black",
    h2("Ship Select"),
    cards(class = "2",
      dropdownUI("ShipTypeDropdownId"),
      dropdownUI("vesselDropdownId"))
    ),
  )
)

###Server####
server<- function(input, output, session){
  csvData<-as.data.frame(read_csv("ships.csv"))
  
  ##Reactives
  allData <- reactive({csvData})
  
  #Vector of unique ship names
  uniqueShips <- reactive({
    allData() %>%
      select(SHIPNAME) %>%
      unique() %>%
      .$SHIPNAME %>%
      sort() %>%
      prepend("All")
    })
  
  #Vector of unique ship types
  allShipTypes <- reactive({
    allData() %>% 
      select(ship_type) %>%
      unique() %>%
      .$ship_type %>%
      sort() %>%
      prepend("All")
  })
  
  #Data from all vessels with current ship type
  selectedShipTypeData <- reactive({
    allData() %>%
      filter(ship_type == input$ShipTypeInput)
  })
  
  ##VESSEL
  #List of vessels names with current ship type
  vessels <- reactive({
    if(input$ShipTypeInput == "All"){
      uniqueShips()
    }
    else{
      selectedShipTypeData() %>%
        select(SHIPNAME) %>%
        unique() %>%
        arrange() %>%
        .$SHIPNAME %>%
        sort() %>%
        prepend("All")
    }
  })
  
  #Data from current selected vessel
  shipData <- eventReactive(input$VesselInput,{
    if(input$VesselInput != "All"){
      allData() %>%
        filter(SHIPNAME == input$VesselInput)
    }
  })
  
  #Get max distance traveled in meters according to Haversine distance for all points
  shipDistData <- reactive({
    distanceToNext.Meters <- shipData() %>%
      arrange(DATETIME) %>%
      select(LON, LAT) %>%
      geodist(sequential = T, measure = "Haversine") %>%
      round(2) %>%
      append(0)
    
    data.frame(shipData() %>% arrange(DATETIME), distanceToNext.Meters)
  })
  
  #Find where Haversine distance is the highest
  maxShipDistanceTimes <- reactive({
    DTindex <- shipDistData() %>%
      arrange(desc(distanceToNext.Meters), desc(DATETIME)) %>%
      select(DATETIME) %>%
      filter(row_number() == 1)
    
    shipDistData() %>%
      arrange(DATETIME, desc(distanceToNext.Meters)) %>%
      distinct(DATETIME, .keep_all = T) %>%
      filter(DATETIME == DTindex$DATETIME | lag(DATETIME) == DTindex$DATETIME)
  })
  
  
  
  ##Renders####
  #Dropdowns
  callModule(dropdowns, "ShipTypeDropdownId", allShipTypes, "ShipTypeInput", "Ship Type")
  callModule(dropdowns, "vesselDropdownId", vessels, "VesselInput", "Vessel")
  
  #Marker Values
  shipMarkerLegendColors <- c("gray", "blue", "yellow", "red", "green", "pink", "purple", "orange", "lightblue")
  shipMarkerLegendTypes <- c("Cargo", "Fishing", "High Special", "Navigation", "Passenger", "Pleasure", 
                             "Tanker", "Tug", "Unspecified")
  #Map
  output$map <- renderLeaflet({
    #Map of all ships
    if(input$VesselInput == "All"){
      
      dat <- data.frame()
      
      if(input$ShipTypeInput == "All"){
        dat <- allData()
      }
      else{
        dat <- selectedShipTypeData()
      }
      
      allShips <- dat %>%
        distinct(SHIPNAME, .keep_all = T)
      
      legendText = paste(nrow(allShips)," Ships on the Map")
      
      descriptions <- map(paste("Name: ", allShips$SHIPNAME, "<br/>Type: ", allShips$ship_type), HTML)
      
      leaflet(allShips) %>%
        addTiles() %>%
        addAwesomeMarkers(lat = ~LAT, lng = ~LON, icon = ~shipIcon(ship_type), layerId = ~SHIPNAME, label = descriptions, clusterOptions = markerClusterOptions()) %>%
        addResetMapButton() %>%
        addLegendCustom(legendText, "Click on a Ship Icon or use the <br/> Ship Select Dropdowns to find a Ship") %>%
        addLegend("bottomright", colors = shipMarkerLegendColors, labels = shipMarkerLegendTypes, 
                  layerId = shipMarkerLegendTypes, title = "Ship Types")
    }
    #Map of a selected ship
    else{
      startDescription <- map(paste("<p><b>Longest Traveled Start </b><br/> Timestamp: ",  maxShipDistanceTimes()[1,]$DATETIME, 
                                    "<br /> LONG: ", maxShipDistanceTimes()[1,]$LON,
                                    "<br /> LAT: ", maxShipDistanceTimes()[1,]$LAT, "</p>"), HTML)
      
      endDescription <- map(paste("<p><b>Longest Traveled End </b><br/> Timestamp: ", maxShipDistanceTimes()[2,]$DATETIME, 
                                  "<br /> LONG", maxShipDistanceTimes()[2,]$LON,
                                  "<br /> LAT: ", maxShipDistanceTimes()[2,]$LAT, 
                                  "</p>"), HTML)
      
      descriptions <- map(paste("Timestamp: ", shipData()$DATETIME,
                                "<br/> LONG: ", shipData()$LON,
                                "<br/> LAT: ", shipData()$LAT), HTML)
      
      shipName <- shipData()[1,]$SHIPNAME
      shipType <- shipData()[1,]$ship_type
      shipLength <- paste(shipData()[1,]$LENGTH, " M")
      shipwidth <- paste(shipData()[1,]$WIDTH, " M")
      furthestDistance <- paste(maxShipDistanceTimes()[1,]$distanceToNext.Meters, " M")
      totalAISMeasurements <- nrow(shipData())
      averageSpeed <- round(mean(shipData()$SPEED),2)
      
      legendLabels <- c(
                        paste("Ship Name: <b>", shipName, "</b>"),
                        paste("Ship Type: <b>", shipType, "</b>"),
                        paste("Length: <b>", shipLength, " </b>"),
                        paste("Width: <b>", shipwidth, " </b>"),
                        paste("Furthest Distance: <b>", furthestDistance,"</b>"),
                        paste("Total AIS Measurements: <b>", totalAISMeasurements,"</b>"),
                        paste("Average Speed: <b>", averageSpeed, " knots</b>")
                        )
      #If there are not 2 points, or there was an error grabbing the points, do not display distance markers
      validate(
        need(
          nrow(maxShipDistanceTimes() == 2),
               leaflet(shipData()) %>%
                 addTiles() %>%
                 addCircleMarkers(lng = ~LON, lat = ~LAT, radius = 4, fillColor = "red", fillOpacity = 0.35,
                                  stroke = F, label = descriptions) %>%
                 addPolylines(data = maxShipDistanceTimes() %>% select(LON, LAT), lng = ~LON, lat = ~LAT, label = furthestDistance) %>%
                 addResetMapButton() %>%
                 addLegendCustom(legendLabels, "Ship Stats (M = Meters)") %>%
                 addEasyButton(easyButton(id = "AllVessels", icon = "fa-arrow-left", title = "Show all Ships",
                                          onClick = JS(" function(btn, map) { Shiny.onInputChange('map_back', 'back', {priority: 'event'}); }")))
               ))
      
      leaflet(shipData()) %>%
        addTiles() %>%
        addAwesomeMarkers(maxShipDistanceTimes()[1,]$LON, maxShipDistanceTimes()[1,]$LAT, icon = customIcon("green"), label = startDescription) %>%
        addAwesomeMarkers(maxShipDistanceTimes()[2,]$LON, maxShipDistanceTimes()[2,]$LAT, icon = customIcon("orange"), label = endDescription) %>%
        addCircleMarkers(lng = ~LON, lat = ~LAT, radius = 4, fillColor = "red", fillOpacity = 0.35,
                         stroke = F, label = descriptions) %>%
        addPolylines(data = maxShipDistanceTimes() %>% select(LON, LAT), lng = ~LON, lat = ~LAT, label = furthestDistance) %>%
        addResetMapButton() %>%
        addLegendCustom(legendLabels, "Ship Stats (M = Meters)") %>%
        addEasyButton(easyButton(id = "AllVessels", icon = "fa-arrow-left", title = "Show all Ships",
                                 onClick = JS(" function(btn, map) { Shiny.onInputChange('map_back', 'back', {priority: 'event'}); }")))
    }
  })
  
  #Select a ship and ship type on map click
  observeEvent(input$map_marker_click,{
    click <- input$map_marker_click
    
    if(is.null(click$id)){
      return()
    }
    
    updateVesselInput(click$id)
  })
  
  #Set ships to "All" when the back button is hit
  observeEvent(input$map_back,{
    updateVesselInput("All")
  })
  
  #Vessul Update Function
  updateVesselInput <- function(ship_name){
    ship <- allData() %>%
      filter(SHIPNAME == ship_name) %>%
      distinct(SHIPNAME, .keep_all = T)
    
    updateSelectInput(session, "VesselInput", label = "Vessel", choices = vessels(), selected = ship_name)
  }
  
  #Data Table
  output$AISDataTable <- DT::renderDataTable({
    validate(
      need(input$VesselInput != "All", 'Select a ship to view it\'s AIS data'
      )
    )
    DT::datatable(shipDistData() %>%
                  select(DATETIME, LAT, LON, distanceToNext.Meters, SPEED, COURSE, HEADING, DESTINATION, is_parked))
  })
  
  #Tab titles
  output$activetab <- renderText(input$topTabset_tab)
  
  #Test Outputs
  output$debugVessels <- renderPrint({
      vessels()
    })
  
  output$debugMaxShipDistanceTimes <- renderPrint({
    maxShipDistanceTimes()
  })
}

shinyApp(ui, server)
