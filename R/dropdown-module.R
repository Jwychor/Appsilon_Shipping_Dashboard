####Dropdown Modules####
library(shiny)
library(ggplot2)

dropdownUI <- function(id){
  ns <- NS(id)
   card(class = "ui card black",
     div(class = "content",
         tagList(
          uiOutput(ns("Dropdown")))
     )
   )
}

dropdowns <- function(input, output, session, dropData, inputName, inputDisplayName){
  
  output$Dropdown <-renderUI({
    selectInput(inputName, inputDisplayName, choices = dropData())
  })
    
}
