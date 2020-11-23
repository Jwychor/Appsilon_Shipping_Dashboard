library(testthat)
library(shiny)


test_that("Ship Type Input",{
  testServer(expr = {
    #Input received
    session$setInputs(ShipTypeInput = "Cargo")
    expect_equal(input$ShipTypeInput, "Cargo")
    #Vessels passed data
    session$setInputs(ShipTypeInput = "Cargo")
    expect_true(grepl("PRINCE OF WAVES", output$debugVessels))
  })
})

test_that("Vessel Input",{
  testServer(expr = {
    #Input received
    session$setInputs(VesselInput = "AGATH")
    expect_equal(input$VesselInput, "AGATH")
    #Data passed to data table correctly
    session$setInputs(VesselInput = "AGATH")
    expect_true(grepl("distanceToNext.Meters", output$AISDataTable))
    #Max Ship Distance Times recorded correctly
    session$setInputs(VesselInput = "AGATH")
    expect_true(grepl("1259.31",output$debugMaxShipDistanceTimes))
  })
})
