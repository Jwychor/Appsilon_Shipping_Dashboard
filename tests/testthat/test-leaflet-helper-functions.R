library(testthat)

context("leaflet helpers")

test_that("customIcon()",{
  #color set
  expect_equal(customIcon("blue")$markerColor, "blue")
  #require color argument
  expect_error(customIcon())
  #use ion library
  expect_equal(customIcon("blue")$library, "ion")
})

test_that("shipIcon()",{
  #color works
  expect_equal(shipIcon("Cargo")$markerColor[[1]], "gray")
  #all ship types have a color
  expect_false(any(is.na(shipIcon(c("Cargo","Fishing","High Special","Navigation",
                            "Passenger", "Pleasure", "Tanker", "Tug", "Unspecified"))$markerColor)))
})

test_that("addLegendCustom()",{
  #works
  expect_true(any(class(addLegendCustom(leaflet(), "label1")) == "leaflet"))
  #'addLegend()' called
  expect_equal(addLegendCustom(leaflet(), "label1")$x$calls[[1]]$method, "addLegend")
  #correct # of labels
  expect_equal(length(addLegendCustom(leaflet(), c("label1", "label2"))$x$calls[[1]]$args[[1]]$colors), 2)
})
