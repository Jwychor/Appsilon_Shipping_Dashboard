library(leaflet)

customIcon <- function(color){
  awesomeIcons(
    icon = 'ios-close',
    iconColor = 'white',
    library = 'ion',
    markerColor = color
  )
}

shipIcon <- function(ship_type){
  color = map(ship_type, function(ship_type){
    switch(ship_type,  
           "Cargo" = "gray", 
           "Fishing" = "blue", 
           "High Special" = "yellow",
           "Navigation" = "red", 
           "Passenger" = "green", 
           "Pleasure" = "pink", 
           "Tanker" = "purple", 
           "Tug" = "orange", 
           "Unspecified" = "lightblue"
    )
  }) 
  
  awesomeIcons(
    icon = 'fa-ship',
    iconColor = 'black',
    markerColor = color,
    library = 'fa'
  )
}

addLegendCustom <- function(map, labels, legend_title = ""){
  legend_shape <- paste0(";margin-top: 5px; color: black; width: 5px; 
                         height: 6px; border: 3px solid; border-radius: 50%;
                         position: inline-block")
  
  make_labels <- function(labels) {
    paste0("<div style='display: inline-block;height: 12px;4px;line
           margin-top: -height: 12px;'>", 
           labels, "</div>")
  }
  
  legend_colors <- rep(legend_shape, length(labels))
  legend_labels <- make_labels(labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, position = "topright", title = legend_title))
}