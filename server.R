server <- function(input,output, session){
  
  data <- reactive({
    x <- df
  })
  
  output$dataurl <- renderUI({
    a("HSL city bike stations history data (2018)", target="_blank", href = "https://dev.hsl.fi/")
  })
  
  output$hist <- renderPlot({
    hist(df$avl_bikes)
  })

  output$mymap <- renderLeaflet({
    df <- data()
    
    
    getColor <- function(df) {
      sapply(df$avl_bikes, function(x) {
        if(x < 30) {
          "green"
        } else if(x <= 13) {
          "orange"
        } else {
          "red"
        } })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(df)
    )
    
    m <- leaflet(data = df) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~Longitude,
                 lat = ~Latitude,
                 popup =  df$name,
                 icon = icons,
                 label = ~htmlEscape(df$name)
                 )
    m
  })
}