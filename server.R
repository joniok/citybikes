server <- function(input,output, session){
  
  data <- reactive({
    
    if(is.null(input$stations)){
      x <- df[(df$time == input$currTime) &  (df$day == input$currDay),]
    } else {
      x <- df[(df$time == input$currTime) &  (df$day == input$currDay) & (df$name %in% input$stations),]    }
  })
  
  output$dataurl <- renderUI({
    a("HSL city bike stations history data (2018)", target="_blank", href = "https://dev.hsl.fi/")
  })
  
  output$pred_plots <- renderPlot({
    #time_sequence <-seq(5,22, by= 0.25)
    plot(pred_bikes~ x, data = df)
  })
  
  output$mymap <- renderLeaflet({
    df <- data()
    
    
    getColor <- function(df) {
      sapply(df$pred_color, function(x) {
        if(x >= 0.90) {
          "green"
        } else if(x >= 0.35) {
          "orange"
        } else if (x > 0.10){
          "red"
        } else {
          "gray"
        } }
      )
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(df)
    )
    
    m <- leaflet(data = df) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~long,
                        lat = ~lat,
                        popup =  paste(df$name,"<br>", paste0(df$pred_bikes,"/",df$slots), "bikes available"),
                        icon = icons,
                        label = ~htmlEscape(df$name)
      )
    m
  })
}