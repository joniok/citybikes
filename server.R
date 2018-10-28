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
    plot_data <- df
    if(is.null(input$stations)){
    }
    plot_data <- plot_data[(plot_data$name == input$plot_station) & (plot_data$day == input$dataDay),]
    plot(plot_data$pred_bikes ~ plot_data$x, 
         xlim = c(5,22),
         xlab = "Predicted bikes",
         ylab = "Time",
         main=paste(plot_data$name[1], 
                    plot_data$day[1], 
                    sep = ", "))
  })
  
  output$mymap <- renderLeaflet({
    df <- data()
    
    
    getColor <- function(df) {
      sapply(df$pred_color, function(x) {
        if(x >= 0.75) {
          "green"
        } else if(x >= 0.50) {
          "orange"
        } else if (x >= 0.25){
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