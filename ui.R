library(leaflet)

navbarPage("Helsinki city bike availability", id = "nav",
           
           tabPanel("Map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        leafletOutput("mymap", width="100%", height="100%"),
                        
                        absolutePanel(id = "controls", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h3("Prediction selection"),
                                      
                                      selectInput('currDay', 'Day', unique(df$day), selected = unique(df$day)[1]),
                                      selectizeInput("currTime", "Time", unique(df$time), 
                                                     selected = "16:15", multiple = FALSE, options = NULL),
                                      
                                      selectizeInput("stations", "Station", 
                                                     unique(df$name),
                                                     selected = NULL, multiple = TRUE, options = NULL)
                        )
                    ),
                    tags$div(id="cite","Data:", htmlOutput("dataurl")
                    )
           ),
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                        selectizeInput('plot_station', 'Station', unique(df$name),
                                       selected = unique(df$name)[1], multiple = FALSE),
                        selectInput('dataDay', 'Day', unique(df$day), selected = unique(df$day)[1])
                      ),
                      mainPanel(
                        plotOutput("pred_plots", width ="100%")
                      )
                    )
           ),
           
           
           tabPanel("Help",
                    absolutePanel(
                      includeMarkdown("help/help.md"))
                    
           )
)