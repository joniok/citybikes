library(leaflet)

navbarPage("City bike availability", id = "nav",
           
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
                                  
                                  h2("Prediction selection"),

                                  selectInput('range', 'Day', c("Monday", "Tuesday", "Wednesday", 
                                                                "Thursday", "Friday", "Saturday", "Sunday")),
                                  selectizeInput("inputId", "Time", 
                                                 format(seq(ISOdatetime(2001,2,3,0,0,0), 
                                                            ISOdatetime(2001,2,4,0,0,0), 
                                                            by=(60*15)),"%H:%M"), 
                                                 selected = NULL, multiple = FALSE, options = NULL),
                                  
                                  selectizeInput("inputId2", "Station", 
                                                 unique(df$name),
                                                 selected = NULL, multiple = TRUE, options = NULL),
                                  
                                  
                                  plotOutput("hist", height = 200)

                    )
                    ),
                    tags$div(id="cite","Data:", htmlOutput("dataurl")
                    )
           ),
           tabPanel("Plot",
                    titlePanel("Plot goes here"))
)