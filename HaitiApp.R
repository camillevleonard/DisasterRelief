library(shiny)
library(dplyr)
library(plotly)
library(leaflet)
library(shiny)

pixelData <- read.csv("Data/HaitiPixels.csv", stringsAsFactors = FALSE) %>%
  filter(Class == "Blue Tarp")
pixelData$lat <- runif(nrow(pixelData), 17.809852, 20.028771)
pixelData$lon <- runif(nrow(pixelData), -74.318636, -71.771760)
pixelData$Date <- sample(seq(as.Date('2019/01/01'), as.Date('2019/04/28'), by="day"), nrow(pixelData), replace = TRUE)
pixelData$Identified <- sample(c(1, 0), nrow(pixelData), replace = TRUE)
pixelData$ID <- row.names(pixelData)

ui <- bootstrapPage(
  navbarPage(theme = shinythemes::shinytheme("sandstone"), 
             collapsible = TRUE,
             "Shelter Locator", 
             id="nav",
             tabPanel("Identfied Shelters",
                      div(class="outer",
                          tags$style("#mainMap {position: fixed; margin: -15px;}"),
                          leafletOutput("mainMap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 300, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("The values given below are based on supah sweet algorithms that are 'looking' for tarps")), style="color:#045a8d"),
                                        h3("Last Updated:"),
                                        h3(textOutput("lastModelUpdate"), align = "right"),
                                        h4("Total Identified Shelters:"),
                                        h4(textOutput("totalIdentified"), align = "right"),
                                        h4("Total Pending Images:"),
                                        h4(textOutput("totalPending"), align = "right"),
                                        div(plotlyOutput("identifiedTimePlot", height="200px", width="100%"), align = "left")
                          )
                      )
             ),
             
             tabPanel("Pending Images",
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h6("This is where we would give you the legal disclaimer that nothing we do should be taken very seriously. You should
                                         both trust us and be entirely skeptical of us.")), style="color:#045a8d"),
                          span(tags$i(h6("Hopefully the above made no sense and total sense.")), style="color:#045a8d"),
                          
                          selectInput("departmentSelect", "Department:",
                                      choices = c("Artibonite", "Centre", "Grand'Anse", "Nippes", "Nord", "Nord-Est", "Nord-Ouest", "Ouest", "Sud", "Sud-Est"),
                                      multiple = FALSE),
                          sliderInput("minProbability",
                                      "Minimum Probability:",
                                      min = 0,
                                      max = 1,
                                      value = .7)
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Image Validation",
                                     tags$style("img {height: inherit; display: flex; margin: auto;} .btn {margin: auto; width: 20%;}"),
                                     span(tags$i(h4("Is there a blue tarp in the image provided below?")), style="color:#045a8d; text-align: center;"),
                                     plotOutput("imageDisplay", width = "100%", height = "300px"),
                                     fluidRow(
                                       style = "margin-top: 20px; display: flex;",
                                       actionButton("yesTarp", "Yes"),
                                       actionButton("noTarp", "No")
                                     )
                            )
                          )
                        )
                      )
             )
             
  )          
)

### SHINY SERVER ###

server = function(input, output, session) {
  
  values <- reactiveValues()
  values$ImageLabels <- data.frame("Image" = character(0), "Label" = character(0))
  
  #Observers
  observeEvent(input$yesTarp, {
    values$ImageLabels <- rbind(values$ImageLabels, data.frame("Image" = values$Image, "Label" = "Blue Tarp"))
    values$Image <- NA
  })
  
  observeEvent(input$noTarp, {
    values$ImageLabels <- rbind(values$ImageLabels, data.frame("Image" = values$Image, "Label" = "No Tarp"))
    values$Image <- NA
  })
  
  # Identified Tab
  output$lastModelUpdate <- renderText({
    format(Sys.Date(),"%d %B %Y")
  })
  
  output$totalIdentified <- renderText({
    sum(pixelData$Identified)
  })
  
  output$totalPending <- renderText({
    nrow(pixelData) - sum(pixelData$Identified)
  })
  
  output$identifiedTimePlot <- renderPlotly({
    data <- pixelData %>%
      dplyr::group_by(Date) %>%
      dplyr::summarise(TotalIdentified = sum(Identified)) %>%
      dplyr::arrange(Date)
    
    pal <- c("#703378", "#325ea9", "#bc506c", "#c5d3ff", "#e2655b", "#76cab8", "#74b7c7")
    
    p <- plot_ly(type = 'scatter', mode = 'lines', source = 'MainTime') %>%
      layout(barmode = 'stack',
             xaxis = list(title = "", color = "#000000"),
             yaxis = list(title ="", color = "#000000"),
             legend = list(orientation = 'h', font=list(color = "#000000", size = 14))) %>%
      layout(plot_bgcolor='#ffffff') %>%
      layout(paper_bgcolor='#ffffff') %>%
      config(displayModeBar = F) %>%
      layout(showlegend = FALSE) %>%
      event_register('plotly_relayout')
    
      p <- add_trace(p,
                     x = data$Date,
                     y = data$TotalIdentified,
                     mode ='lines',
                     connectgaps = TRUE,
                     line = list(color = "#703378"),
                     name = "Total Identified",
                     hovertemplate = paste(
                       "<b>Total Identified</b><br>",
                       "Value: %{y}<br>",
                       "Date: %{x}<br>",
                       "<extra></extra>"
                     )
      )
    p
  })
  
  output$mainMap <- renderLeaflet({
    data <- pixelData %>%
      dplyr::filter(Identified == 1)
    
    leaflet(data[c(1:500),]) %>%
      addProviderTiles(providers$CartoDB) %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, layerId = ~ID,
                       label = ~lapply(as.list(ID), HTML),
                       # color = "yellow",
                       clusterOptions = markerClusterOptions(iconCreateFunction =
                                                               JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>%
      leaflet.extras::addHeatmap(lng=~lon, lat=~lat, radius = 5, blur = 10)
  })
  
  output$imageDisplay <- renderImage({
    values$Image <- paste('TarpImage', sample(c(1,2,3), 1), '.jpg', sep='')
    filename <- normalizePath(file.path('./Data', values$Image))

    list(src = filename)
  }, deleteFile = FALSE)
  
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)