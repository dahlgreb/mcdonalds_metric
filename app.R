library(tidyverse)
library(shiny)
library(leaflet)
library(rgdal)


ui <- navbarPage("Global McDonals'", id="main",
                 tabPanel("Map", sliderInput(inputId = "num",
                                             label = "Choose the number of McDonalds'",
                                             value = 25, min = 1, max = 15000),
                          leafletOutput("mdmap", height=1000)),
                 tabPanel("Prediction", htmlOutput("inc"))
)

server <- function(input, output, session){
  print("SERVER STARTED!")
  # Preprocess data to fit with shape files
  print("PROCESSING DATA")
  md_data <- read.csv("McDonalds.csv")[-1]
  countries <- readOGR("Igismap/TM_WORLD_BORDERS-0.3.shp",
                       layer = "TM_WORLD_BORDERS-0.3", GDAL1_integer64_policy = TRUE)
  
  Name <- as.character(countries$name[!(countries$name %in% md_data$Name)])
  num_new <- length(Name)
  First_opened <- rep(NA, num_new)
  First_location <- rep(NA, num_new)
  Count <- rep(0, num_new)
  Source <- rep(NA, num_new)
  People_per_md <- rep(NA, num_new)
  Notes <- rep(NA, num_new)
  
  more_countries <- data.frame(Name,First_opened, First_location, Count, Source, People_per_md, Notes)
  md_data <- rbind(md_data, more_countries)
  md_data <- md_data[md_data$Name %in% countries$name,]
  
  md_data$name <- md_data$Name
  countries <- merge(x=countries, y=md_data, by= "name", all.x = TRUE)
  
  
  # from https://www.kaggle.com/fernandol/countries-of-the-world
  countries_data <- read.csv("countries of the world.csv")
  countries_data <- countries_data %>%
    mutate(Country = str_trim(Country))
  
  countries_data$Country <- countries_data$Country %>%
    str_replace("Laos", "Lao People's Democratic Republic") %>%
    str_replace("Bahamas, The", "Bahamas") %>%
    str_replace("Korea, South", "Korea, Republic of") %>%
    str_replace("Vietnam", "Viet Nam") %>%
    str_replace("Bosnia & Herzegovina", "Bosnia and Herzegovina") %>%
    str_replace("Congo, Repub. of the", "Congo") %>%
    str_replace("Korea, North", "Korea, Democratic People's Republic of") %>%
    str_replace("Iran", "Iran (Islamic Republic of)")
  
  
  countries_data <- countries_data %>%
    filter(countries_data$Country %in% md_data$name) %>%
    mutate(Name = Country, name = Country)
  
  countries_data$md_count <- merge(x=countries_data, y=md_data, by= "Name", all.x = TRUE)$Count
  countries_data$GDP <- countries_data$Population*(countries_data$GDP....per.capita./1000000)
  
  countries_data <- countries_data %>%
    mutate(cntnt=paste0('<strong>Country: </strong>',Country,
                                          '<br><strong>McDonalds Count:</strong> ', md_count,
                                          '<br><strong>est GDP(USD, Millions):</strong> ',GDP))
  countries <- merge(x=countries, y=countries_data, by= "name", all.x = TRUE)
  
  output$mdmap <- renderLeaflet({
    leaflet()
  })
  ## filter data
  valid_countries <- reactive({
    countries[countries$Count>=input$num,]
  })
  
  ## respond to the filtered data
  observe({
    print("EVALUATING COUNTRIES")
    leafletProxy(mapId = "mdmap", data = valid_countries()) %>%
      clearShapes() %>%
      addTiles() %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = "red",
                  popup = ~cntnt,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  getPage<-function() {
    return(includeHTML("analysis.html"))
  }
  output$inc<-renderUI({getPage()})
}

shinyApp(ui = ui, server = server)

