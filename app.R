## Example Shiny ScotGov ##

# Load the necessary packages
library(rgdal) ; library(leaflet) ; library(SPARQL) ; library(DT)


# Load the smoking data
endpoint <- "http://statistics.gov.scot/sparql"
query <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?areacode ?areaname ?percsmoking 
WHERE {
?indicator qb:dataSet data:smoking-at-booking ;
sdmxd:refArea ?area ;
sdmxd:refPeriod ?period ;
<http://statistics.gov.scot/def/dimension/populationGroup> <http://statistics.gov.scot/def/concept/population-group/current-smoker> ;
mp:ratio ?percsmoking ;
<http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/gregorian-interval/2013-01-01T00:00:00/P3Y> .
?area stat:code <http://statistics.gov.scot/id/statistical-entity/S13> ;
rdfs:label ?areaname ;
skos:notation ?areacode .
}"

qd <- SPARQL(endpoint,query)

smokpreg <- qd$results

smokpreg$areacode <- substr(smokpreg$areacode,2,10)

# Load the ward boundary vector layer 
scotward <- readOGR("scotwardsmin.geojson", "OGRGeoJSON")

ui <- shinyUI(fluidPage(
  fluidRow(
    column(7, offset = 1,
           br(),
           div(h3("Smoking in Pregnancy")),
           div(h4("The percentage of women who were current smokers at the time of first booking with maternity services, by Scottish Ward")),
           br())),
  fluidRow(
    column(7, offset = 1,
           tabsetPanel(
             tabPanel("Map",leafletOutput("map", height="600"),
                      br(),
                      actionButton("reset_button", "Reset view")),
             tabPanel("Table",DT::dataTableOutput("table"))
           )
           ),
    column(3,
           sliderInput("smokerange", "Choose the range of values you would like to display", min = 0, max = 35, value = c(0,35)),
           div("This is an example of an interactive map, created using RStudio (with various helpful libraries), which visualises data from the Scottish Datastore (a PublishMyData linked data platform). In this example, the data shows the proportion of expectant mothers who are recorded as smoking at the time of booking maternity services."),
           br(),
           div("The slider below allows further interaction with the map by suppressing (in this case) Wards who fall outside the selected range. This could be useful for (for example) selecting areas to target certain grants - such as those to reduce smoking in pregnancy."),
           br(),
           div("Click the 'table' tab at the top to see a table of the filtered data."),
           br()))
  
))

server <- (function(input, output, session) {
  
  selected <- reactive({
    subset(smokpreg,
           percsmoking < input$smokerange[2] & percsmoking >= input$smokerange[1])
  })
  
  lat <- 57.542788
  lng <- -6.144708
  zoom <- 6
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lat = lat, lng = lng, zoom = zoom) %>%
      addPolygons(data = scotward, opacity=1, color = "black", weight = 1, fillOpacity=0.5)
    
  })
  
  output$table <- DT::renderDataTable({
    data.frame(x=selected())}, colnames = c('Ward Code','Ward Name','Percentage'), options = list(order = list(3,'desc')))
    
    observe({
  
      scotward@data <- left_join(scotward@data, selected(), by=c("CODE"="areacode"))
      
      qpal <- colorNumeric("YlGn", smokpreg$percsmoking, na.color = "#bdbdbd")
      
      popup <- paste0("<h5>",scotward$NAME, " (",scotward$FILE_NAME,")</h5><br />",
                      "<h1>",scotward$percsmoking,"%</h1>")
      
      leafletProxy("map", data = scotward) %>%
        clearShapes() %>% 
        clearControls() %>% 
        addPolygons(data = scotward, fillColor = ~qpal(percsmoking), fillOpacity = 0.7, 
                  color = "white", weight = 2, popup = popup) %>%
        addLegend(pal = qpal, values = ~percsmoking, opacity = 0.7,
                  position = 'bottomleft', 
                  title = paste0("Antenatal Smoking Rates"))
  })


})

shinyApp(ui, server)