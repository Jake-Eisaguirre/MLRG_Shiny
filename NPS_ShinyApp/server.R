source(here("NPs_ShinyApp", "global.R"))

# leaflet base map
server <- function(input, output, session){
    
    #create leaflet base map
    output$site_map <- renderLeaflet({
    leaflet() %>% 
        addProviderTiles("Esri.WorldImagery") %>% 
        addMouseCoordinates() %>% 
        setView(lng = -118.734441, lat = 37.433288, zoom = 7) %>% 
        addMeasure(
                position = "bottomleft",
                primaryLengthUnit = "feet",
                primaryAreaUnit = "sqfeet",
                activeColor = "#3D535D",
                completedColor = "#7D4479") 
    })
    
    
    
}