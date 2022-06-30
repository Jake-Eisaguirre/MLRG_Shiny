source(here("NPs_ShinyApp", "global.R"))


server <- function(input, output, session){
    

    #reactive bd map data frame filtering on year and wilderness
    data_reactive <- reactive({
        
        data %>% 
            dplyr::filter(date == input$site_year, wilderness == input$wilderness, species == input$species)
    })
    

    

    
    # leaflet map with date, species, and site as reactive 
    output$site_map <- renderLeaflet({
        
        leaflet() %>% 
            addProviderTiles("Esri.WorldImagery") %>% 
            addMouseCoordinates() %>% 
            setView(lng = -119.36697, lat = 37.923288, zoom = 7) %>% 
            addMeasure(
                position = "bottomleft",
                primaryLengthUnit = "feet",
                primaryAreaUnit = "sqfeet",
                activeColor = "#3D535D",
                completedColor = "#7D4479") %>% 
            addCircleMarkers(data = data_reactive(), lng = ~long, lat = ~lat,  color = "blue", radius = 1, layerId = ~id,
                             popup = paste("<B>Year:", data_reactive()$date, "<br>",
                                           "Site:", data_reactive()$id, "(", paste(round(data$lat, 3)), ",", paste(round(data$long, 3)), ")", "<br>", 
                                           "Wilderness:", data_reactive()$wilderness, "<br>",
                                           paste(data_reactive()$species), "Bd Load:", round(data_reactive()$bd, 2), "<br>",
                                           paste(data_reactive()$species), "count:", data_reactive()$count, "<br>"),
                             popupOptions(closeOnClick = T))
        
    })
    
    # observe events to update wilderness and years based on selection for leaflet map
    observeEvent(input$site_year, {
        updateSelectInput(session, inputId = "wilderness", choices = unique(data$wilderness[data$date == input$site_year]), selected = "yosemite")
    })
    
    observeEvent(input$species, {
        updateSelectInput(session, inputId = "site_year", choices = sort(unique(data$date[data$species == input$species]),  decreasing = T))
    })
    

    
    #reactive df for VES
    
    ves_reac <- reactive({
        
        data %>% 
            dplyr::filter(date == input$ves_date, wilderness == input$wilderness_1, 
                          id == input$site, species == input$ves_species)
    })
    
  
    
    
    # bar plot 
    
    output$ves_plots = renderPlot({
        
        ggplot(data = ves_reac(), aes(x = visual_life_stage, y = count)) +
            geom_col() +
            theme_minimal()
            
    })
    
    observeEvent(input$wilderness_1, {
        updateSelectInput(session, inputId = "site", choices = unique(data$id[data$wilderness == input$wilderness_1]), selected = "yosemite")
    })
    
    
}
    
