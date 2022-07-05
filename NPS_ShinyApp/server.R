source(here("NPs_ShinyApp", "global.R"))


server <- function(input, output, session){
    

    #reactive bd map data frame filtering on year and wilderness
    data_reactive <- reactive({
        
        data %>% 
            dplyr::filter(date == input$site_year, wilderness == input$wilderness, species == input$species)
    })
    
    #reactive shape file for wilderness outlines
    
    shape_reactive <- reactive({
        
        shape %>% 
            dplyr::filter(names == input$wilderness)
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
            addCircleMarkers(data = data_reactive(), lng = ~long, lat = ~lat,  color = "blue", radius = 1, 
                             layerId = ~id, label = paste(data_reactive()$id),
                             popup = paste("<B>Year:", data_reactive()$date, "<br>",
                                           "Site:", data_reactive()$id, "(", paste(round(data$lat, 3)), ",", paste(round(data$long, 3)), ")", "<br>", 
                                           "Wilderness:", data_reactive()$wilderness, "<br>",
                                           paste(data_reactive()$species), "Bd Load:", round(data_reactive()$bd, 2), "<br>",
                                           paste(data_reactive()$species), "count:", data_reactive()$count, "<br>"),
                             popupOptions(closeOnClick = T)) %>% 
            addPolylines(data = shape_reactive()$geometry, color = "green", dashArray = T, opacity = 0.9, weight = 1.9,
                         label = paste(shape_reactive()$names),
                         popup = paste("<B> Annual Wilderness Totals <br>",
                                       "Wilderness:", data_reactive()$wilderness, "<br>",
                                       paste(data_reactive()$species), "mean Bd Load:", round(mean(data_reactive()$bd, na.rm = T), 2), "<br>",
                                       paste(data_reactive()$species), "count:", sum(data_reactive()$count, na.rm = T)
                                       ))
        
    })
    
    # observe events to update wilderness and years based on selection for leaflet map
    observeEvent(c(input$site_year, input$wilderness), {
        updateRadioButtons(session, inputId = "species", 
                          choices = unique(data$species[data$date == input$site_year & data$wilderness == input$wilderness]))
       
    })
    
    observeEvent(input$site_year, {
      updateSelectInput(session, inputId = "wilderness", choices = unique(data$wilderness[data$date == input$site_year]))
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
    
