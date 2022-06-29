source(here("NPs_ShinyApp", "global.R"))


server <- function(input, output, session){
    
    data_of_click <- reactiveValues(clickedMarker=NULL)
    

    #reactive bd map data frame filtering on year and wilderness
    data_reactive <- reactive({
        
        data %>% 
            dplyr::filter(date == input$site_year, wilderness == input$wilderness)
    })
    

    
    #species list per site id
    
    species_list_reac <- reactive({
        
        data %>% 
            group_by(id, date) %>% 
            filter(species == "ramu",
                   date == input$site_year, wilderness == input$wilderness)
    })    
    
    
    # leaflet base map
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
                completedColor = "#7D4479") %>% 
            addCircleMarkers(data = data_reactive(), lng = ~long, lat = ~lat,  color = "blue", radius = 1, layerId = ~id,
                             popup = paste("Year:", data_reactive()$date, "<br>",
                                           "Site:", data_reactive()$id, "<br>", 
                                           "Bd Load:", round(data_reactive()$bd, 2), "<br>"))
        
    })
    
    #store the click
    observeEvent(input$map_marker_click, (
        
        data_of_click$clickedMarker <- input$map_marker_click
    ))
    
    output$plot = renderPlot({
        
        my_place = data_of_click$clickedMarker$id
        if(is.null(my_place)){my_place="place1"}
        if(my_place == "place1"){
            plot(rnorm(1000), col=rgb(0.9,0.4,0.1,0.3), cex=3, pch=20)
        }else{
            barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
        }
    })
    
    
    #reactive df for VES
    
    ves_reac <- reactive({
        
        data %>% 
            dplyr::filter(date == input$ves_date, wilderness == input$wilderness_1, 
                          id == input$site, species == input$ves_species)
    })
    
    observe({
        updateSelectInput(session, inputId = "date", choices = ves_reac()$id)
        updateSelectInput(session, inputId = "wilderness_1", choices = ves_reac()$wilderness)
    })
    
    
    # bar plot 
    
    output$ves_plots = renderPlot({
        
        ggplot(data = ves_reac(), aes(x = visual_life_stage, y = count)) +
            geom_col() +
            theme_minimal()
            
    })
    
}
    
