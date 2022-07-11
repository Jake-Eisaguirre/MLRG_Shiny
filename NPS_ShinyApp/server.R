source(here("NPs_ShinyApp", "global.R"))


server <- function(input, output, session){
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })

    #reactive ves map data frame filtering on year, wilderness, species, and life stage
    data_reactive <- reactive({
      
      
        data %>%
            dplyr::filter(date >= input$site_year[1], date <= input$site_year[2], wilderness == input$wilderness, 
                          species == input$species, visual_life_stage == input$stage) %>% 
        group_by(id) %>% 
        mutate(sum_count = sum(count),
               mean_bd = mean(bd))
      
      
    })
    
    #ractive bd map data filtering on year, wilderness, species, and life stage
    data_reactive_bd <- reactive({
      
      
      bd_data %>%
        group_by(id) %>% 
        dplyr::filter(date >= input$site_year[1], date <= input$site_year[2], wilderness == input$wilderness, 
                      species == input$species) %>% 
        mutate(mean_bd = mean(bd),
               bd = bd)
      
      
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
                             layerId = ~id, label = paste('Site:', data_reactive()$id),
                             popup = paste("<B>Year:",input$site_year[1], "-", input$site_year[2], "<br>",
                                           
                                           "Site:", data_reactive()$id, "(", paste(round(data$lat, 3)), 
                                                    ",", paste(round(data$long, 3)), ")", "<br>", 
                                           
                                           "Wilderness:", data_reactive()$wilderness, "<br>",
                                           
                                           data_reactive()$species, "Bd Load:", round(data_reactive()$mean_bd, 2), "<br>",
                                           
                                           data_reactive()$visual_life_stage, data_reactive()$species, "count:", data_reactive()$sum_count, "<br>"),
                             
                             popupOptions(closeOnClick = T)) %>% 
            addPolylines(data = shape_reactive()$geometry, color = "green", dashArray = T, opacity = 0.9, weight = 1.9,
                         label = paste("Wilderness:", shape_reactive()$names),
                         popup = paste("<B>", input$site_year[1], "-", input$site_year[2], "Wilderness Totals <br>",
                                       
                                       "Wilderness:", data_reactive()$wilderness, "<br>",
                                       
                                       "adult", paste(data_reactive()$species), 
                                             "mean Bd Load:", round(mean(data_reactive()$mean_bd, na.rm = T), 2), "<br>",
                                       
                                       paste(data_reactive()$visual_life_stage ,paste(data_reactive()$species), 
                                            "count:", sum(data_reactive()$count, na.rm = T))))
        
    })
    
    # observe events to update wilderness and years based on selection for leaflet map
    observeEvent(input$site_year, {
      
      updatePickerInput(session, inputId = "wilderness", 
                        choices = unique(ves_data$wilderness[ves_data$date %in% input$site_year[1:2]]),
                        selected = "yosemite")
    })
    
    observeEvent(input$wilderness, {
      
      updatePickerInput(session, inputId = "species", 
                          choices = unique(ves_data$species[ves_data$date %in% input$site_year[1:2] 
                                                        & ves_data$wilderness == input$wilderness]))
       
    })
    
    observeEvent(input$species, {
      
      updatePickerInput(session, inputId = "stage", 
                        choices = unique(ves_data$visual_life_stage[ves_data$date %in% input$site_year[1:2] 
                                                                & ves_data$wilderness == input$wilderness 
                                                                & ves_data$species == input$species]))
    })
    
    
    


    
    

    
    #reactive df for VES
    
    ves_reac <- reactive({
      
      
        ves_data %>%            
        group_by(visual_life_stage) %>% 
            dplyr::filter(date %in% input$ves_date[1:2],
                          wilderness == input$wilderness_1, species == input$ves_species, id %in% input$id) 
            
    })
    
  
    
    
    # bar plot 
    
    output$ves_plots = renderPlot({
      
      
      ggplot(data = ves_reac(), aes(x = visual_life_stage, y = count, fill = visual_life_stage)) +
        geom_col() +
        theme_minimal() +
        scale_x_discrete("Visual Life Stage", limits=c("adult", "subadult", "tadpole")) +
        geom_text(aes(label = paste("count:", ves_reac()$count)), vjust = -0.5) +
        ylab("Count") +
        scale_fill_manual(values = c("adult" = "green", "subadult" = "blue", "tadpole" = "red"))
      

            
    })
    
    
    # observe events to update wilderness and years based on selection for leaflet map
    observeEvent(input$ves_date, {
      
      updatePickerInput(session, inputId = "wilderness_1", 
                        choices = unique(ves_data$wilderness[ves_data$date %in% input$ves_date[1:2]]), 
                        selected = "yosemite")
    })
    
    observeEvent(input$wilderness_1, {
      
      updatePickerInput(session, inputId = "ves_species", 
                        choices = unique(ves_data$species[ves_data$date %in% input$ves_date[1:2] 
                                                      & ves_data$wilderness == input$wilderness_1]))
      
    })
    
    observeEvent(input$ves_species, {
      
      updatePickerInput(session, inputId = "id", 
                        choices = unique(ves_data$id[ves_data$date %in% input$ves_date[1:2] 
                                                                & ves_data$wilderness == input$wilderness_1 
                                                                & ves_data$species == input$ves_species]))
    })
    
    bd_reac <- reactive({
      
      bd_data %>% 
        dplyr::filter(date %in% input$bd_date[1:2]) %>% 
        group_by(wilderness) %>% 
        summarise(mean_bd = round(mean(bd), 2 )) %>% 
        mutate(csum = rev(cumsum(rev(mean_bd))), 
               pos = mean_bd/2 + lead(csum, 1),
               pos = if_else(is.na(pos), mean_bd/2, pos))
      
    })
    
    output$bd_plots <- renderPlot({
      
      ggplot(data = bd_reac(), aes(x = "", y = mean_bd, fill = wilderness)) +
        geom_bar(stat = "identity", width =20, color = "black")+
        coord_polar("y", start = 0) +
        theme_void() +
        geom_label_repel(data = bd_reac(),
                         aes(y = pos, label = mean_bd),
                         size = 4.5, nudge_x = 20, show.legend = FALSE) +
        theme(legend.position = "bottom",
              title = element_text(face = "bold")) +
        ggtitle(paste(input$bd_date[1], "-", input$bd_date[2], "Wilderness Bd Load"))
    })
    
}
    
