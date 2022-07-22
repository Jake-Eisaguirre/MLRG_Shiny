source(here("NPs_ShinyApp", "global.R"))


server <- function(input, output, session){
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })

    #reactive ves map data frame filtering on year, wilderness, species, and life stage
    data_reactive <- reactive({
      
      
        ves_data %>%
            dplyr::filter(date <= input$site_year[2] & date >= input$site_year[1], wilderness == input$wilderness, 
                          species == input$species, visual_life_stage == input$stage) %>% 
        group_by(id, wilderness, species, visual_life_stage) %>% 
        mutate(sum_count = sum(count))
      
      
    })
    
    #ractive bd map data filtering on year, wilderness, species, and life stage
    data_reactive_bd <- reactive({ 
 
  
      
      bd_data %>%
        dplyr::filter(date <= input$site_year[2] & date >= input$site_year[1], wilderness == input$wilderness, 
                      species == input$species, visual_life_stage == input$stage) %>% 
        mutate(med_bd = mean(bd))
      
      
    })
    
    
    #reactive shape file for wilderness outlines
    
    shape_reactive <- reactive({
      

        shape %>% 
            dplyr::filter(names == input$wilderness)
    })

    

    
    # leaflet map with date, species, and site as reactive 
    output$site_map <- renderLeaflet({
      
      
      validate(
        need(input$wilderness, "Please select a wilderness"),
        need(input$species, "Please select a species"),
        need(input$stage, "Please select a life stage"))
      
      
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
                                           
                                           "Site:", data_reactive()$id, "(", paste(round(ves_data$lat, 3)), 
                                                    ",", paste(round(ves_data$long, 3)), ")", "<br>", 
                                           
                                           "Wilderness:", data_reactive()$wilderness, "<br>",
                                           
                                           data_reactive_bd()$species, "median log(Bd) Load:", round(data_reactive_bd()$bd, 2), "<br>",
                                           
                                           data_reactive()$visual_life_stage, data_reactive()$species, "count:", data_reactive()$sum_count, "<br>"),
                             
                             popupOptions(closeOnClick = T)) %>% 
          
            addPolylines(data = shape_reactive()$geometry, color = "green", dashArray = T, opacity = 0.9, weight = 1.9,
                         label = paste("Wilderness:", shape_reactive()$names),
                         popup = paste("<B>", input$site_year[1], "-", input$site_year[2], "Wilderness Totals <br>",
                                       
                                       "Wilderness:", data_reactive()$wilderness, "<br>",
                                       
                                       "adult", paste(data_reactive_bd()$species), 
                                             "median wilderness log(Bd) Load:", round(mean(data_reactive_bd()$med_bd), 2), "<br>",
                                       
                                       paste(data_reactive()$visual_life_stage ,paste(data_reactive()$species), 
                                            "count:", sum(data_reactive()$count))))
        
    })
    
    # observe events to update wilderness and years based on selection for leaflet map
    observeEvent(input$site_year, {
      
      updatePickerInput(session, inputId = "wilderness", 
                        choices = unique(ves_data$wilderness[ves_data$date <= input$site_year[2]
                                                             & ves_data$date >= input$site_year[1]]))
    })
    
    observeEvent(input$wilderness, {
      
      updatePickerInput(session, inputId = "species", 
                          choices = unique(ves_data$species[ves_data$date <= input$site_year[2]
                                                            & ves_data$date >= input$site_year[1] 
                                                            & ves_data$wilderness == input$wilderness]))
       
    })
    
    observeEvent(input$species, {
      
      updatePickerInput(session, inputId = "stage", 
                        choices = unique(ves_data$visual_life_stage[ves_data$date <= input$site_year[2]
                                                                    & ves_data$date >= input$site_year[1] 
                                                                    & ves_data$wilderness == input$wilderness 
                                                                    & ves_data$species == input$species]))
    })
    
    
    


    
    

    
    #reactive df for VES
    
    ves_reac <- reactive({

    
      
        ves_data %>%
            dplyr::filter(date <= input$ves_date[2] & date >= input$ves_date[1],
                          wilderness == input$wilderness_1, species == input$ves_species, id == input$id) %>% 
        group_by(visual_life_stage) %>% 
        summarise(count = sum(count))
            
    })
    
  
    
    
    # bar plot 
    
    output$ves_plots = renderPlot({
      
      
      validate(
        need(input$wilderness_1, "Please select a wilderness & species"),
        need(input$ves_species, "Please select a species"))
 
      
      ggplot(data = ves_reac(), aes(x = visual_life_stage, y = count, fill = visual_life_stage)) +
        geom_col() +
        theme_minimal() +
        scale_x_discrete("Visual Life Stage", limits=c("adult", "subadult", "tadpole", "eggmass")) +
        geom_text(aes(label = paste("count:", ves_reac()$count)), vjust = -0.5) +
        ylab("Count") +
        scale_fill_manual(values = c("adult" = "green", "subadult" = "blue", "tadpole" = "red", "eggmass" = "purple"),
                          name = "Visual Life Stage") +
        ggtitle(paste(input$ves_date[1], "-", input$ves_date[2], input$ves_species, "Annual Count")) +
        theme(plot.title = element_text(hjust = 0.5))
      

            
    })
    
    
    # observe events to update wilderness and years based on selection for leaflet map
    observeEvent(input$ves_date, {
      
      updatePickerInput(session, inputId = "wilderness_1", 
                        choices = unique(ves_data$wilderness[ves_data$date <= input$ves_date[2] 
                                                             & ves_data$date >= input$ves_date[1]]))
    })
    
    observeEvent(input$wilderness_1, {
      
      updatePickerInput(session, inputId = "ves_species", 
                        choices = unique(ves_data$species[ves_data$date <= input$ves_date[2] 
                                                          & ves_data$date >= input$ves_date[1] 
                                                          & ves_data$wilderness == input$wilderness_1]))
      
    })
    
    observeEvent(input$ves_species, {
      
      updatePickerInput(session, inputId = "id", 
                        choices = unique(ves_data$id[ves_data$date <= input$ves_date[2] 
                                                     & ves_data$date >= input$ves_date[1] 
                                                     & ves_data$wilderness == input$wilderness_1 
                                                     & ves_data$species == input$ves_species]))
    })
    
    
    
    
    
    bd_reac <- reactive({

      
      bd_data %>% 
        dplyr::filter((date <= input$bd_date[2] & date >= input$bd_date[1]), 
                      wilderness == input$wilderness_2, species == input$bd_species, 
                      visual_life_stage == input$stage_bd, id == input$bd_id)
      
    })
    
    output$bd_plots <- renderPlot({
      
      validate(
        need(input$wilderness_2, "Please select a wilderness & species"),
        need(input$bd_species, "Please select a species"),
        need(input$stage_bd, "Please select a life stage"),
        need(input$bd_id, "Please select a site ID"))

      
      ggplot(data = bd_reac(), aes(x = date, y = bd)) +
        geom_point() +
        geom_line(aes(x=date, y = bd)) +
        xlim(c(input$bd_date[1:2])) +
        ylab("Median log(Bd)") +
        xlab("Year") +
        ggtitle(paste(input$bd_date[1], "-", input$bd_date[2], input$bd_species, "Median Log(Bd)")) +
        geom_text_repel(aes(label = paste(round(bd_reac()$bd, 3)))) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    
    })
    
    observeEvent(input$bd_date, {
      
      updatePickerInput(session, inputId = "wilderness_2", 
                        choices = unique(bd_data$wilderness[bd_data$date <= input$bd_date[2] 
                                                             & bd_data$date >= input$bd_date[1]]))
    })
    
    observeEvent(input$wilderness_2, {
      
      updatePickerInput(session, inputId = "bd_species", 
                        choices = unique(bd_data$species[bd_data$date <= input$bd_date[2] 
                                                          & bd_data$date >= input$bd_date[1] 
                                                          & bd_data$wilderness == input$wilderness_2]))
      
    })
    
    observeEvent(input$bd_species, {

      updatePickerInput(session, inputId = "stage_bd",
                        choices = unique(bd_data$visual_life_stage[bd_data$date <= input$bd_date[2]
                                                     & bd_data$date >= input$bd_date[1]
                                                     & bd_data$wilderness == input$wilderness_2
                                                     & bd_data$species == input$bd_species]))
    })
    
    observeEvent(input$stage_bd, {

      updatePickerInput(session, inputId = "bd_id",
                        choices = unique(bd_data$id[bd_data$date <= input$bd_date[2]
                                                    & bd_data$date >= input$bd_date[1]
                                                    & bd_data$wilderness == input$wilderness_2
                                                    & bd_data$species == input$bd_species
                                                    & bd_data$visual_life_stage == input$stage_bd]))

    })
    

    
    
    # observeEvent(input$bd_date, {
    #   
    #   updatePickerInput(session, inputId = "wilderness_2", 
    #                     choices = unique(bd_data$wilderness[bd_data$date <= input$bd_date[2]
    #                                                         & bd_data$date >= input$bd_date[1]]))
    # })
    # 
    # 
    # observeEvent(input$wilderness_2, {
    #   
    #   updatePickerInput(session, inputId = "bd_species", 
    #                     choices = unique(bd_data$species[bd_data$date <= input$bd_date[2]
    #                                                      & bd_data$date >= input$bd_date[1] 
    #                                                      & bd_data$wilderness == input$wilderness_2]))
    #   
    # })
    # 
    # observeEvent(input$bd_species, {
    #   
    #   updatePickerInput(session, inputId = "stage", 
    #                     choices = unique(bd_data$visual_life_stage[bd_data$date <= input$bd_date[2]
    #                                                                & bd_data$date >= input$bd_date[1] 
    #                                                                & bd_data$wilderness == input$wilderness_2 
    #                                                                & bd_data$species == input$bd_species]))
    # })
    # 

    
    
}
    
