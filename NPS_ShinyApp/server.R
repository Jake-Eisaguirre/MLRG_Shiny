source(here("NPS_ShinyApp", "global.R"))
source(here("NPS_ShinyApp", "creds.R"))

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
        mutate(sum_count = sum(count),
               med = mean(bd),
               bd = bd)
      
      
    })
    
    #ractive bd map data filtering on year, wilderness, species, and life stage
    data_reactive_bd <- reactive({ 
 
  
      
      bd_data %>%
        dplyr::filter(date <= input$site_year[2] & date >= input$site_year[1], wilderness == input$wilderness, 
                      species == input$species, visual_life_stage == input$stage) %>% 
        group_by(id, species, visual_life_stage) %>% 
        mutate(med_bd = mean(bd),
                  bd = bd,
                  id = order(id, decreasing = T))
      
      
    })
    
    
    #reactive shape file for wilderness outlines
    
    shape_reactive <- reactive({
      

        shape %>% 
            dplyr::filter(names == input$wilderness)
    })

  

    
    # leaflet map with date, species, and site as reactive 
    output$site_map <- renderLeaflet({
      
      
      if(input$wilderness < 0 && input$species < 0 && input$stage < 0) {
        validate("Please select a wilderness, species, and life stage")
      }

      if(input$wilderness > 0 && input$species < 0 && input$stage < 0){
        validate("Please select a species and life stage")
      }

      if(input$wilderness > 0 && input$species > 0 && input$stage < 0){
        validate("Please select a life stage")
      }

      
      leaflet() %>% 
        addProviderTiles("Esri.WorldImagery") %>% 
        addMouseCoordinates() %>% 
        setView(lng = -119.36697, lat = 37.3, zoom = 7) %>% 
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "feet",
          primaryAreaUnit = "sqfeet",
          activeColor = "#3D535D",
          completedColor = "#7D4479") %>%
        addCircleMarkers(data = data_reactive(), lng = ~long, lat = ~lat,  color = "#31688e", radius = 1,
                             layerId = ~id, label = paste('Site:', data_reactive()$id),
                             popup = paste("<B>Year:",input$site_year[1], "-", input$site_year[2], "<br>",

                                           "Site:", data_reactive()$id, "(", paste(round(ves_data$lat, 3)),
                                                    ",", paste(round(ves_data$long, 3)), ")", "<br>",

                                           "Wilderness:", data_reactive()$wilderness, "<br>",

                                           data_reactive_bd()$species, "Median log(Bd) Load:", round(data_reactive()$med, 2), "<br>",

                                           data_reactive()$visual_life_stage, data_reactive()$species, "Count:", data_reactive()$sum_count, "<br>"),

                             popupOptions(closeOnClick = T)) %>%

            addPolylines(data = shape_reactive()$geometry, color = "#0d0887", dashArray = T, opacity = 0.9, weight = 1.9,
                         label = paste("Wilderness:", shape_reactive()$names),
                         popup = paste("<B>", input$site_year[1], "-", input$site_year[2], "Wilderness Totals <br>",

                                       "Wilderness:", data_reactive()$wilderness, "<br>",

                                       paste(data_reactive_bd()$visual_life_stage), paste(data_reactive_bd()$species),
                                             "Median Wilderness log(Bd) Load:", round(mean(data_reactive_bd()$bd), 2), "<br>",

                                       paste(data_reactive()$visual_life_stage ,paste(data_reactive()$species),
                                            "Count:", sum(data_reactive()$count))))

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
    
    
    
 

    
    

    
# reactive ves plot all below
    
    ves_reac <- reactive({

    
      
        ves_data %>%
            dplyr::filter(date <= input$ves_date[2] & date >= input$ves_date[1],
                          wilderness == input$wilderness_1, species == input$ves_species, id == input$id) %>% 
        group_by(visual_life_stage) %>% 
        summarise(count = sum(count)) %>% 
        mutate("Visual Life Stage" = visual_life_stage,
               Count = count)
            
    })
    
  
    
    

    
    output$ves_plots = renderPlot({
      
      if(input$wilderness_1 < 0 && input$ves_species < 0 && input$id < 0) {
        validate("Please select a wilderness, species, and site ID")
      }
      
      if(input$wilderness_1 > 0 && input$ves_species < 0 && input$id < 0){
        validate("Please select a species and site ID")
      }
      
      if(input$wilderness_1 > 0 && input$ves_species > 0 && input$id < 0){
        validate("Please select a site ID")
      }
      
 
      
      ggplot(data = ves_reac(), aes(x = visual_life_stage, y = Count, fill = visual_life_stage)) +
        geom_col() +
        theme_classic() +
        scale_x_discrete("Visual Life Stage", limits=c("Adult", "Subadult", "Tadpole", "Eggmass")) +
        scale_y_continuous(expand = c(0,0)) +
        geom_label(aes(label = paste("Count:", ves_reac()$Count)), vjust = 1.0, fill = "grey90") +
        ylab("Count") +
        scale_fill_manual(values = c("Adult" = "#35b779", "Subadult" = "#fde725", "Tadpole" = "#31688e", "Eggmass" = "#440154"),
                          name = "Visual Life Stage") +
        ggtitle(paste(input$ves_date[1], "-", input$ves_date[2], input$ves_species, "Annual Count")) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 3))
      

            
    })
    
    output$ves_counts = renderTable({
      
      ves_reac() %>% 
        select("Visual Life Stage", Count)
      
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
    
    
    
    
 # bd reactive time series plot all below   
    bd_reac <- reactive({

      
      bd_plot %>% 
        dplyr::filter((date <= input$bd_date[2] & date >= input$bd_date[1]), 
                      wilderness == input$wilderness_2, species == input$bd_species, 
                      visual_life_stage == input$stage_bd, id == input$bd_id) %>% 
        mutate("Year Month" = month_year,
               "Log(Bd) Load" = bd)
      
    })
    

    
    output$bd_plots <- renderPlot({
      
      if(input$wilderness_2 < 0 && input$bd_species < 0 && input$stage_bd < 0 && input$bd_id < 0) {
        validate("Please select a wilderness, species, life stage, and site ID")
      }
      
      
      if(input$wilderness_2 > 0 && input$bd_species < 0 && input$stage_bd < 0 && input$bd_id < 0) {
        validate("Please select a species, life stage, and site ID")
      }
      
      
      if(input$wilderness_2 > 0 && input$bd_species > 0 && input$stage_bd < 0 && input$bd_id < 0) {
        validate("Please select a life stage and site ID")
      }
      
      
      if(input$wilderness_2 > 0 && input$bd_species > 0 && input$stage_bd > 0 && input$bd_id < 0) {
        validate("Please select a site ID")
      }
      
      
      
      ggplot(data = bd_reac(), aes(x = month_year, y = bd, group = 1)) +
        geom_point() +
        geom_smooth(se = F) +
        ylab("Median log(Bd)") +
        xlab("Date") +
        ggtitle(paste(input$bd_date[1], "-", input$bd_date[2], input$bd_species, "Median Log(Bd)")) +
        geom_text_repel(aes(label = paste(round(bd_reac()$bd, 3)))) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    })
    
    output$bd_counts = renderTable({
      
      bd_reac() %>% 
        select("Year Month", "Log(Bd) Load")
      
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
    


    
    
}
    
