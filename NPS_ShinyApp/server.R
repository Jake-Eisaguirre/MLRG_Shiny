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
    
    
    #reactive shape file for wilderness outlines
    
    shape_reactive <- reactive({

        shape %>%
            dplyr::filter(wilderness == input$wilderness)
    })
    
    # reactive for all visits
    
    visit_reactive <- reactive({
      
      all_visits %>% 
        filter(year <= input$site_year[2] & year >= input$site_year[1],
               wilderness == input$wilderness)
    })
    
  
    
    view <- reactive({

      shape %>%
        filter(wilderness == input$wilderness) %>%
        st_bbox(geometry) %>%
        as.vector()
    })

    
    # leaflet map with date, species, and site as reactive 
    output$site_map <- renderLeaflet({
      
      #OpenTopoMap
      #Esri.WorldTopoMap
      leaflet() %>% 
        addProviderTiles("OpenTopoMap") %>% 
        addMouseCoordinates() %>% 
        setView(lng = -119.36697, lat = 37.3, zoom = 7.25) %>% 
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "feet",
          primaryAreaUnit = "sqfeet",
          activeColor = "#3D535D",
          completedColor = "#7D4479") 
        
     })

    
    observeEvent(c(input$site_year, input$wilderness), {

      leafletProxy("site_map") %>% 
        clearMarkers() %>% 
        clearShapes() %>% 
        clearControls() %>% 
        fitBounds(view()[1], view()[2], view()[3], view()[4])  %>%
        
        addPolylines(data = shape_reactive()$geometry, color = "#0d0887", dashArray = T, opacity = 0.9, weight = 1.9) 

    })
    
    observeEvent(c(input$species, input$stage), { 
      
      leafletProxy("site_map") %>%
        clearMarkers() %>% 
        fitBounds(view()[1], view()[2], view()[3], view()[4])  %>%
        addCircleMarkers(data = data_reactive(), lng = ~long, lat = ~lat,  color = "#35b779", radius = 1, opacity = 0.5, 
                         fillOpacity = 0.05, weight = 5,
                         label = paste('Site:', data_reactive()$id),
                         popup = paste("<B>Year:",input$site_year[1], "-", input$site_year[2], "<br>",
                                       
                                       "Site:", data_reactive()$id, "(", paste(round(ves_data$lat, 3)),
                                       ",", paste(round(ves_data$long, 3)), ")", "<br>",
                                       
                                       "Water Type:", data_reactive()$lake_type, "<br>",
                                       
                                       data_reactive()$species, "Median log(Bd) Load:", round(data_reactive()$med, 2), "<br>",
                                       
                                       data_reactive()$visual_life_stage, data_reactive()$species, "Count:", data_reactive()$sum_count, "<br>"),
                         
                         popupOptions(closeOnClick = T)) %>% 
        addPolylines(data = shape_reactive()$geometry, color = "#0d0887", dashArray = T, opacity = 0.9, weight = 1.9,
                     label = paste("Wilderness:", shape_reactive()$wilderness),
                     popup = paste("<B>", input$site_year[1], "-", input$site_year[2], "Wilderness Totals <br>",
                                   
                                   "Wilderness:", shape_reactive()$wilderness, "<br>",
                                   
                                   paste(data_reactive()$visual_life_stage), paste(data_reactive()$species),
                                   "Median Wilderness log(Bd) Load:", round(data_reactive()$bd, 2), "<br>",
                                   
                                   paste(data_reactive()$visual_life_stage, paste(data_reactive()$species),
                                         "Count:", sum(data_reactive()$count))))
      
    })
    
    observeEvent(input$visits, {
      
      leafletProxy("site_map") %>% 
        #clearMarkers() %>% 
        addCircleMarkers(data = visit_reactive(), lng = ~long, lat = ~lat, color = "#440154", radius = 1,
                         label = paste('Site:', visit_reactive()$site_id),
                         popup = paste("<B>Year:",input$site_year[1], "-", input$site_year[2], "<br>",
                                               
                                               "Site:", data_reactive()$id, "(", paste(round(ves_data$lat, 3)),
                                               ",", paste(round(ves_data$long, 3)), ")", "<br>",
                                               
                                               "Water Type:", data_reactive()$lake_type, "<br>")) %>% 
        
        addCircleMarkers(data = data_reactive(), lng = ~long, lat = ~lat,  color = "#35b779", radius = 1, opacity = 0.5, 
                         fillOpacity = 0.05, weight = 5,
                         label = paste('Site:', data_reactive()$id),
                         popup = paste("<B>Year:",input$site_year[1], "-", input$site_year[2], "<br>",
                                       
                                       "Site:", data_reactive()$id, "(", paste(round(ves_data$lat, 3)),
                                       ",", paste(round(ves_data$long, 3)), ")", "<br>",
                                       
                                       "Water Type:", data_reactive()$lake_type, "<br>",
                                       
                                       data_reactive()$species, "Median log(Bd) Load:", round(data_reactive()$med, 2), "<br>",
                                       
                                       data_reactive()$visual_life_stage, data_reactive()$species, "Count:", data_reactive()$sum_count, "<br>"),
                         
                         popupOptions(closeOnClick = T)) %>% 
        addLegend(position = c("bottomright"), title = "Organism Encounters", colors = c("#35b779", "#440154"), 
                  labels = c("Encounters", "No Encounters"))
      
      })
    
    
    observeEvent(input$clear, {
      
      leafletProxy("site_map") %>% 
        fitBounds(view()[1], view()[2], view()[3], view()[4])
      
      updatePickerInput(session, "site_year", selected = "2021")
      updatePickerInput(session, "wilderness", selected = "")
      updatePickerInput(session, "species", selected = "")
      updatePickerInput(session, "stage", selected = "")
      updateCheckboxGroupButtons(session, "visits", selected = "")
      updateCheckboxGroupButtons(session, "clear", selected = "")
      

      
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
                   filter(!visual_life_stage == "Tadpole") %>% 
            dplyr::filter(date <= input$ves_date[2] & date >= input$ves_date[1],
                          wilderness == input$wilderness_1, 
                          species == input$ves_species, 
                          id == input$id,
                          visual_life_stage %in% input$vls) %>% 
        mutate("Visual Life Stage" = visual_life_stage,
               Count = as.character(count),
               Year = as.character(date))
            
    })
    
    ves_tad <- reactive({
      
      
      
      ves_data %>%
        dplyr::filter(date <= input$ves_date[2] & date >= input$ves_date[1],
                      wilderness == input$wilderness_1, 
                      species == input$ves_species, 
                      id == input$id,
                      visual_life_stage == "Tadpole") %>% 
        mutate("Visual Life Stage" = visual_life_stage,
               Count = as.character(count),
               Year = as.character(date))
      
    })
  
    
    ves_table <- reactive({
      
      
      
      ves_data %>%
        dplyr::filter(date <= input$ves_date[2] & date >= input$ves_date[1],
                      wilderness == input$wilderness_1, 
                      species == input$ves_species, 
                      id == input$id,
                      visual_life_stage %in% input$vls) %>% 
        mutate("Visual Life Stage" = visual_life_stage,
               Count = as.character(count),
               Year = as.character(date))
      
    })  
    

    
    output$ves_plots = renderPlot({
      
      if(input$wilderness_1 < 0 && input$ves_species < 0 && input$id < 0) {
        validate("Please select a wilderness, species, site ID, and life stage")
      }
      
      if(input$wilderness_1 > 0 && input$ves_species < 0 && input$id < 0){
        validate("Please select a species, site ID, and life stage")
      }
      
      if(input$wilderness_1 > 0 && input$ves_species > 0 && input$id < 0){
        validate("Please select a site ID and life stage")
      }
 
      
      ggplot(data = ves_reac(), aes(x = date, y = count, color = visual_life_stage)) +
        geom_point(size = 2.5) +
        geom_line(size = 1.2) +
        theme_classic() +
        ylab("Median Count") +
        xlab("Year") +
        ggtitle(paste(input$ves_date[1], "-", input$ves_date[2], input$ves_species, "Annual Count")) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 17),
              axis.line = element_line(size = 1.1),
              axis.ticks = element_line(size = 1.8),
              axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              legend.key.height = unit(2, "cm"),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 15),
              legend.key.size = unit(2, "cm")) +
        scale_color_manual(values = c("Adult" = "#35b779", "Subadult" = "#fde725", "Eggmass" = "#440154"),
                          name = "Visual Life Stage") +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(breaks = integer_breaks())
            
    })
    
    output$ves_tad = renderPlot({
      
      if(input$wilderness_1 < 0 && input$ves_species < 0 && input$id < 0) {
        validate(" ")
      }
      
      if(input$wilderness_1 > 0 && input$ves_species < 0 && input$id < 0){
        validate(" ")
      }
      
      if(input$wilderness_1 > 0 && input$ves_species > 0 && input$id < 0){
        validate(" ")
      }
      
      
      ggplot(data = ves_tad(), aes(x = date, y = count, color = visual_life_stage)) +
        geom_point(size = 2.5) +
        geom_line(size = 1.2) +
        theme_classic() +
        ylab("Median Count") +
        xlab("Year") +
        theme(plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 17),
              axis.line = element_line(size = 1.1),
              axis.ticks = element_line(size = 1.8),
              axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              legend.key.height = unit(2, "cm"),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 15),
              legend.key.size = unit(2, "cm")) +
        scale_color_manual(values = c("Tadpole" = "#31688e"),
                           name = "Visual Life Stage") +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(breaks = integer_breaks())
      
    })
      
    
    output$ves_counts = renderTable({
      
      ves_table() %>% 
        arrange(visual_life_stage) %>% 
        select(Year, "Visual Life Stage", Count) %>% 
        rename("Median Count" = Count)
      
      })
    
    
    # observe events to update wilderness and years based on selection for leaflet map
    observeEvent(input$ves_date, ignoreNULL = T,{
      
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
    
    observeEvent(input$id, {
      
      updateCheckboxGroupInput(session, inputId = "vls", 
                        choices = unique(ves_data$visual_life_stage[ves_data$date <= input$ves_date[2] 
                                                     & ves_data$date >= input$ves_date[1] 
                                                     & ves_data$wilderness == input$wilderness_1 
                                                     & ves_data$species == input$ves_species
                                                     & ves_data$id == input$id]),
                        selected = unique(ves_data$visual_life_stage))
    })
    
    
    
    
 # bd reactive time series plot all below   
    bd_reac <- reactive({

      
      bd_plot %>% 
        dplyr::filter((date <= input$bd_date[2] & date >= input$bd_date[1]), 
                      wilderness == input$wilderness_2, 
                      species == input$bd_species, 
                      visual_life_stage %in% input$stage_bd, 
                      id == input$bd_id) %>% 
        mutate("Year-Month" = month_year,
               "Log(Bd) Load" = bd,
               "Visual Life Stage" = visual_life_stage,
               Prevalence = round(Prevalence, 2),
               bd = round(bd, 2))
      
    })
    

    
    output$bd_plots <- renderPlot({
      
      if(input$wilderness_2 < 0 && input$bd_species < 0 && input$bd_id < 0) {
        validate("Please select a wilderness, species, site ID, and life stage")
      }
      
      
      if(input$wilderness_2 > 0 && input$bd_species < 0 && input$bd_id < 0) {
        validate("Please select a species, site ID, and life stage")
      }
      
      
      if(input$wilderness_2 > 0 && input$bd_species > 0 && input$bd_id < 0) {
        validate("Please select a site ID and life stage")
      }
      
      
      
      ggplot(data = bd_reac(), aes(x = month_year, y = bd, group = 1)) +
        geom_point(aes(color = visual_life_stage), size = 2.5) +
        geom_smooth(se = F) +
        ylab("Median log(Bd)") +
        xlab("Year-Month") +
        ggtitle(paste(input$bd_date[1], "-", input$bd_date[2], input$bd_species, "Median Log(Bd)")) +
        geom_text_repel(aes(label = paste(round(bd_reac()$bd, 3)))) +
        theme_classic() +
        theme(#plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.9, size = 12),
              plot.title = element_text(hjust = 0.5, vjust = 1.2, size = 17),
              axis.line = element_line(size = 1.1),
              axis.ticks = element_line(size = 1.8),
              axis.text.y = element_text(size = 14),
              axis.title.x = element_text(size = 14, vjust = 3),
              axis.title.y = element_text(size = 15),
              legend.key.height = unit(2, "cm"),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 15),
              legend.key.size = unit(2, "cm")) +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_color_manual(values = c("Adult" = "#35b779", "Subadult" = "#fde725", "Tadpole" = "#31688e", "Eggmass" = "#440154"),
                           name = "Visual Life Stage") +
        geom_hline(yintercept=5.8, linetype='dotted', col = 'red') 
    
    })
    
    output$bd_counts = renderTable({
      
      bd_reac() %>% 
        arrange(visual_life_stage) %>% 
        select("Year-Month", "Visual Life Stage", "Log(Bd) Load", "Prevalence")
      
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

      updatePickerInput(session, inputId = "bd_id",
                        choices = unique(bd_data$id[bd_data$date <= input$bd_date[2]
                                                     & bd_data$date >= input$bd_date[1]
                                                     & bd_data$wilderness == input$wilderness_2
                                                     & bd_data$species == input$bd_species]))
    })
    
    observeEvent(input$bd_id, {

      updateCheckboxGroupInput(session, inputId = "stage_bd",
                        choices = unique(bd_data$visual_life_stage[bd_data$date <= input$bd_date[2]
                                                    & bd_data$date >= input$bd_date[1]
                                                    & bd_data$wilderness == input$wilderness_2
                                                    & bd_data$species == input$bd_species
                                                    & bd_data$id == input$bd_id]),
                        selected = unique(ves_data$visual_life_stage))

    })
    
    
    
}
    
