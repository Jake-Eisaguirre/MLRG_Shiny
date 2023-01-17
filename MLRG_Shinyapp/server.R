source("global.R", local = T)
source("creds.R", local = T)

server <- function(input, output, session){
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })

########## MAP ####################
  
    #reactive ves map data frame filtering on year, wilderness, species, and life stage
    data_reactive <- reactive({
      
      
        ves_data %>%
            dplyr::filter(date <= input$site_year[2] & date >= input$site_year[1], wilderness == input$wilderness, 
                          species == input$species, visual_life_stage == input$stage) %>% 
        group_by(id, species, visual_life_stage) %>% 
        mutate(sum_count = median(count),
               med = mean(bd),
               bd = bd)
      
      
    }) %>% 
      bindCache(input$site_year, input$wilderness, input$species, input$stage)
    
    
    #reactive shape file for wilderness outlines
    shape_reactive <- reactive({

        shape %>%
            dplyr::filter(wilderness == input$wilderness)
    }) %>% bindCache(input$wilderness, cache = "app")
    
    
    # reactive for all visits
    visit_reactive <- reactive({
      
      all_visits %>% 
        filter(year <= input$site_year[2] & year >= input$site_year[1],
               wilderness == input$wilderness)
    }) %>% bindCache(input$site_year, input$wilderness, cache = "app")
    
    # reactive zoom
    view <- reactive({

      shape %>%
        filter(wilderness == input$wilderness) %>%
        st_bbox(geometry) %>%
        as.vector()
    }) %>% bindCache(input$wilderness, cache = "app")
    
    
    # bd present data set
    bd_present <- reactive({
      
      ves_data %>% 
        filter(date <= input$site_year[2] & date >= input$site_year[1],
               species == input$species, visual_life_stage == input$stage,
               wilderness == input$wilderness) %>% 
        group_by(date, species, visual_life_stage, wilderness) %>% 
        group_by(id) %>% 
        mutate(int_bd_avg = mean(bd, na.rm = T)) %>% 
        filter(int_bd_avg > 0)
        
      
    })
    
    # bd absent data set
    bd_absent <- reactive({
      
      ves_data %>% 
        filter(date <= input$site_year[2] & date >= input$site_year[1],
               species == input$species, visual_life_stage == input$stage,
               wilderness == input$wilderness) %>% 
        group_by(date, species, visual_life_stage, wilderness) %>% 
        group_by(id) %>% 
        mutate(int_bd_avg = mean(bd, na.rm = T)) %>% 
        filter(int_bd_avg == 0)
      
    })

    
    # leaflet map with date, species, and site as reactive 
    output$site_map <- renderLeaflet({
      
      #OpenTopoMap
      #Esri.WorldTopoMap
      leaflet() %>% 
        clearMarkers() %>% 
        clearControls() %>%  
        addProviderTiles("OpenTopoMap") %>% 
        addMouseCoordinates() %>% 
        setView(lng = -119.36697, lat = 37.3, zoom = 7.25) %>% 
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "feet",
          primaryAreaUnit = "sqfeet",
          activeColor = "#3D535D",
          completedColor = "#7D4479")  %>% 
        fitBounds(view()[1], view()[2], view()[3], view()[4])  %>%
        addCircleMarkers(data = data_reactive(), lng = ~long, lat = ~lat,  color = "#35b779", radius = 1, opacity = 1, 
                         fillOpacity = 1, weight = 5, layerId = data_reactive()$id,
                         label = paste('Site:',data_reactive()$id)) %>% 
                         
        addPolylines(data = shape_reactive()$geometry, color = "#0d0887", dashArray = T, opacity = 0.9, weight = 1.9,
                     label = paste("Wilderness:", shape_reactive()$wilderness), layerId = shape_reactive()$wilderness)

      })
      
   # add all visits to map
    observeEvent(input$visits, {

      leafletProxy("site_map") %>%
        clearMarkers() %>% 
        clearControls()  %>% 
        addCircleMarkers(data = visit_reactive(), lng = ~long, lat = ~lat, color = "#440154", radius = 1, opacity = 1, 
                         fillOpacity = 1, weight = 5,
                         label = paste("Site: ", all_visits$site_id, " | ",
                                            "Water type: ", all_visits$lake_type)) %>% 
        addCircleMarkers(data = data_reactive(), lng = ~long, lat = ~lat,  color = "#35b779", radius = 1, opacity = 1, 
                         fillOpacity = 1, weight = 5, layerId = data_reactive()$id,
                         label = paste('Site:',data_reactive()$id)) %>% 
        addLegend(position = c("bottomright"), title = "Species/Life stage Detected", colors = c("#35b779", "#440154"),
                                 labels = c("Detected", "Not Detected"), opacity = 1)

    })
    
    # add bd present absent to map
    observeEvent(input$bd_presence, {
      
      leafletProxy("site_map") %>%
        clearMarkers() %>% 
        clearControls()  %>% 
        addCircleMarkers(data = bd_present(), lng = ~long, lat = ~lat, color ="#de7065ff", radius = 1, opacity = 1, 
                         fillOpacity = 1, weight = 5, layerId = bd_present()$id,
                         label = paste('Site:', bd_present()$id)) %>% 
        addCircleMarkers(data = bd_absent(), lng = ~long, lat = ~lat,  color = "#6b4596ff", radius = 1, opacity = 1, 
                         fillOpacity = 1, weight = 5, layerId = bd_absent()$id,
                         label = paste('Site:',bd_absent()$id)) %>% 
        addLegend(position = c("bottomright"), title = "Bd Site Status", colors = c("#de7065ff", "#6b4596ff"),
                  labels = c("Detected", "Not Detected"), opacity = 1)
      
    })
    

   # click site to render data
    observe({
      leafletProxy("site_map")
        
        event <- input$site_map_marker_click
        
        x <- ves_data %>%
          filter(id %in% event$id)

        dat <- x %>%
          filter(date <= input$site_year[2] & date >= input$site_year[1], wilderness == input$wilderness,
                 species == input$species, visual_life_stage == input$stage) %>% 
          group_by(date, id, lat, long, wilderness, species, visual_life_stage) %>% 
          mutate(id = id,
                 date = date,
                 lat = round(lat, 2),
                 long = round(long, 2),
                 wilderness = wilderness,
                 species = species, 
                 visual_life_stage = visual_life_stage,
                 bd = round(bd, 2),
                 count = count,
                 lake_type = lake_type)
        
        message <- data.frame(Date = dat$date[dat$id == event$id],
                              Site = as.character(dat$id[dat$id == event$id]),
                              Lat = dat$lat[dat$id == event$id],
                              Long = dat$long[dat$id == event$id],
                              lake_type= dat$lake_type[dat$id == event$id],
                              count = (dat$count[dat$id == event$id]),
                              bd = (dat$bd[dat$id == event$id])) %>% 
          rename("Median Count" = count,
                 "Median Log10(Bd)" = bd,
                 "Water Type" = lake_type) 
      
        
        output$test_id <- DT::renderDataTable(message, rownames = F,  options = list(dom = 't'))

    })
    
    
    # bd tab click to render data
    observe({
      leafletProxy("site_map")
      
      event_bd <- input$site_map_marker_click
      
      b <- ves_data %>%
        filter(id %in% event_bd$id)
      
      dat_bd <- b %>%
        filter(date <= input$site_year[2] & date >= input$site_year[1], wilderness == input$wilderness,
               species == input$species, visual_life_stage == input$stage) %>% 
        group_by(date, id, lat, long, wilderness, species, visual_life_stage) %>% 
        mutate(id = id,
               date = date,
               lat = round(lat, 2),
               long = round(long, 2),
               wilderness = wilderness,
               species = species, 
               visual_life_stage = visual_life_stage,
               bd = round(bd, 2),
               count = count,
               lake_type = lake_type)
      
      message_bd <- data.frame(Date = dat_bd$date[dat_bd$id == event_bd$id],
                            Site = as.character(dat_bd$id[dat_bd$id == event_bd$id]),
                            Lat = dat_bd$lat[dat_bd$id == event_bd$id],
                            Long = dat_bd$long[dat_bd$id == event_bd$id],
                            lake_type= dat_bd$lake_type[dat_bd$id == event_bd$id],
                            count = (dat_bd$count[dat_bd$id == event_bd$id]),
                            bd = (dat_bd$bd[dat_bd$id == event_bd$id])) %>% 
        rename("Median Count" = count,
               "Median Log10(Bd)" = bd,
               "Water Type" = lake_type) 
      
      
      output$test_id <- DT::renderDataTable(message_bd, rownames = F,  options = list(dom = 't'))
      
    })
  
    
    # #click polygon to redner data
    observe({

      leafletProxy("site_map")

      event_poly <- input$site_map_shape_click

      p <- ves_data %>%
        filter(wilderness %in% event_poly$id)

      p_dat <- p %>%
        dplyr::filter(date <= input$site_year[2] & date >= input$site_year[1], wilderness == input$wilderness,
                      species == input$species, visual_life_stage == input$stage) %>%
        group_by(species, visual_life_stage, wilderness, date) %>%
        summarise(sum_count = sum(count),
                  av_bd = round(mean(bd, na.rm = T), 2))


      p_message <- data.frame(
                            Jurisdiction = as.character(p_dat$wilderness[p_dat$wilderness == event_poly$id]),
                            Year = as.character(p_dat$date[p_dat$wilderness == event_poly$id]),
                            count = (p_dat$sum_count[p_dat$wilderness == event_poly$id]),
                            bd = (p_dat$av_bd[p_dat$wilderness == event_poly$id])) %>% 
        rename("Jurisdiction Median Count" = count,
               "Jurisdiction Bd Load" = bd) 

      output$test_id <- DT::renderDataTable(p_message, rownames = F,  options = list(dom = 't', pageLength = 25))
       
     })
     
    

    
    # reset action buttons clicks for bd and visits
    observeEvent(input$visits, {
      updateCheckboxGroupButtons(session, "visits", selected = "")
    })
    
    observeEvent(input$bd_presence, {
      updateCheckboxGroupButtons(session, "bd_presence", selected = "")
    })
    
  
################ END MAP #######################
    
############## VES ########################
    
# reactive ves data for adult, subadult and eggs
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
    
    
    # reactive ves data for tadpoles
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
  
    # render data for table
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
    

    # render plot for adult, sub adult, and egg
    output$ves_plots = renderPlot({
      
      
      
      if(input$wilderness_1 < 0 && input$ves_species < 0 && input$id < 0) {
        validate("Please select a jurisdiction, species, site ID, and life stage")
      }
      
      if(input$wilderness_1 > 0 && input$ves_species < 0 && input$id < 0){
        validate("Please select a species, site ID, and life stage")
      }
      
      if(input$wilderness_1 > 0 && input$ves_species > 0 && input$id < 0){
        validate("Please select a site ID and life stage")
      }
 
      
      ggplot(data = ves_reac(), aes(x = date, y = count, color = visual_life_stage)) +
        geom_point(size = 3) +
        geom_line(size = 1.2) +
        theme_classic() +
        ylab("Median Count") +
        xlab("Year") +
        ggtitle(paste(input$ves_date[1], "-", input$ves_date[2], input$ves_species, "Annual Median Count")) +
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
    
    
    # render plot for tadpole
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
      
    # agg table render
    output$ves_counts = renderTable({
      
      ves_table() %>% 
        arrange(visual_life_stage) %>% 
        dplyr::select(Year, "Visual Life Stage", Count) %>% 
        rename("Median Count" = Count)
      
      })
    
    # agg table for download
    ves_agg_reac <- reactive({

      ves_table() %>%
        arrange(visual_life_stage) %>%
        dplyr::select(Year, "Visual Life Stage", Count) %>%
        rename("Median Count" = Count) %>% 
        mutate(Jurisdiction = input$wilderness_1,
               Species = input$ves_species,
               Site = input$id)

    })
    
        # Data download
        observeEvent(input$ves_agg_download, {

          shinyalert(title = "Pump the breaks!",
                     text = "This Feature will be live following data sharing contract",
                     type = "warning", closeOnClickOutside = T, showCancelButton = T, inputId = "ves_agg_download_btn",
                     showConfirmButton = T, confirmButtonText = "Yes", cancelButtonText = "No",
                     animation = "slide-from-top")
        })

        observeEvent(input$ves_agg_download_btn,{
          if(input$ves_agg_download_btn == T)
            showModal(modalDialog(downloadButton("ves_agg_dwnld", "Download"), footer = NULL, easyClose = T, size = "s"))
        })

        output$ves_agg_dwnld <- downloadHandler(
          filename = function(){"insert_name.csv"},

          content = function(file) {
            shiny::withProgress(
              message = paste0("Downloading Aggregated VES Data"),
              value = 0,
              {
                shiny::incProgress(3/10)
                Sys.sleep(1)
                shiny::incProgress(9/10)
                write.csv(ves_agg_reac(), file, row.names = FALSE)
              }
            )
          }
        )
    
    
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
    
####################### END VES ########################
    
#################### Bd ######################
    
    
 # bd reactive time series data
    
    bd_reac <- reactive({
      
      
      bd_plot %>% 
        dplyr::filter((date <= input$bd_date[2] & date >= input$bd_date[1]), 
                      wilderness == input$wilderness_2, 
                      species == input$bd_species, 
                      id == input$bd_id) %>% 
        mutate("Year-Month" = month_year,
               "Log10(Bd) Load" = bd,
               "Visual Life Stage" = visual_life_stage,
               Prevalence = round(Prevalence, 2),
               bd = round(bd, 2),
               "Sample Size" = as.character(sample_size))
      
    })
    
    # adult data
    bd_reac_adult <- reactive({

      
      bd_plot %>% 
        dplyr::filter((date <= input$bd_date[2] & date >= input$bd_date[1]), 
                      wilderness == input$wilderness_2, 
                      species == input$bd_species, 
                      visual_life_stage == "Adult", 
                      id == input$bd_id) %>% 
        mutate("Year-Month" = month_year,
               "Log(Bd) Load" = bd,
               "Visual Life Stage" = visual_life_stage,
               Prevalence = round(Prevalence, 2),
               bd = round(bd, 2)) %>% 
        ungroup()
      
    })
    
    # tadpole data
    bd_reac_tad <- reactive({
      
      
      bd_plot %>% 
        dplyr::filter((date <= input$bd_date[2] & date >= input$bd_date[1]), 
                      wilderness == input$wilderness_2, 
                      species == input$bd_species, 
                      visual_life_stage == "Tadpole", 
                      id == input$bd_id) %>% 
        mutate("Year-Month" = month_year,
               "Log(Bd) Load" = bd,
               "Visual Life Stage" = visual_life_stage,
               Prevalence = round(Prevalence, 2),
               bd = round(bd, 2)) %>% 
        ungroup()
      
    })
    
    #sub adult data
    bd_reac_subadult <- reactive({
      
      
      bd_plot %>% 
        dplyr::filter((date <= input$bd_date[2] & date >= input$bd_date[1]), 
                      wilderness == input$wilderness_2, 
                      species == input$bd_species, 
                      visual_life_stage == "Subadult", 
                      id == input$bd_id) %>% 
        mutate("Year-Month" = month_year,
               "Log(Bd) Load" = bd,
               "Visual Life Stage" = visual_life_stage,
               Prevalence = round(Prevalence, 2),
               bd = round(bd, 2))
      
    })

    # render bd plots
    output$bd_plots <- renderPlot({
      
      if(input$wilderness_2 < 0 && input$bd_species < 0 && input$bd_id < 0) {
        validate("Please select a jurisdiction, species, and site ID")
      }
      
      
      if(input$wilderness_2 > 0 && input$bd_species < 0 && input$bd_id < 0) {
        validate("Please select a species, and site ID")
      }
      
      
      if(input$wilderness_2 > 0 && input$bd_species > 0 && input$bd_id < 0) {
        validate("Please select a site ID")
      }
      
      
      
      ggplot(data = bd_reac_adult(), aes(x = month_year, y = bd, fill = visual_life_stage, group = visual_life_stage)) +
        geom_point(size = 2.75, color = "#35b779") +
        geom_smooth(se = F, show.legend = F) +
        ylab("Median log10(Bd)") +
        xlab("Year-Month") +
        labs(fill = "Visual Life Stage") + 
        ggtitle(paste(input$bd_date[1], "-", input$bd_date[2], input$bd_species, "Median Log10(Bd)")) +
        geom_text_repel(aes(label = paste(round(bd_reac_adult()$sample_size, 3)))) +
        theme_classic() +
        theme(#plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.9, size = 12),
              plot.title = element_text(hjust = 0.5, vjust = 1.2, size = 17),
              axis.line = element_line(size = 1.1),
              axis.ticks = element_line(size = 1.8),
              axis.text.y = element_text(size = 14),
              axis.title.x = element_text(size = 14, vjust = 1),
              axis.title.y = element_text(size = 15),
              legend.key.height = unit(2, "cm"),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 15),
              legend.key.size = unit(2, "cm")) +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_fill_manual(values = c("Adult" = "#35b779"),
                           name = "Visual Life Stage") +
        geom_hline(yintercept=5.8, linetype='dotted', col = 'red') 
    
    })
    
    output$bd_plots_sub <- renderPlot({
      
      
      if(input$wilderness_2 < 0 && input$bd_species < 0 && input$bd_id < 0) {
        validate(" ")
      }


      if(input$wilderness_2 > 0 && input$bd_species < 0 && input$bd_id < 0) {
        validate(" ")
      }


      if(input$wilderness_2 > 0 && input$bd_species > 0 && input$bd_id < 0) {
        validate(" ")
      }

      validate(
        need(nrow(bd_reac_subadult() > 0), 'No data exists')
      )
      
      ggplot(data = bd_reac_subadult(), aes(x = month_year, y = bd, fill = visual_life_stage, group = visual_life_stage)) +
        geom_point(size = 2.5, color = "#fde725") +
        geom_smooth(se = F, show.legend = F) +
        ylab("Median log10(Bd)") +
        xlab("Year-Month") +
        labs(fill = "Visual Life Stage") + 
        geom_text_repel(aes(label = paste(round(bd_reac_subadult()$sample_size, 3)))) +
        theme_classic() +
        theme(#plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.9, size = 12),
          plot.title = element_text(hjust = 0.5, vjust = 1.2, size = 17),
          axis.line = element_line(size = 1.1),
          axis.ticks = element_line(size = 1.8),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, vjust = 1),
          axis.title.y = element_text(size = 15),
          legend.key.height = unit(2, "cm"),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.key.size = unit(2, "cm")) +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_fill_manual(values = c("Subadult" = "#fde725"),
                           name = "Visual Life Stage") +
        geom_hline(yintercept=5.8, linetype='dotted', col = 'red')
      
    })
    
    output$bd_plots_tad <- renderPlot({
      
      if(input$wilderness_2 < 0 && input$bd_species < 0 && input$bd_id < 0) {
        validate(" ")
      }
      
      
      if(input$wilderness_2 > 0 && input$bd_species < 0 && input$bd_id < 0) {
        validate(" ")
      }
      
      
      if(input$wilderness_2 > 0 && input$bd_species > 0 && input$bd_id < 0) {
        validate(" ")
      }
      
      
      
      ggplot(data = bd_reac_tad(), aes(x = month_year, y = bd, fill = visual_life_stage, group = visual_life_stage)) +
        geom_point(color = "#31688e", size = 2.5) +
        geom_smooth(se = F, show.legend = F) +
        ylab("Median log10(Bd)") +
        xlab("Year-Month") +
        labs(fill = "Visual Life Stage") + 
        geom_text_repel(aes(label = paste(round(bd_reac_tad()$sample_size, 3)))) +
        theme_classic() +
        theme(#plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.9, size = 12),
          plot.title = element_text(hjust = 0.5, vjust = 1.2, size = 17),
          axis.line = element_line(size = 1.1),
          axis.ticks = element_line(size = 1.8),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, vjust = 1),
          axis.title.y = element_text(size = 15),
          legend.key.height = unit(2, "cm"),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.key.size = unit(2, "cm")) +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_fill_manual(values = c("Tadpole" = "#31688e"),
                           name = "Visual Life Stage") +
        geom_hline(yintercept=5.8, linetype='dotted', col = 'red')
      
    })
    
    # render table
    output$bd_counts = renderTable({
      
      bd_reac() %>% 
        arrange(visual_life_stage) %>% 
        dplyr::select("Year-Month", "Visual Life Stage", "Log10(Bd) Load", "Prevalence", "Sample Size")
      
    })
    
    # agg table for download
    bd_agg_reac <- reactive({
      
      bd_reac() %>% 
        arrange(visual_life_stage) %>% 
        dplyr::select("Year-Month", "Visual Life Stage", "Log10(Bd) Load", "Prevalence", "Sample Size") %>% 
        mutate(Jurisdiction = input$wilderness_2, Species = input$bd_species, Site = input$bd_id)
      
    })
    
    # Data download
    observeEvent(input$bd_agg_download, {
      
      shinyalert(title = "Pump the breaks!",
                 text = "This Feature will be live following data sharing contract",
                 type = "warning", closeOnClickOutside = T, showCancelButton = T, inputId = "bd_agg_download_btn",
                 showConfirmButton = T, confirmButtonText = "Yes", cancelButtonText = "No",
                 animation = "slide-from-top")
    })
    
    observeEvent(input$bd_agg_download_btn,{
      if(input$bd_agg_download_btn == T)
        showModal(modalDialog(downloadButton("bd_agg_dwnld", "Download"), footer = NULL, easyClose = T, size = "s"))
    })
    
    output$bd_agg_dwnld <- downloadHandler(
      filename = function(){"insert_name.csv"},
      
      content = function(file) {
        shiny::withProgress(
          message = paste0("Downloading Aggregated Bd Data"),
          value = 0,
          {
            shiny::incProgress(3/10)
            Sys.sleep(1)
            shiny::incProgress(9/10)
            write.csv(bd_agg_reac(), file, row.names = FALSE)
          }
        )
      }
    )
    
    
    # update input options
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
    
############## END Bd #######################
    
    
##### capture data download ######
    
    
    cap_data <- reactive({

      full_capture %>%
        filter(year <= input$data_year[2] & year>= input$data_year[1],
               jurisdiction %in% input$data_jur) %>%
        dplyr::select(wilderness, input$data_site, input$data_visit, input$data_survey, input$data_capture, input$data_bd)

    })

    output$cap_table <- DT::renderDataTable(cap_data(), rownames = FALSE, extensions= 'Buttons',
                                            options = list(scrollX = T, TRUEom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))


    # Data download
    observeEvent(input$cap_download, {

      shinyalert(title = "Pump the breaks!",
                 text = "This Feature will be live following data sharing contract",
                 type = "warning", closeOnClickOutside = T, showCancelButton = T, inputId = "cap_download_btn",
                 showConfirmButton = T, confirmButtonText = "Yes", cancelButtonText = "No",
                 animation = "slide-from-top")
    })

    # observeEvent(input$cap_download_btn,{
    #   if(input$cap_download_btn == T)
    #     showModal(modalDialog(downloadButton("cap_dwnld", "Download"), footer = NULL, easyClose = T, size = "s"))
    # })
    #
    # output$cap_dwnld <- downloadHandler(
    #   filename = function(){"insert_name.csv"},
    #
    #   content = function(file) {
    #     shiny::withProgress(
    #       message = paste0("Downloading Capture Data"),
    #       value = 0,
    #       {
    #         shiny::incProgress(3/10)
    #         Sys.sleep(1)
    #         shiny::incProgress(9/10)
    #         write.csv(cap_data(), file, row.names = FALSE)
    #       }
    #     )
    #   }
    # )


    # update wilderness options based on year selection
    observe(
      {input$data_year

        updatePickerInput(session, inputId = "data_jur",
                          choices = unique(full_capture$wilderness[full_capture$year <= input$data_year[2]
                                                        & full_capture$year>=input$data_year[1]]))
      })

    # clear button
    observeEvent(input$cap_clear,
                 {
                   updatePickerInput(session, inputId = "data_year", selected = c(max(visit$year) - 5, max(visit$year)))
                   updatePickerInput(session, inputId = "data_jur", selected = "")
                   updatePickerInput(session, inputId = "data_site", selected = "")
                   updatePickerInput(session, inputId = "data_visit", selected = "")
                   updatePickerInput(session, inputId = "data_survey", selected = "")
                   updatePickerInput(session, inputId = "data_capture", selected = "")
                   updatePickerInput(session, inputId = "data_bd", selected = "")

                 })


###### end capture download #########

####### VES Data download ##########

    v_data <- reactive({

      full_ves %>%
        filter(year <= input$data_year_ves[2] & year >= input$data_year_ves[1],
               jurisdiction %in% input$data_jur_ves) %>%
        dplyr::select(wilderness, input$data_site_ves, input$data_visit_ves, input$data_survey_ves, input$data_ves)

    })

    output$ves_table <- DT::renderDataTable(v_data(), rownames = FALSE, extensions= 'Buttons',
                                            options = list(scrollX = T, TRUEom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

    # VES Data download
    observeEvent(input$ves_download, {

      shinyalert(title = "Pump the breaks!",
                 text = "This Feature will be live following data sharing contract",
                 type = "warning", closeOnClickOutside = T, showCancelButton = T, inputId = "ves_download_btn",
                 showConfirmButton = T, confirmButtonText = "Yes", cancelButtonText = "No",
                 animation = "slide-from-top")
    })

    # observeEvent(input$ves_download_btn,{
    #   if(input$ves_download_btn == T)
    #     showModal(modalDialog(downloadButton("ves_dwnld", "Download"), footer = NULL, easyClose = T, size = "s"))
    # })
    #
    # output$ves_dwnld <- downloadHandler(
    #   filename = function(){"insert_name.csv"},
    #
    #   content = function(file) {
    #     shiny::withProgress(
    #       message = paste0("Downloading VES Data"),
    #       value = 0,
    #       {
    #         shiny::incProgress(3/10)
    #         Sys.sleep(1)
    #         shiny::incProgress(9/10)
    #         write.csv(v_data(), file, row.names = FALSE)
    #       }
    #     )
    #   }
    # )
    #

    # update wilderness options based on year selection
    observe(
      {input$data_year_ves

        updatePickerInput(session, inputId = "data_jur_ves",
                          choices = unique(full_ves$wilderness[full_ves$year <= input$data_year_ves[2]
                                                                   & full_ves$year>=input$data_year_ves[1]]))
      })


    # clear button
    observeEvent(input$ves_clear,
                 {
                   updatePickerInput(session, inputId = "data_year_ves", selected = c(max(visit$year) - 5, max(visit$year)))
                   updatePickerInput(session, inputId = "data_jur_ves", selected = "")
                   updatePickerInput(session, inputId = "data_site_ves", selected = "")
                   updatePickerInput(session, inputId = "data_visit_ves", selected = "")
                   updatePickerInput(session, inputId = "data_survey_ves", selected = "")
                   updatePickerInput(session, inputId = "data_ves", selected = "")

                 })

################# END VES data download ###################    
    
    
################# Relocate data #########################
    
    # reactive relocate data
    reloc_data <- reactive({
      
      full_cmr %>% 
        filter(year %in% c(input$data_year_cmr), collect_siteid %in% c(input$id_cmr),
               type %in% c(input$retran_cmr)) %>% 
        dplyr::select(input$relocate_cmr, input$relocate_frog_cmr, collect_date, collect_siteid, type)
      
    })
    
    # render data
    output$cmr_table <- DT::renderDataTable(reloc_data(), rownames = FALSE, extensions= 'Buttons',
                                            options = list(scrollX = T, TRUEom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    
    #download data
    # VES Data download
    observeEvent(input$cmr_download, {
      
      shinyalert(title = "Pump the breaks!",
                 text = "This Feature will be live following data sharing contract",
                 type = "warning", closeOnClickOutside = T, showCancelButton = T, inputId = "cmr_download_btn",
                 showConfirmButton = T, confirmButtonText = "Yes", cancelButtonText = "No",
                 animation = "slide-from-top")
    })
    
    # observeEvent(input$cmr_download_btn,{
    #   if(input$cmr_download_btn == T)
    #     showModal(modalDialog(downloadButton("cmr_dwnld", "Download"), footer = NULL, easyClose = T, size = "s"))
    # })
    # 
    # output$cmr_dwnld <- downloadHandler(
    #   filename = function(){"insert_name.csv"},
    # 
    #   content = function(file) {
    #     shiny::withProgress(
    #       message = paste0("Downloading Relocate Data"),
    #       value = 0,
    #       {
    #         shiny::incProgress(3/10)
    #         Sys.sleep(1)
    #         shiny::incProgress(9/10)
    #         write.csv(reloc_data(), file, row.names = FALSE)
    #       }
    #     )
    #   }
    # )
    
    
    # clear button
    observeEvent(input$cmr_clear,
                 {
                   updatePickerInput(session, inputId = "data_year_cmr", selected = "")
                   updatePickerInput(session, inputId = "id_cmr", selected = "")
                   updatePickerInput(session, inputId = "retran_cmr", selected = "")
                   updatePickerInput(session, inputId = "relocate_cmr", selected = "")
                   updatePickerInput(session, inputId = "relocate_frog_cmr", selected = "")
                   
                 })
    
    
    # update site_id options based on year selection
    observe(
      {input$data_year_cmr
        
        updatePickerInput(session, inputId = "id_cmr",
                          choices = unique(full_cmr$collect_siteid[full_cmr$year == c(input$data_year_cmr)]))
      })
    

}
    
