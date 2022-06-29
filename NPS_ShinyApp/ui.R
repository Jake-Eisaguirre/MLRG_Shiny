source(here("NPs_ShinyApp", "global.R"))


ui <- fluidPage(
    
    
    
    
    titlePanel("NPS Amphib Data"),
    
    
    navbarPage("",
               
               tabPanel("Home", icon = icon("info-circle")),
               
               
               tabPanel(title = "Site Map", icon = icon("globe-asia"),
                        
                        sidebarLayout(
                                
                            sidebarPanel(
                                
                                selectInput(inputId = "site_year",
                                            label = "First, select a Year",
                                            choices = sort(unique(data$date), decreasing = T),
                                            multiple = F,
                                            selectize = T),
                                
                                selectInput(inputId = "wilderness",
                                            label = "Now, select a wilderness",
                                            choices = unique(ves$wilderness),
                                            multiple = F,
                                            selectize = T,
                                            selected = "yosemite"),
                                
                                checkboxGroupButtons(inputId = "clear_1",
                                                     label = "Remove Selection",
                                                     choices = c("Clear"))),
                                
                    mainPanel(leafletOutput(outputId = "site_map", width = 900, height = 500)))
                    
                    ),
               
               
               tabPanel(title = "VES", icon = icon("frog"),
                        
                        sidebarLayout(
                        
                            sidebarPanel(
                            
                                selectInput(inputId = "ves_date",
                                      label = "First, select a Year",
                                      choices = sort(ui_date_ves, decreasing = T),
                                      multiple = F,
                                      selectize = T),
                                
                                selectInput(inputId = "wilderness",
                                           label = "Now, select a wilderness",
                                           choices = unique(ves$wilderness),
                                           multiple = T,
                                           selectize = T),
                                
                                selectInput(inputId = "live_stage",
                                            label = "Finally, select life stage",
                                            choices = unique(ves$visual_life_stage))),
                        
                        mainPanel(plotOutput(outputId = "ves_plots")))
                            
                        ),
               
               
               tabPanel(title = "Bd Load", icon = icon("bacterium"),
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                selectInput(inputId = "bd_date",
                                            label = "First, select a Year",
                                            choices = sort(ui_date_bd, decreasing = T),
                                            multiple = F,
                                            selectize = T),
                                
                                selectInput(inputId = "wilderness",
                                            label = "Now, select a wilderness",
                                            choices = unique(bd$wilderness),
                                            multiple = T,
                                            selectize = T)),
                        
                        mainPanel(plotOutput(outputId = "bd_plots")))
                            
                        )
    
    
))
