source(here("NPs_ShinyApp", "global.R"))


ui <- fluidPage(
    
    theme = bs_theme(bootswatch = "sandstone"),
    
    
    titlePanel(""),
    
    fluidPage(
        fluidRow(column(8,
                        h1("National Park Service Amphibian Monitoring App - RIBBiTR ")),
                 column(4,
                        img(src = "nps.png", align = "right", height = "150", width = "150" ),
                        img(src = "ribbitr.png", align = "right", height = "150", width = "180")))),
    
    
    navbarPage("",
               
               tabPanel("Home", icon = icon("info-circle"),
                        fluidPage(
                            h1("Disclaimer"),
                                fluidRow(
                                    column(12, p("This web-based application was created in collaboration with the Resilience Institute Bridging Biological Training and Research (RIBBiTR) and National Park Service (NPS). The data presented within this application contains sensitive species locations and under no circumstances shall this data or user login be shared outside these organizations. "))
                                    )
                        )),
               
               
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
                                            choices = sort(unique(data$wilderness), decreasing = T),
                                            multiple = F,
                                            selectize = T,
                                            selected = "Yosemite"),
                                
                                radioButtons(inputId = "species",
                                                   label = "Finally, select a species",
                                                   choices = unique(data$species),
                                                   selected = "ramu")),
                                
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
                                
                                selectInput(inputId = "wilderness_1",
                                           label = "Then, select a wilderness",
                                           choices = unique(data$wilderness),
                                           multiple = F,
                                           selectize = T,
                                           selected = "yosemite"),
                                
                                selectInput(inputId = "site",
                                            label = "Now, select site",
                                            choices = unique(data$id)),
                                
                                selectInput(inputId = "ves_species",
                                            label = "Finally, select a species",
                                            choices = unique(data$species),
                                            multiple = F,
                                            selected = "ramu",
                                            selectize = T)),
                        
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
                                            choices = unique(data$wilderness),
                                            multiple = T,
                                            selectize = T)),
                        
                        mainPanel(plotOutput(outputId = "bd_plots")))
                            
                        )
    
    
))
