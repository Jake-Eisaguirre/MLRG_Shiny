source(here("NPs_ShinyApp", "global.R"))




ui <- fluidPage(
    
    #includeCSS(here("NPS_ShinyApp/theme.css")),
    
    theme = bs_theme(bootswatch = "sandstone",
                     bg = "#506e57", # green background
                     fg = "#f0f2f0", # cream text foreground
                     primary = "#a9a9a9", # hyperlinks
                     secondary = "#506e57", # messages
                     success = "#506e57", # operation succeeded
                     info = "#506e57", # light blue from bar
                     warning = "#C3512C",# yellow
                     danger = "#FACE00", # orange red
                     base_font = font_google("Open Sans"), 
                     heading_font = font_google("Source Sans Pro")),
    
    
    titlePanel(""),
    
    fluidPage(
        fluidRow(column(8,
                        h1(strong("National Park Service Amphibian Monitoring App - RIBBiTR "))),
                 column(4,
                        img(src = "nps.png", align = "right", height = "120", width = "120" ),
                        img(src = "ribbitr.png", align = "right", height = "120", width = "150"),
                        img(src = "nsf_logo.png", align = "right", height = "120", width = "120")))),
    
    
    navbarPage("",
               
               tabPanel("Home", icon = icon("info-circle"),
                        fluidPage(
                            fluidRow(
                                h1(strong("Disclaimer"), style = "font-size:20px;"),
                                
                                    column(12, p("This web-based application was created in collaboration with the Resilience Institute Bridging Biological Training and Research (",tags$a(href = "https://ribbitr.com/", "RIBBiTR"), ") and National Park Service (NPS). The data presented within this application contains sensitive species locations and under no circumstances shall this data or user login be shared outside these organizations. "))),
                            
                            fluidRow(
                                h1(strong("Intended Use"),style = "font-size:20px;"),
                                
                                    column(12, p("This application was created for the purpose of the NPS to quickly identify amphibian species counts and Batrachochytrium fungi load across NPS wildernesses in California."))),
                            
                            fluidRow(
                                h1(strong("Data Collection"),style = "font-size:20px;"),
                                    column(12, p("The data presented in this application is part of a long term and ongoing effort to monitor high altitude Sierra Nevada amphibian populations and the amphibian response to the infectious disease Chytridiomycosis. Species counts were summed to an annual site level and Bd load was averaged to an annual site level. All data presented is for the “Adult” visual life stage. A distance tool was incorporated at the bottom left of the map for researchers to gauge distance between sites."))),
                            fluidRow(
                                column(12, align = "center",
                                       div(style = "display: inline;",
                                           img(src = "frog.jpeg",
                                               height = 300,
                                               width = 300)),
                                       div(style = "display: inline;",
                                           img(src = "lake.jpeg",
                                               heigt = 400,
                                               width = 400)),
                                       div(style = "display: inline;",
                                            img(src = "dist.jpeg",
                                                height = 300,
                                                width = 300))))
                        )),
               
               
               tabPanel(title = "Site Map", icon = icon("globe-asia"),
                        
                        sidebarLayout(
                                
                            sidebarPanel(
                                
                                selectInput(inputId = "site_year",
                                            label = "First, select a Year",
                                            choices = sort(unique(data$date), decreasing = T),
                                            multiple = F,
                                            selectize = T,
                                            selected = max(unique(data$date))),
                                
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
