source(here("NPs_ShinyApp", "global.R"))


#secure_app(head_auth = tags$script(inactivity), 

ui <- fluidPage(

    
    
    
    #includeCSS(here("NPS_ShinyApp/theme.css")),
    
    theme = theme,
    
    
    titlePanel(""),
    
    fluidPage(
        fluidRow(column(8,
                        h1(strong("National Park Service Amphibian Monitoring App - RIBBiTR "))),
                 column(4,
                        img(src = "nps.png", align = "right", height = "120", width = "120" ),
                        img(src = "ribbitr.png", align = "right", height = "120", width = "150"),
                        img(src = "nsf_logo.png", align = "right", height = "120", width = "120")))),
    
    
    navbarPage("", inverse = T,
               
               tabPanel("Home", icon = icon("info-circle"),
                        fluidPage(
                            fluidRow(
                                h1(strong("Disclaimer"), style = "font-size:20px;"),
                                
                                    column(12, p("This web-based application was created in collaboration with the National Park Service (",tags$a(href = "https://www.nps.gov/index.htm", "NPS"),") and the Resilience Institute Bridging Biological Training and Research (",tags$a(href = "https://ribbitr.com/", "RIBBiTR"),"), funded by the National Science Foundation (",tags$a(href ="https://www.nsf.gov/", "NSF"),"). The data presented within this application contains sensitive species locations and under no circumstances shall this data or user login be shared outside these organizations. "))),
                            
                            fluidRow(
                                h1(strong("Intended Use"),style = "font-size:20px;"),
                                
                                    column(12, p("This application was created for the purpose of the NPS to quickly identify amphibian species counts and Batrachochytrium fungi load across NPS wildernesses in California."))),
                            
                            fluidRow(
                                h1(strong("Data Collection"),style = "font-size:20px;"),
                                    column(12, p("The data presented in this application is part of a long term and ongoing effort to monitor high altitude Sierra Nevada amphibian populations and the amphibian response to the infectious disease Chytridiomycosis. Species counts were summed to an annual site level and Bd load was averaged to an annual site level. All data presented is for the “Adult” visual life stage. A distance tool was incorporated at the bottom left of the site map for researchers to gauge distance between sites."))),
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
                                
                                sliderInput(inputId = "site_year",
                                            label = "Select an annual range",
                                            min = min(data$date), max = max(data$date), 
                                            value =  c(max(data$date) -1, max(data$date)),
                                            sep = ""),
                                
                                pickerInput(inputId = "wilderness",
                                            label = "Select a wilderness",
                                            choices = sort(unique(data$wilderness), decreasing = T),
                                            multiple = F,
                                            selected = ""),
                                
                                pickerInput(inputId = "species",
                                            label = "Select a species",
                                            choices = unique(data$species),
                                            selected = "",
                                            multiple = F),
                                pickerInput(inputId = "stage",
                                            label = "Select a life stage",
                                            choices = unique(data$visual_life_stage),
                                            selected = "",
                                            multiple = F)),
                                
                    mainPanel(leafletOutput(outputId = "site_map", width = 900, height = 500)))
                    
                    ),
               
               
               tabPanel(title = "VES", icon = icon("frog"),
                        
                        sidebarLayout(
                        
                            sidebarPanel(
                            
                                selectInput(inputId = "ves_date",
                                      label = "First, select a Year",
                                      choices = sort(unique(data$date), decreasing = T),
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
                                            choices = sort(unique(data$date), decreasing = T),
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
