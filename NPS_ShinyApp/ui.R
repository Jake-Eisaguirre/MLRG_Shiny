source(here("NPs_ShinyApp", "global.R"))
source(here("NPS_ShinyApp", "creds.R"))



ui <-  secure_app(head_auth = tags$script(inactivity), 
                  
    
    fluidPage(tags$head(
    
    
    
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;}"))),

    
    includeCSS(here("NPS_ShinyApp/theme.css")),
    
    #theme = theme,
    
    
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
                                    column(12, p("The data presented in this application is part of a long term and ongoing effort to monitor high altitude Sierra Nevada amphibian populations and the amphibian response to the infectious disease Chytridiomycosis. Species counts were summed to an annual site level with the natural log of Bd loads presetented at site level and wilderness level median Bd loads. A distance tool was incorporated at the bottom left of the site map for researchers to gauge distance between sites."))),
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
               
               
               tabPanel(title = "Site Map", icon = icon("globe"),
                        
                        sidebarLayout(
                                
                            sidebarPanel(
                                
                                sliderInput(inputId = "site_year",
                                            label = "Select an annual range",
                                            min = min(ves_data$date), max = max(ves_data$date), 
                                            value =  c(max(ves_data$date), max(ves_data$date)),
                                            sep = ""),
                                
                                pickerInput(inputId = "wilderness",
                                            label = "Select a wilderness",
                                            choices = unique(ves_data$wilderness),
                                            multiple = F,
                                            options = pickerOptions(title = "Select Variable")),
                                pickerInput(inputId = "species",
                                            label = "Select a species",
                                            choices = unique(ves_data$species),
                                            options = pickerOptions(title = "Select Variable"),
                                            multiple = F),
                                pickerInput(inputId = "stage",
                                            label = "Select a life stage",
                                            choices = unique(ves_data$visual_life_stage),
                                            options = pickerOptions(title = "Select Variable"),
                                            multiple = F)),
                                
                    mainPanel(withSpinner(leafletOutput(outputId = "site_map", width = 900, height = 500))))
                    
                    ),
               
               
               tabPanel(title = "VES", icon = icon("frog"),
                        
                        sidebarLayout(
                        
                            sidebarPanel(
                            
                                sliderInput(inputId = "ves_date",
                                            label = "Select an annual range",
                                            min = min(ves_data$date), max = max(ves_data$date), 
                                            value =  c(max(ves_data$date), max(ves_data$date)),
                                            sep = ""),
                                
                                pickerInput(inputId = "wilderness_1",
                                           label = "Select a wilderness",
                                           choices = unique(ves_data$wilderness),
                                           multiple = F,
                                           options = pickerOptions(title = "Select Variable")),
                                pickerInput(inputId = "ves_species",
                                            label = "Select a species",
                                            choices = unique(ves_data$species),
                                            multiple = F,
                                            options = pickerOptions(title = "Select Variable")),
                                pickerInput(inputId = "id",
                                            label = "Select a site",
                                            choices = unique(ves_data$id),
                                            multiple = F,
                                            options = pickerOptions(title = "Select Variable")),
                                tableOutput("ves_counts")),
                            
                        
                        mainPanel(withSpinner(plotOutput(outputId = "ves_plots", width = 900, height = 500))))
                            
                        ),
               
               
               tabPanel(title = "Bd Load", icon = icon("bacterium"),
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                sliderInput(inputId = "bd_date",
                                            label = "Select an annual range",
                                            min = min(bd_data$date), max = max(bd_data$date), 
                                            value =  c(max(bd_data$date) - 5, max(bd_data$date)),
                                            sep = ""),
                                pickerInput(inputId = "wilderness_2",
                                            label = "Select a wilderness",
                                            choices = unique(bd_data$wilderness),
                                            multiple = F,
                                            options = pickerOptions(title = "Select Variable")),
                                pickerInput(inputId = "bd_species",
                                            label = "Select a species",
                                            choices = unique(bd_data$species),
                                            multiple = F,
                                            selected = "adult",
                                            options = pickerOptions(title = "Select Variable")),
                                pickerInput(inputId = "stage_bd",
                                            label = "Select a life stage",
                                            choices = unique(bd_data$visual_life_stage),
                                            multiple = F,
                                            selected = "",
                                            options = pickerOptions(title = "Select Variable")),
                                pickerInput(inputId = "bd_id",
                                            label = "select a site",
                                            choices = unique(bd_data$id),
                                            multiple = F,
                                            options = pickerOptions(title = "Select Variable")),
                                tableOutput("bd_counts")),
                        
                        mainPanel(withSpinner(plotOutput(outputId = "bd_plots", width = 900, height = 500))))
                            
                        )
    
    
)))
