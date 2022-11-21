source(here("NPS_ShinyApp", "global.R"))
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
                        h1(strong("Mountain Lakes Research Group Amphibian Populations App"))),
                 column(4,
                        
                        img(src = "ribbitr.png", align = "right", height = "120", width = "150"),
                        img(src = "nsf_logo.png", align = "right", height = "120", width = "120")))),
    
    
    navbarPage("", inverse = T,
               
               tabPanel("Home", icon = icon("info-circle"),
                        fluidPage(
                            fluidRow(
                                h1(strong("Intended Use"),style = "font-size:20px;"),
                                
                                    column(12, p("The data presented in this application were collected as part of a long-term and ongoing effort by the", tags$a(href = "https://mountainlakesresearch.com", "Mountain Lakes Research Group"), "to survey amphibian populations in the southern Sierra Nevada, with a primary focus on mountain yellow-legged frogs. These data contain the locations of federally-endangered species, and under no circumstances shall these data or user credentials be shared without the explicit permission of the Mountain Lakes Research Group."))),
                          
                            fluidRow(
                              h1(strong("Acknowledgements"), style = "font-size:20px;"),
                              
                              column(12, p("This web-based application was created by ",tags$a(href = "https://jake-eisaguirre.github.io/", "Jake Eisaguirre"), ", Data Manager for the ",tags$a(href ="https://ribbitr.com/", "Resilience Institue Bridging Biological Training and Research"),"(RIBBiTR). Financial support was provided by the National Science Foundation. "))),
                            
                            fluidRow(
                                h1(strong("Data Collection"),style = "font-size:20px;"),
                                    column(12, p("The data presented in this application is part of a long term and ongoing effort to monitor high altitude Sierra Nevada amphibian populations and the amphibian response to the infectious disease Chytridiomycosis. Data collection protocols can be found at...."))),
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
               
               
               tabPanel(title = "VES", icon = icon("frog"),
                        
                        sidebarLayout(
                        
                            sidebarPanel(
                            
                                sliderInput(inputId = "ves_date",
                                            label = "Select an annual range",
                                            min = min(ves_data$date), max = max(ves_data$date), 
                                            value =  c(max(ves_data$date) - 5, max(ves_data$date)),
                                            sep = ""),
                                pickerInput(inputId = "wilderness_1",
                                           label = "Select a jurisdiction",
                                           choices = unique(ves_data$wilderness),
                                           multiple = F,
                                           options = pickerOptions(title = " ")),
                                pickerInput(inputId = "ves_species",
                                            label = "Select a species",
                                            choices = unique(ves_data$species),
                                            multiple = F,
                                            options = pickerOptions(title = " ")),
                                pickerInput(inputId = "id",
                                            label = "Select a site",
                                            choices = unique(ves_data$id),
                                            multiple = F,
                                            options = pickerOptions(title = " ")),
                                checkboxGroupInput(inputId = "vls",
                                            label = "Select life stages",
                                            choices = unique(ves_data$visual_life_stage),
                                            selected = unique(ves_data$visual_life_stage)),
                                tableOutput("ves_counts")),
                            
                        mainPanel(h5("Counts of each species-life stage are median counts across all surveys conducted per year"),
                                  withSpinner(plotOutput(outputId = "ves_plots", width = 900, height = 500))))
                            
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
                                            label = "Select a jurisdiction",
                                            choices = unique(bd_data$wilderness),
                                            multiple = F,
                                            options = pickerOptions(title = " ")),
                                pickerInput(inputId = "bd_species",
                                            label = "Select a species",
                                            choices = unique(bd_data$species),
                                            multiple = F,
                                            selected = "adult",
                                            options = pickerOptions(title = " ")),
                                pickerInput(inputId = "bd_id",
                                            label = "select a site",
                                            choices = unique(bd_data$id),
                                            multiple = F,
                                            options = pickerOptions(title = " ")),
                                checkboxGroupInput(inputId = "stage_bd",
                                                   label = "Select a life stage",
                                                   choices = unique(bd_data$visual_life_stage),
                                                   selected = unique(bd_data$visual_life_stage)),
                                tableOutput("bd_counts")),
                        
                        mainPanel(h5("Bd loads are determined from skin swabs collected from frogs and analyzed using qPCR. Loads are expressed as median(log10(ITS copies + 1)). The log(Bd load) above which mortality of post-metamorphic animals is likely approximately 5.8 (red dotted line). Th blue line represents the average trend of Bd load"),
                                  withSpinner(plotOutput(outputId = "bd_plots", width = 900, height = 500))))
                            
                        ),
               
               tabPanel(title = "Amphibian Occupancy Map", icon = icon("globe"),
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            sliderInput(inputId = "site_year",
                                        label = "Select an annual range",
                                        min = min(ves_data$date), max = max(ves_data$date), 
                                        value =  c(max(ves_data$date), max(ves_data$date)),
                                        sep = ""),
                            pickerInput(inputId = "wilderness",
                                        label = "Select a jurisdiction",
                                        choices = unique(ves_data$wilderness),
                                        multiple = F,
                                        options = pickerOptions(title = " ")),
                            pickerInput(inputId = "species",
                                        label = "Select a species",
                                        choices = unique(ves_data$species),
                                        options = pickerOptions(title = " "),
                                        multiple = F),
                            pickerInput(inputId = "stage",
                                        label = "Select a life stage",
                                        choices = unique(ves_data$visual_life_stage),
                                        options = pickerOptions(title = " "),
                                        multiple = F),
                            checkboxGroupButtons(inputId = "visits",
                                                 label = "All Sites Visited",
                                                 choices = c("Sites")),
                            h5("*Please be patient, map rendering can be delayed dependent on inputs*"),
                            hr(style = "border-top: 1px solid #000000;"),
                            checkboxGroupButtons(inputId = "clear",
                                                 label = "Clear Selection",
                                                 choices = c("Clear"))),
                          
                          mainPanel(h5("Map of water bodies and associated site ids, and (depending on selection) detection/non-detection of species/life stages. Detection information is provided by site/year and not based on specific survey dates. A distance tool is provided in the lower-left of the map to allow measure of distances of interest"),
                                    withSpinner(leafletOutput(outputId = "site_map", width = 900, height = 500))))
                        
               )
    
    
)))
