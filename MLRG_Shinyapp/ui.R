source("global.R", local = T)
source("creds.R", local = T)



ui <-  secure_app(head_auth = tags$script(inactivity), 
                  
                  
                  fluidPage(
                    
                    tags$head(
                      tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }")),
      tags$style(HTML(".well {
        border: 1px solid #000000;
      }")), 
      # tags$style(HTML(".col-sm-8 {
      #   border: 1px solid #000000;
      # }")),
      tags$style('body {
        background-color: #cdd1cb;
      }'),
      
      
      includeCSS(here("theme.css")),
      
      #theme = theme,
      
      
      titlePanel(""),
      
      fluidPage(
        fluidRow(column(8,
                        h1(strong("Amphibian Populations App"), style = "font-size:60px;")),
                 column(4,
                        img(src = "mlrg_logo.png", alright = "right", height = "120", width = "400") 
                 ))),
      
      
      navbarPage("", inverse = T,
                 
                 tabPanel("Home", icon = icon("info-circle"),
                          fluidPage(
                            fluidRow(
                              h1(strong("Intended Use"),style = "font-size:20px;"),
                              
                              column(12, p("The data presented in this application were collected as part of a long-term and ongoing effort by the", tags$a(href = "https://mountainlakesresearch.com", "Mountain Lakes Research Group"), "to survey amphibian populations in the southern Sierra Nevada, with a primary focus on mountain yellow-legged frogs. These data contain the locations of federally-endangered species, and under no circumstances shall these data or user credentials be shared without the explicit permission of the Mountain Lakes Research Group.")), style = "font-size:17px"),
                            
                            fluidRow(
                              h1(strong("Acknowledgements"), style = "font-size:20px;"),
                              
                              column(12, p("This web-based application was created by ",tags$a(href = "https://jake-eisaguirre.github.io/", "Jake Eisaguirre"), ", Data Manager for the ",tags$a(href ="https://ribbitr.com/", "Resilience Institue Bridging Biological Training and Research"),"(RIBBiTR). Financial support was provided by the National Science Foundation.")), style = "font-size:17px"),
                            
                            fluidRow(
                              align = "center", div(style = "display: inline", img(src = "ribbitr.png", height = "75", width = "95")),
                              img(src = "nsf_logo.png", align = "center", height = "75", width = "75")),
                            
                            fluidRow(
                              h1(strong("Data Collection"),style = "font-size:20px;"),
                              column(12, p("The data presented in this application is part of a long term and ongoing effort to monitor high altitude Sierra Nevada amphibian populations and the amphibian response to the infectious disease Chytridiomycosis. Data collection protocols can be found", tags$a(href ="https://mountainlakesresearch.com/resources/", "HERE.")), style = "font-size:17px")),
                            fluidRow(
                              column(12, align = "center",
                                     div(style = "display: inline;",
                                         img(src = "good_frog.jpeg",
                                             height = 300,
                                             width = 400)),
                                     
                                     div(style = "display: inline;",
                                         img(src = "new_dist.jpeg",
                                             height = 300,
                                             width = 300))))
                          )),
                 
                 tabPanel(title = "Amphibian Occupancy Map", icon = icon("globe"),
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              fluidRow(width = 3,
                                       column(12, sliderInput(inputId = "site_year",
                                                              label = "Select an annual range",
                                                              min = min(ves_data$date), max = max(ves_data$date), 
                                                              value =  c(max(ves_data$date) - 5, max(ves_data$date)),
                                                              sep = "")),
                                       column(12, pickerInput(inputId = "wilderness",
                                                              label = "Select a jurisdiction",
                                                              choices = unique(ves_data$wilderness),
                                                              options = pickerOptions(title = " "),
                                                              selected = "Yosemite")),
                                       column(12, pickerInput(inputId = "species",
                                                              label = "Select a species",
                                                              choices = unique(ves_data$species),
                                                              options = pickerOptions(title = " "),
                                                              multiple = F,
                                                              selected = "Mountain yellow-legged frog - Rana muscosa")),
                                       column(12, pickerInput(inputId = "stage",
                                                              label = "Select a life stage",
                                                              choices = c("Adult", "Subadult", "Tadpole", "Eggmass"),
                                                              options = pickerOptions(title = " "),
                                                              selected = "Adult",
                                                              multiple = T)),
                                       
                                       fluidRow(column(12, hr(style = "border-top: 1px solid #000000;")),
                                                
                                                fluidRow(column(3, div(style="display:inline-block", 
                                                                       
                                                                       checkboxGroupButtons(inputId = "visits",
                                                                                            label = HTML("All Sites <br/> Visited"),
                                                                                            choices = c("Sites"))), offset = 1),
                                                         
                                                         column(3, div(style="display:inline-block", 
                                                                       
                                                                       checkboxGroupButtons(inputId = "bd_presence",
                                                                                            label = HTML("Bd Detection <br/> Status"),
                                                                                            choices = c("Bd")))))))),
                            
                            # column(5, div(style="display:inline-block", 
                            #               
                            #               checkboxGroupButtons(inputId = "any_all",
                            #                                    label = HTML("Any/All Lifestage <br/> Detection"),
                            #                                    choices = c("Lifestage")))),
                            
                            mainPanel(h5("Map of water bodies and associated site ids, and (depending on selection) detection/non-detection of species/life stages. Detection information is provided by site/year and not based on specific survey dates. Detection and Bd load is quantified based on locations that were surveyed and organisms encountered at sites. A distance tool is provided in the lower-left of the map to allow measure of distances of interest."),
                                      withSpinner(leafletOutput(outputId = "site_map", width = 1000, height = 500)),
                                      withSpinner(DT::dataTableOutput("test_id")),
                                      headerPanel("")))),
                 
                 
                 tabPanel(title = "VES", icon = icon("frog"),
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              sliderInput(inputId = "ves_date",
                                          label = "Select an annual range",
                                          min = min(ves_plot$date), max = max(ves_plot$date), 
                                          value =  c(max(ves_data$date) - 5, max(ves_data$date)),
                                          sep = ""),
                              pickerInput(inputId = "wilderness_1",
                                          label = "Select a jurisdiction",
                                          choices = unique(ves_plot$wilderness),
                                          multiple = F,
                                          options = pickerOptions(title = " ")),
                              pickerInput(inputId = "ves_species",
                                          label = "Select a species",
                                          choices = unique(ves_plot$species),
                                          multiple = F,
                                          options = pickerOptions(title = " ")),
                              pickerInput(inputId = "id",
                                          label = "Site (site ids available in Occupancy Map)",
                                          choices = unique(ves_plot$id),
                                          multiple = F,
                                          options = pickerOptions(title = " ")),
                              # checkboxGroupInput(inputId = "vls",
                              #                    label = "Select life stages",
                              #                    choices = c("Adult", "Subadult,", "Eggmass"),
                              #                    selected = c("Adult", "Subadult,", "Eggmass")),
                              hr(style = "border-top: 1px solid #000000;"),
                              tableOutput("ves_counts")),
                            
                            
                            mainPanel(h5("Counts of each species-life stage are median counts across all surveys conducted per year"),
                                      withSpinner(plotOutput(outputId = "ves_plots", width = 900, height = 850)),
                                      #withSpinner(plotOutput(outputId = "ves_tad", width = 900, height = 375)),
                                      headerPanel(""),
                                      actionButton('ves_agg_download',"Download data",
                                                   icon("download"),
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
                 
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
                                          options = pickerOptions(title = " ")),
                              pickerInput(inputId = "bd_id",
                                          label = "Site (site ids available in Occupancy Map)",
                                          choices = unique(bd_data$id),
                                          multiple = F,
                                          options = pickerOptions(title = " ")),
                              hr(style = "border-top: 1px solid #000000;"),
                              # pickerInput(inputId = "stage_bd",
                              #             label = "Select a life stage",
                              #             choices = unique(bd_data$visual_life_stage),
                              #             selected = c("Adult"),
                              #             multiple = T,
                              #             options = pickerOptions(title = " ",
                              #                                     actionsBox = T)),
                              tableOutput("bd_counts")),
                            
                            mainPanel(
                              h5("Bd loads are determined from skin swabs collected from frogs and analyzed using qPCR. Loads are expressed as median(log10(ITS copies + 1)). The log10(Bd load) above which mortality of post-metamorphic animals is likely approximately 5.8 (red dotted line). The blue line indicates average trend and the integer adjacent to each point represents sample size."),
                              withSpinner(plotOutput(outputId = "bd_plots", width = 900, height = 850)),
                              # withSpinner(plotOutput(outputId = "bd_plots_sub", width = 900, height = 350)),
                              # withSpinner(plotOutput(outputId = "bd_plots_tad", width = 900, height = 350)),
                              headerPanel(""),
                              actionButton('bd_agg_download',"Download data",
                                           icon("download"),
                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
                 
                 navbarMenu(title = "Data Download", icon = icon("download"),
                            
                            tabPanel("Capture Data",
                                     
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         sliderInput(inputId = "data_year",
                                                     label = "Select Annual Range",
                                                     min = min(visit$year), max = max(visit$year),
                                                     value = c(max(visit$year) - 5, max(visit$year)),
                                                     sep = ""),
                                         pickerInput(inputId = "data_jur",
                                                     label = "Select a Jurisdiction",
                                                     choices = unique(site$wilderness),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         hr(style = "border-top: 1px solid #000000;"),
                                         pickerInput(inputId = "data_site",
                                                     label = "Select Site Level Variables",
                                                     choices = colnames(site),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         pickerInput(inputId = "data_visit",
                                                     label = "Select Visit Level Variables",
                                                     choices = colnames(visit),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         pickerInput(inputId = "data_survey",
                                                     label = "Select Survey Level Variables",
                                                     choices = colnames(survey),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         pickerInput(inputId = "data_capture",
                                                     label = "Select Capture Level Variables",
                                                     choices = colnames(capture),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         pickerInput(inputId = "data_bd",
                                                     label = "Select Bd Load Variables",
                                                     choices = colnames(bd_load_table),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE)
                                         
                                         
                                       ),
                                       
                                       mainPanel(
                                         withSpinner(DT::dataTableOutput("cap_table")),
                                         headerPanel(""),
                                         actionButton('cap_download',"Download the data",
                                                      icon("download"),
                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                         actionButton('cap_clear', "Clear Selection",
                                                      icon("trash"),
                                                      style="color: #fff; background-color: red; border-color: black")))),
                            
                            tabPanel("VES Data",
                                     
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         sliderInput(inputId = "data_year_ves",
                                                     label = "Select Annual Range",
                                                     min = min(visit$year), max = max(visit$year),
                                                     value = c(max(visit$year) - 5, max(visit$year)),
                                                     sep = ""),
                                         pickerInput(inputId = "data_jur_ves",
                                                     label = "Select a Jurisdiction",
                                                     choices = unique(site$wilderness),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         hr(style = "border-top: 1px solid #000000;"),
                                         pickerInput(inputId = "data_site_ves",
                                                     label = "Select Site Level Variables",
                                                     choices = colnames(site),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         pickerInput(inputId = "data_visit_ves",
                                                     label = "Select Visit Level Variables",
                                                     choices = colnames(visit),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         pickerInput(inputId = "data_survey_ves",
                                                     label = "Select Survey Level Variables",
                                                     choices = colnames(survey),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         pickerInput(inputId = "data_ves",
                                                     label = "Select VES Level Variables",
                                                     choices = colnames(ves_table),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE)
                                         
                                         
                                       ),
                                       
                                       mainPanel(
                                         withSpinner(DT::dataTableOutput("ves_table")),
                                         headerPanel(""),
                                         actionButton('ves_download',"Download the data",
                                                      icon("download"),
                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                         actionButton('ves_clear', "Clear Selection",
                                                      icon("trash"),
                                                      style="color: #fff; background-color: red; border-color: black")))),
                            tabPanel("Relocate Data",
                                     
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         pickerInput(inputId = "data_year_cmr",
                                                     label = "Select Annual Collection Range",
                                                     choices = sort(unique(relocate_table$year)),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         pickerInput(inputId = "id_cmr",
                                                     label = "Select Release Site ID",
                                                     choices = sort(unique(relocate_table$collect_siteid)),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         prettyCheckboxGroup(inputId = "retran_cmr",
                                                             label = "Select Type of Relocation",
                                                             choices = unique(relocate_table$type),
                                                             inline = T,
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE,
                                                             animation = "jelly"),
                                         hr(style = "border-top: 1px solid #000000;"),
                                         pickerInput(inputId = "relocate_cmr",
                                                     label = "Select Relocation Variables",
                                                     choices = colnames(relocate_table),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE),
                                         pickerInput(inputId = "relocate_frog_cmr",
                                                     label = "Select Relocated Frog Variables",
                                                     choices = colnames(relocate_frog_table),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       size = 10,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE)
                                         
                                         
                                         
                                       ),
                                       
                                       mainPanel(
                                         withSpinner(DT::dataTableOutput("cmr_table")),
                                         headerPanel(""),
                                         actionButton('cmr_download',"Download the data",
                                                      icon("download"),
                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                         actionButton('cmr_clear', "Clear Selection",
                                                      icon("trash"),
                                                      style="color: #fff; background-color: red; border-color: black")))),
                            
                            tabPanel("Metadata",
                                     
                                     tags$iframe(style="height:800px; 
                                             width:100%; 
                                             scrolling=yes", 
                                             src= "db_description.pdf")),
                            hr(style = "border-top: 1px solid #000000;")),
                 
                 
                 
                 
                 
      ))))