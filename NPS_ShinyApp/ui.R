source(here("NPs_ShinyApp", "global.R"))


ui <- fluidPage(
    
    
    
    
    titlePanel("NPS Amphib Data"),
    
    
    navbarPage("",
               
               tabPanel("Home", icon = icon("info-circle")),
               
               tabPanel(title = "VES", icon = icon("frog"),
                        
                        
                        sidebarLayout(
                        
                            sidebarPanel(
                            
                                selectInput(inputId = "ves_date",
                                      label = "Select a Year",
                                      choices = sort(ves$date, decreasing = T),
                                      multiple = F,
                                      selectize = T)),
                        
                        mainPanel(plotOutput(outputId = "ves_plots"))
                            
                            
                        )),
               
               tabPanel(title = "Bd Load", icon = icon("bacterium"),
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                selectInput(inputId = "bd_date",
                                            label = "Select a Year",
                                            choices = sort(bd$date, decreasing = T),
                                            multiple = F,
                                            selectize = T)),
                        
                        mainPanel = plotOutput(outputId = "bd_plots")
                            
                        ))),
    
    
    
    
    
    
    
    
    
    
)
