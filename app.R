# Libraries----
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(highcharter)
library(magrittr)
library(tidyverse)
library(lubridate)
library(CASdatasets)
library(jsonlite)

# Data----
TPL_claims <- read_csv('data/french-motor-TPL/claims_TPL.csv')
french_map <- jsonlite::fromJSON('data/french-motor-TPL/fr-all.geo.json', simplifyVector = F)


# UI----
ui <- shinyUI(
    dashboardPage(
        # Application title
        dashboardHeader(title = 'Arametric Dashboard'),
        
        # Sidebar
        dashboardSidebar(
            # tags$a(
            #        tags$img(src = "am-logo-black.png", height = 75, width = 200)),
            # br(),
            # br(),
            
        ),
        
        # Body
        dashboardBody(
            fluidRow(
                column(9,
                    highchartOutput("mapPlot") %>% withSpinner()
                ),
                column(3,
                    radioButtons('mapRadioB', 'Select Sample In/Out',
                                 choices = c('Expuesto' = 'exp',
                                             'Nº Reclamos' = 'n_claims',
                                             'Nº Polizas' = 'n_pol',
                                             'Monto de Reclamos' = 'claimAmount'),
                                 selected = 'n_claims',
                                 inline = F)
                )
            ),
            fluidRow(
                column(6,
                       highchartOutput("TPL_bar_drivAge") %>% withSpinner()
                ),
                column(6,
                       highchartOutput("TPL_bar_vehBrand") %>% withSpinner()
                )
            )
        )
    )
)

# Server----
server <- function(input, output) {
    
    filter_TPL_claims <- reactive({
        
        TPL_claims
        
    })
    
    mapSelection <- reactive({
        
        filter_TPL_claims() %>% 
            group_by(region_before_2016) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T)) %>% 
            mutate(region_before_2016 = case_when(region_before_2016 == 'Pays-de-la-Loire' ~ 'Pays de la Loire',
                                                  region_before_2016 == "Provence-Alpes-Côte d'Azur" ~ "Provence-Alpes-Côte-d'Azur",
                                                  region_before_2016 == 'Rhone Alpes' ~ 'Rhône-Alpes',
                                                  T ~ region_before_2016)) %>% 
            select(region_before_2016, input$mapRadioB)
        
    })
    
    output$TPL_bar_drivAge <- renderHighchart({
        
        grouped <- filter_TPL_claims() %>% 
            group_by(drivAge) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T)) %>% 
        select(drivAge, input$mapRadioB)
        
        highchart() %>% 
            hc_add_series(data = grouped, type = "bar", hcaes(y = input$mapRadioB),
                          showInLegend = F, name = 'und vendidas')
    })
    
    output$TPL_bar_vehBrand <- renderHighchart({
        
        grouped <- filter_TPL_claims() %>% 
            group_by(vehBrand) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T))
        
        hchart(grouped, "column", hcaes(x = vehBrand, y = input$mapRadioB)) %>% 
            hc_xAxis(title = '')
    })
    
    output$mapPlot <- renderHighchart({
        
        highchart() %>% 
            hc_add_series_map(french_map, mapSelection(), value = input$mapRadioB,
                              joinBy = c("name", "region_before_2016"), name = "n",
                              dataLabels = list(enabled = TRUE, format = '{point.name}'),
                              borderColor = "#FAFAFA", borderWidth = 0.1,
                              tooltip = list(valueDecimals = 0, valuePrefix = "$", valueSuffix = " ")) %>%
            hc_title(text = "Distribucion de siniestros por Region")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
