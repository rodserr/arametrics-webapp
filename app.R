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
    # TPL----
# TPL_claims <- read_csv('data/french-motor-TPL/claims_TPL.csv')
data(freMTPL2freq)
data(freMTPL2sev)
freq <- freMTPL2freq
sev <- freMTPL2sev

french_map <- jsonlite::fromJSON('data/french-motor-TPL/fr-all.geo.json', simplifyVector = F)
map_division <- read_csv('data/french-motor-TPL/departments.csv')

freq$ClaimNb <- pmin(freq$ClaimNb, 4)
freq$Exposure <- pmin(freq$Exposure, 1)

group_sev <- sev %>% 
    group_by(IDpol) %>% 
    summarise(claimAmount = sum(ClaimAmount))

TPL_claims <- freq %>% 
    transmute(IDpol,
              area = as.integer(Area),
              vehPower = as.factor(pmin(VehPower, 9)),
              vehAge = case_when(VehAge == 0 ~ 'Nuevo',
                                 VehAge < 10 ~ '<10 años',
                                 T ~ '>= 10 años'),
              drivAge = cut(DrivAge, c(18,21,26,31,41,51, 71, 101), right = F) %>% as.character(),
              bonusMalus = as.integer(pmin(BonusMalus, 150)),
              density = Density,
              vehGas = VehGas,
              vehBrand = VehBrand %>% as.character(),
              exposure = Exposure,
              claimN = ClaimNb,
              region_id = str_remove(Region, 'R')
    ) %>% 
    left_join(map_division %>% select(n, department, region_before_2016), by = c('region_id' = 'n')) %>% 
    left_join(group_sev, by = 'IDpol')

drivAge_choices <- unique(TPL_claims$drivAge)
vehGas_choices <- unique(TPL_claims$vehGas)
vehAge_choices <- unique(TPL_claims$vehAge)
vehBrand_choices <- unique(TPL_claims$vehBrand)

# UI----
ui <- shinyUI(
    dashboardPage(
        # Application title
        dashboardHeader(title = 'Arametric Dashboard'),
        
        # Sidebar
        dashboardSidebar(
            tags$a(
                   tags$img(src = "am-logo-white.png", height = 100, width = 200)),
            br(),
            br(),
            
            sidebarMenu(
                # Setting id makes input$tabs give the tabName of currently-selected tab
                id = "tabs",
                
                menuItem("Motor Portfolio TPL", tabName = "TPL_motor", icon = icon("bar-chart-o")),
                menuItem("otro Analisis", tabName = "otro_dataset", icon = icon("th"))
            )
            
        ),
        
        # Body
        dashboardBody(
            tabItems(
                tabItem("TPL_motor",
                        tabsetPanel(
                            tabPanel('Graficos Descrp',
                                     br(),
                                     fluidRow(
                                         column(3, valueBoxOutput('TPL_n_claims', 12)),
                                         column(3, valueBoxOutput('TPL_amount_claims', 12)),
                                         column(3, valueBoxOutput('TPL_exposure', 12)),
                                         column(3, 
                                                radioButtons('mapRadioB', 'Variable de estudio:',
                                                      choices = c('Expuesto' = 'exp',
                                                                  'Nº Reclamos' = 'n_claims',
                                                                  'Nº Polizas' = 'n_pol',
                                                                  'Monto de Reclamos' = 'claimAmount'),
                                                      selected = 'n_claims',
                                                      inline = F))
                                     ),
                                     fluidRow(
                                         column(5,
                                                box(width =  12,
                                                    shinyWidgets::pickerInput(
                                                        inputId = "drivAge_filter", 
                                                        label = "Edad del Conductor", 
                                                        choices = drivAge_choices, 
                                                        options = list(`actions-box` = TRUE), 
                                                        multiple = TRUE,
                                                        selected = drivAge_choices
                                                    ),
                                                    
                                                    shinyWidgets::pickerInput(
                                                        inputId = "vehGas_filter", 
                                                        label = "Tipo de Vehiculo", 
                                                        choices = vehGas_choices, 
                                                        options = list(`actions-box` = TRUE), 
                                                        multiple = TRUE,
                                                        selected = vehGas_choices
                                                    ),
                                                    
                                                    shinyWidgets::pickerInput(
                                                        inputId = "vehAge_filter", 
                                                        label = "Año del Vehiculo", 
                                                        choices = vehAge_choices, 
                                                        options = list(`actions-box` = TRUE), 
                                                        multiple = TRUE,
                                                        selected = vehAge_choices
                                                    ),
                                                    
                                                    shinyWidgets::pickerInput(
                                                        inputId = "vehBrand_filter", 
                                                        label = "Marca de Vehiculo", 
                                                        choices = vehBrand_choices, 
                                                        options = list(`actions-box` = TRUE), 
                                                        multiple = TRUE,
                                                        selected = vehBrand_choices
                                                    )
                                                )
                                                )
                                         
                                         ,
                                         column(7,
                                                box(width =  12, highchartOutput("mapPlot") %>% withSpinner())
                                                )
                                         
                                     ),
                                     fluidRow(
                                         box(highchartOutput("TPL_bar_drivAge") %>% withSpinner(), width = 4),
                                         box(highchartOutput("TPL_bar_vehBrand") %>% withSpinner(), width = 4),
                                         box(highchartOutput("TPL_bar_vehAge") %>% withSpinner(), width = 2),
                                         box(highchartOutput("TPL_bar_vehGas") %>% withSpinner(), width = 2)
                                     )
                                     ),
                            tabPanel('Cotizador')
                                
                            )
                        )
                    ,
                
                tabItem("otro_dataset"
                    
                )
            )

        )
    )
)

# Server----
server <- function(input, output) {
    
    filter_TPL_claims <- reactive({
        req(input$drivAge_filter)
        req(input$vehGas_filter)
        req(input$vehBrand_filter)
        req(input$vehAge_filter)
        
        TPL_claims %>%
            filter(drivAge %in% input$drivAge_filter,
                   vehGas %in% input$vehGas_filter,
                   vehBrand %in% input$vehBrand_filter,
                   vehAge %in% input$vehAge_filter
            )
        
    })
    
    TPL_list <- reactive({
        
        df <- filter_TPL_claims()
        
        mapsSelection <- df  %>% 
            group_by(region_before_2016) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T)) %>% 
            mutate(region_before_2016 = case_when(region_before_2016 == 'Pays-de-la-Loire' ~ 'Pays de la Loire',
                                                  region_before_2016 == "Provence-Alpes-Côte d'Azur" ~ "Provence-Alpes-Côte-d'Azur",
                                                  region_before_2016 == 'Rhone Alpes' ~ 'Rhône-Alpes',
                                                  T ~ region_before_2016)) %>% 
            select(region_before_2016, input$mapRadioB)
        
        bar_drivAge <- df %>% 
            group_by(drivAge) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T)) %>% 
            select(drivAge, value = input$mapRadioB)
        
        bar_vehBrand <- df %>% 
            group_by(vehBrand) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T))%>% 
            select(vehBrand, value = input$mapRadioB)
        
        bar_vehAge <- df %>% 
            group_by(vehAge) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T))%>% 
            select(vehAge, value = input$mapRadioB)        
        
        bar_vehGas <- df %>% 
            group_by(vehGas) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T))%>% 
            select(vehGas, value = input$mapRadioB)
        
        list(mapsSelection = mapsSelection, bar_drivAge = bar_drivAge, bar_vehBrand = bar_vehBrand, 
             bar_vehAge = bar_vehAge, bar_vehGas = bar_vehGas)
        
        
    })
    
    output$TPL_amount_claims <- renderValueBox({
        
        value <- filter_TPL_claims()$claimAmount %>% sum(na.rm = T) %>% round(2) %>% format(big.mark = ',')
        
        valueBox(
            value = value,
            subtitle = 'Monto de Reclamos', icon = icon('euro-sign', lib = 'font-awesome')
        )
    })
    
    output$TPL_n_claims <- renderValueBox({
        
        value <- filter_TPL_claims()$claimN %>% sum(na.rm = T) %>% round(0) %>% format(big.mark = ',')
        
        valueBox(
            value = value,
            subtitle = 'Nº de Reclamos', icon = icon('fas fa-file-alt', lib = 'font-awesome')
        )
    })
    
    output$TPL_exposure <- renderValueBox({
        
        value <- filter_TPL_claims()$exposure %>% sum(na.rm = T) %>% round(2) %>% format(big.mark = ',')
        
        valueBox(
            value = value,
            subtitle = 'Expuestos', icon = icon('signal', lib = 'font-awesome')
        )
    })
    
    output$TPL_bar_drivAge <- renderHighchart({
        
        l <- TPL_list()$bar_drivAge
        
        hchart(l, "column", hcaes(x = drivAge, y = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Edad del Conductor")
    })
    
    output$TPL_bar_vehBrand <- renderHighchart({
        
        l <- TPL_list()$bar_vehBrand
        
        hchart(l, "column", hcaes(x = vehBrand, y = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Marca de Vehiculo")
    })
    
    output$TPL_bar_vehGas <- renderHighchart({
        
        l <- TPL_list()$bar_vehGas
        
        hchart(l, "column", hcaes(x = vehGas, y = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Tipo de Vehiculo")
    })
    
    output$TPL_bar_vehAge <- renderHighchart({
        
        l <- TPL_list()$bar_vehAge
        
        hchart(l, "column", hcaes(x = vehAge, y = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Año de Vehiculo")
    })
    
    output$mapPlot <- renderHighchart({
        
        l <- TPL_list()$mapsSelection
        
        highchart() %>% 
            hc_add_series_map(french_map, l, value = input$mapRadioB,
                              joinBy = c("name", "region_before_2016"), name = "n",
                              dataLabels = list(enabled = TRUE, format = '{point.name}'),
                              borderColor = "#FAFAFA", borderWidth = 0.1,
                              tooltip = list(valueDecimals = 0, valuePrefix = "$", valueSuffix = " ")) %>%
            hc_title(text = "Distribucion de siniestros por Region")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
