# Libraries----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyWidgets)
library(highcharter)
library(magrittr)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(DT)
library(zoo)
library(xts)
library(forecast)
library(ChainLadder)
library(plotly)
library(jsonlite)
library(rtweet)

# Data----
    # Docs----
doc <- read_file('data/docu.txt')
    # TPL----
french_map <- jsonlite::fromJSON('data/french-motor-TPL/fr-all.geo.json', simplifyVector = F)
map_division <- read_csv('data/french-motor-TPL/departments.csv')

TPL_claims <- read_csv('data/french-motor-TPL/claims.csv') %>% 
    mutate(area = as.integer(area),
           vehPower = as.factor(vehPower),
           bonusMalus = as.integer(bonusMalus)
           )

# DF for table output
out_table <- TPL_claims %>% 
    transmute(IDpol = as.character(IDpol),
              vehPower = as.factor(vehPower),
              drivAge = as.factor(drivAge),
              vehAge = case_when(vehAge == '<10 años' ~ '-10 años',
                                 vehAge == '>=10 años' ~ '+10 años',
                                 T ~ 'Nuevo') %>% as.factor(),
              bonusMalus = as.factor(bonusMalus),
              density,
              vehGas = as.factor(vehGas),
              vehBrand = as.factor(vehBrand),
              exposure,
              claimN,
              region = as.character(region_before_2016),
              claimAmount
    )

drivAge_choices <- unique(TPL_claims$drivAge)
vehGas_choices <- unique(TPL_claims$vehGas)
vehAge_choices <- unique(TPL_claims$vehAge)
vehBrand_choices <- unique(TPL_claims$vehBrand)

helpText_TPL <- 'Este modulo muestra el performance de una cartera de Resposabilidad Civil Vehículos <br>
<br>
Despliega gráficos descriptivos con la opción de cambiar la variable a estudiar y muestra indicadores en base a los 
inputs seleccionados <br>
<br>
Realiza una simulación de frecuencia y severidad de siniestros y despliega gráficos de barras condicionados a inputs de 
retención de Reaseguro <br>
<br>
Muestra también una tabla con el detalle por póliza'

    # ts-InterestRate----
i_rate <- read_csv('data/ts-interest-rate/long_term_interest_rates.csv') %>% 
    mutate(dt = as.yearmon(time))

iRate_country_choices <- unique(i_rate$country)

helpText_iRate <- 'Este modulo realiza el forecast de la tasa de interes a largo plazo del país seleccionado, con
técnicas de series de tiempo, se trabaja con data mensual <br>
<br>
En el panel Overview se observa un Mapa de calor con las tasas agrupadas por Q de cada país disponible. 
Se despliega también gráficos de descomposición de serie de tiempo STL -Seasonal and Trend decomposition using Loess- \n
asi como gráficos de Autocorrelación y Autocorrelación Parcial <br>
<br>
Se modela la serie de tiempo en data de entrenamiento utilizando varias técnicas y 
se realiza el forecast en la data de prueba, se muestra también un cuadro comparativo de los modelos <br>
<br>
Finalmente se pronostica en función al modelo seleccionado'

    # IBNR----
data('MW2014')

.aux <- tibble(year = c(rep(2015, 4), rep(2016, 4), rep(2017, 4), rep(2018, 4), 2019),
               q = c(rep(1:4, 4), 1),
               aux = as.character(seq(1, 17))) %>% 
    transmute(origin_aux = paste0(year, '0', q), aux)

.df <- as.data.frame(MW2014, na.rm = T)

ibnr_tri <- left_join(.df, .aux, by = c('origin' = 'aux')) %>% 
    transmute(origin = origin_aux, dev, value) %>% 
    as.triangle()

    # EM----
EM_states <- tibble(state = c('Caracas', 'Maracaibo', 'La Guaira', 'Maracay', 'Pto. Ordaz'),
                    lat = c(10.491, 10.6417, 10.6038, 10.2353, 8.2751),
                    long = c(-66.902, -71.6295, -67.0303, -67.5911, -62.7677),
                    exp = round(runif(5)*10000, 0)) %>% 
    mutate(sa = round(exp*rgamma(5, 2)*10000, 0))

EM_darksy_key <- Sys.getenv("darksky_api_key")

EM_token <- create_token(
    app = Sys.getenv("app_name"),
    consumer_key = Sys.getenv("api_key"),
    consumer_secret = Sys.getenv("api_secret_key"),
    access_token = Sys.getenv("access_token"),
    access_secret = Sys.getenv("access_token_secret"))

# UI----
ui <- shinyUI(
    dashboardPage(
        # Application title
        dashboardHeader(title = 'Arametrics Dashboard'),
        
        # Sidebar----
        dashboardSidebar(
            br(),
            tags$a(
                   tags$img(src = "am-logo-white.png", height = 120, width = 200)),
            br(),
            br(),
            
            sidebarMenu(
                # Setting id makes input$tabs give the tabName of currently-selected tab
                id = "tabs",
                
                menuItem("Motor Portfolio TPL", tabName = "TPL_motor", icon = icon("bar-chart-o")),
                menuItem("Long-Term IRate Forecast", tabName = "iRate", icon = icon("chart-area")),
                menuItem("IBNR Development triangles", tabName = "ibnr", icon = icon("calendar-alt")),
                menuItem("Emergency Monitor", tabName = "EM", icon = icon("exclamation"))
            ),
            
            br(),
            actionBttn(
                inputId = "doc",
                label = " Docs",
                color = "succes",
                style = "jelly",
                size = "sm",
                icon = icon("file-signature"),
                block = F)
            
            # conditionalPanel(
            #     condition = "input.tabs == 'TPL_motor'",
            #     span(HTML(helpText_TPL), style = 'color:gray')
            # ),
            # 
            # conditionalPanel(
            #     condition = "input.tabs == 'iRate'",
            #     span(HTML(helpText_iRate), style = 'color:gray')
            # )
            
        ),
        
        # Body-----
        dashboardBody(
            tabItems(
                # TPL motor----
                tabItem("TPL_motor",
                        tabsetPanel(
                            tabPanel('Portafolio',
                                     br(),
                                     fluidRow(
                                         column(4, valueBoxOutput('TPL_n_claims', 12)),
                                         column(4, valueBoxOutput('TPL_amount_claims', 12)),
                                         column(4, valueBoxOutput('TPL_exposure', 12))
                                         ),
                                     fluidRow(
                                         column(4, offset = 1,
                                                radioButtons('TPL_select_var', 'Variable de estudio:',
                                                             choices = c('Expuesto' = 'exp',
                                                                         'Nº Reclamos' = 'n_claims',
                                                                         'Nº Polizas' = 'n_pol',
                                                                         'Monto de Reclamos' = 'claimAmount'),
                                                             selected = 'n_claims',
                                                             inline = T),
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
                                                ),
                                         column(7,
                                                highchartOutput("TPL_map", width = '500px') %>% withSpinner())
                                         ),
                                     fluidRow(
                                         br(),
                                         column(4, highchartOutput("TPL_bar_drivAge") %>% withSpinner()),
                                         column(4, highchartOutput("TPL_bar_vehBrand") %>% withSpinner()),
                                         column(2, highchartOutput("TPL_bar_vehAge") %>% withSpinner()),
                                         column(2, highchartOutput("TPL_bar_vehGas") %>% withSpinner())
                                         )
                                     ),
                            tabPanel('Simulacion de Reclamos',
                                     br(),
                                     fluidRow(
                                         column(7, h3("Nº Simulaciones"),
                                                sliderInput("TPL_sim_n", ' ',
                                                            min = 10000, max = 100000, value = 50000, step = 10000 
                                                    )
                                                ),
                                         column(5,
                                                fluidRow(h3("Limite de exceso")),
                                                fluidRow(
                                                    column(6,
                                                               numericInput(
                                                                   inputId = "TPL_sim_claim_limit", 
                                                                   label = "Por Reclamo", 
                                                                   value = 2000
                                                                   )
                                                               ),
                                                        column(6, 
                                                               numericInput(
                                                                   inputId = "TPL_sim_group_limit", 
                                                                   label = "Por Poliza", 
                                                                   value = 4000
                                                                   )
                                                               )
                                                        )
                                                    )
                                         ),
                                     fluidRow(
                                         column(12,
                                                highchartOutput("TPL_histo_sim_gross") %>% withSpinner()),
                                         fluidRow(
                                             column(6,
                                                    highchartOutput("TPL_histo_sim_net") %>% withSpinner()
                                                    ),
                                             column(6,
                                                    highchartOutput("TPL_histo_sim_ceded") %>% withSpinner())
                                             )
                                         )
                                    ),
                            
                            tabPanel('Detalle por Poliza',
                                     DT::dataTableOutput('TPL_table'))
                            )
                        ),
                # ts I Rate------
                tabItem("iRate",
                        tabsetPanel(
                            tabPanel('Overview',
                                     br(),
                                     selectInput('iRate_select_country', 'Select Country: ',
                                                 choices = iRate_country_choices,
                                                 selected = 'United States'),
                                     tabBox(
                                         side = "left", width  = 12, height = NULL,
                                         selected = "Heatmap",
                                         tabPanel("Heatmap",
                                                  highchartOutput("iRate_heatmap", height = '500px') %>% withSpinner()
                                                  ),
                                         tabPanel("STL",
                                                  highchartOutput("iRate_stl") %>% withSpinner()   
                                                  ),
                                         tabPanel("Diff",
                                                  highchartOutput("iRate_diff_plot") %>% withSpinner() 
                                                  ),
                                         tabPanel("ACF",
                                                  fluidRow(
                                                      column(6,
                                                             highchartOutput("iRate_ACF") %>% withSpinner()
                                                             ),
                                                      column(6,
                                                             highchartOutput("iRate_PACF") %>% withSpinner()
                                                             )
                                                      )
                                                  )
                                         )
                            ),
                            tabPanel('Modelling',
                                     br(),
                                     highchartOutput("iRate_agg_forecast") %>% withSpinner(),
                                     h3('Tabla Comparativa'),
                                     DT::dataTableOutput('iRate_comparative_table')
                                     ),
                            tabPanel('Forecast',
                                br(),
                                fluidRow(
                                    column(6,
                                           selectInput('iRate_select_model', 'Select Model: ',
                                                       choices = c('ARIMA', 'ETS', 'STL', 'NNAR', 'TBATS', 'Combine'),
                                                       selected = 'ARIMA'), 
                                           actionButton('iRate_run', 'Run')
                                           ),
                                    column(6, 
                                           sliderInput('iRate_h', 'Select Forecast Horizon: ',
                                                       min = 1, max = 100, step = 10, value = 10)
                                           )
                                    ),
                                highchartOutput('iRate_indv_forecast') %>% withSpinner()
                                )
                            )
                        ),
                # IBNR-----
                tabItem("ibnr",
                        tabsetPanel(
                            tabPanel('Overview',
                                     br(),
                                     fluidRow(
                                         tabBox(
                                             side = "left", width  = 12, height = NULL,
                                             selected = "Triangulo",
                                             tabPanel("Triangulo", 
                                                      radioButtons('ibnr_ovw_cum', 'Mostrar:',
                                                                   choices = c('Acumulativo' = T,
                                                                               'Discreto' = F),
                                                                   selected = F,
                                                                   inline = T),
                                                      DT::dataTableOutput('ibnr_ovw_tri_table') %>% withSpinner()
                                                      ),
                                             tabPanel("Grafico",
                                                      plotlyOutput('ibnr_ovw_plot') %>% withSpinner()
                                                      )
                                             )
                                         )
                                     ),
                            
                            tabPanel('ChainLadder',
                                     br(),
                                     fluidRow(
                                         tabBox(
                                             side = "left", width  = 12, height = NULL,
                                             selected = "LinkRatios",
                                             tabPanel("LinkRatios",
                                                      DT::dataTableOutput('ibnr_cl_tri') ,
                                                      # DT::dataTableOutput('ibnr_cl_tri_result')
                                                      column(12,
                                                             offset = 1,
                                                             DT::dataTableOutput('ibnr_cl_tri_result'))
                                                      ),
                                             tabPanel("Resultados",
                                                      DT::dataTableOutput('ibnr_cl_result')
                                                      )
                                             )
                                         )
                                     ),
                            
                            tabPanel('Mack',
                                     br(),
                                     fluidRow(
                                         tabBox(
                                             side = "left", width  = 12, height = NULL,
                                             selected = "Resultado",
                                             tabPanel("Resultado", 
                                                      DT::dataTableOutput('ibnr_mack_result') %>% withSpinner()
                                                      ),
                                             tabPanel("Barras",
                                                      plotlyOutput('ibnr_mack_bar') %>% withSpinner()
                                                      ),
                                             tabPanel("ResVSFitt",
                                                      plotlyOutput('ibnr_mack_resvfit') %>% withSpinner(),
                                                      plotlyOutput('ibnr_mack_resvsori') %>% withSpinner()
                                                      ),
                                             tabPanel("Forecast",
                                                      plotlyOutput('ibnr_mack_ovw') %>% withSpinner()
                                                      )
                                             )
                                         )
                                     ),
                            
                            tabPanel('Bootstrap',
                                     br(),
                                     fluidRow(
                                         tabBox(
                                             side = "left", width  = 12, height = NULL,
                                             selected = "Resultado",
                                             tabPanel("Resultado", 
                                                      DT::dataTableOutput('ibnr_boots_result')
                                                      ),
                                             tabPanel("Histo",
                                                      plotlyOutput('ibnr_boots_histo')
                                                      ),
                                             tabPanel("ecdf",
                                                      plotOutput('ibnr_boots_ecdf')
                                                      ),
                                             tabPanel("BoxPLot",
                                                      plotlyOutput('ibnr_boots_ult'),
                                                      plotlyOutput('ibnr_boots_ibnr')
                                                      ),
                                             tabPanel('Quantiles',
                                                      DT::dataTableOutput('ibnr_boots_q'))
                                             )
                                         )
                                     )
                            )
                        ),
                # EM-----
                tabItem("EM",
                        tabsetPanel(
                            tabPanel('Forecast',
                                     br(),
                                     fluidRow(
                                         column(4, selectInput('EM_state',
                                                               choices = EM_states$state,
                                                               selected = 'Caracas',
                                                               label = 'Seleccionar Estado')),
                                         column(4, valueBoxOutput('EM_curr_exp', width  = 12)),
                                         column(4, valueBoxOutput('EM_curr_SA', width  = 12))
                                     ),
                                     fluidRow(
                                         column(3, valueBoxOutput('EM_curr_prob', width  = 12)),
                                         column(3, valueBoxOutput('EM_curr_rain', width  = 12)),
                                         column(3, valueBoxOutput('EM_curr_wind', width  = 12)),
                                         column(3, valueBoxOutput('EM_curr_temp', width  = 12))
                                     ),
                                     fluidRow(
                                         tabBox(
                                             side = "left", width  = 12, height = NULL,
                                             selected = "Hour",
                                             tabPanel("Hour",
                                                      highchartOutput('EM_hour_rain_plot') %>% withSpinner()
                                             ),
                                             tabPanel("Daily",
                                                      highchartOutput('EM_day_rain_plot') %>% withSpinner()
                                             )
                                         )
                                     ),
                                     br(),
                                     p("Powered by: ",
                                       a("DarkSky", href = "https://darksky.net/dev"))
                                     ),
                            tabPanel('Twitter Monitor',
                                     br(),
                                     fluidRow(
                                         column(4,
                                                tabBox(
                                                    side = "right", width  = 12, height = NULL,
                                                    selected = "INAMEH",
                                                    tabPanel("INAMEH", style = 'overflow-y: scroll;height:500px;',
                                                             includeHTML("data/emergency-monitor/INAMEH_twt.html")
                                                             ),
                                                    tabPanel("Clima_Litoral", style = 'overflow-y: scroll;height:500px;',
                                                             includeHTML("data/emergency-monitor/Clima_Litoral_twt.html")
                                                             )
                                                    )
                                                ),
                                         column(8,
                                                fluidRow(
                                                    fluidRow(
                                                        column(4,
                                                               textInput('EM_twt_region',
                                                                         'Buscar Tweets en: ',
                                                                         'Caracas',
                                                                         placeholder = 'puede usar el operador OR')
                                                               ),
                                                        column(4,
                                                               textInput('EM_twt_subject',
                                                                         'sobre: ',
                                                                         'lluvia OR tormenta',
                                                                         placeholder = 'puede usar el operador OR')),
                                                        column(3, 
                                                               br(),
                                                               actionBttn(
                                                                   inputId = "EM_twt_go",
                                                                   label = " Go!",
                                                                   color = "primary",
                                                                   style = "jelly",
                                                                   icon = icon("twitter"),
                                                                   block = TRUE)
                                                               ),
                                                        column(1)
                                                    ),
                                                    tabBox(
                                                        side = "left", width  = 12, height = '510px',
                                                        selected = "Time Line",
                                                        tabPanel("Time Line",
                                                                 boxPlus(
                                                                     width = 12,
                                                                     title = "Frecuencia de Tweets", 
                                                                     closable = F, 
                                                                     status = "primary", 
                                                                     solidHeader = F, 
                                                                     collapsible = F,
                                                                     height = NULL,
                                                                     enable_sidebar = TRUE,
                                                                     sidebar_width = 25,
                                                                     sidebar_start_open = F,
                                                                     sidebar_content = tagList(
                                                                         radioButtons('EM_twt_plot_time', 'Escala de agregacion:',
                                                                                      choices = c('1 hora' = '1 hours',
                                                                                                  '3 horas' = '3 hours',
                                                                                                  '12 horas' = '12 hours',
                                                                                                  '1 dia' = '24 hours'),
                                                                                      selected = '3 hours',
                                                                                      inline = F)
                                                                         ),
                                                                     plotlyOutput('EM_twt_lineplot') %>% withSpinner()
                                                                     )
                                                                 )
                                                        )
                                                    )
                                                )
                                         )
                                     )
                            )
                        )
                )
            )
        )
)

# Server----
server <- function(input, output) {
    
    # Docs----
    observeEvent(input$doc, {
        showModal(modalDialog(
            title = "Important message",
            size = 'l',
            HTML(doc),
            easyClose = TRUE
        ))
    })
    
    #Reactives----
    # TPL
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
            dplyr::select(region_before_2016, input$TPL_select_var)
        
        bar_drivAge <- df %>% 
            group_by(drivAge) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T)) %>% 
            dplyr::select(drivAge, value = input$TPL_select_var)
        
        bar_vehBrand <- df %>% 
            group_by(vehBrand) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T)) %>% 
            dplyr::select(vehBrand, value = input$TPL_select_var)
        
        bar_vehAge <- df %>% 
            group_by(vehAge) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T)) %>% 
            dplyr::select(vehAge, value = input$TPL_select_var)        
        
        bar_vehGas <- df %>% 
            group_by(vehGas) %>% 
            summarise(exp = sum(exposure), n_claims = sum(claimN), n_pol = n(), claimAmount = sum(claimAmount, na.rm = T)) %>% 
            dplyr::select(vehGas, value = input$TPL_select_var)
        
        y_labels <- switch(input$TPL_select_var,
                         'exp' = 'Expuestos',
                         'n_claims' = 'Nº Reclamos',
                         'n_pol' = 'Nº Polizas',
                         'claimAmount' = 'Monto de Reclamos'
                         )
        
        list(mapsSelection = mapsSelection, bar_drivAge = bar_drivAge, bar_vehBrand = bar_vehBrand, 
             bar_vehAge = bar_vehAge, bar_vehGas = bar_vehGas, y_labels = y_labels)
        
        
    })
    
    TPL_sim_df <- reactive({
        
        # Code by TyChobra
        
        n <- input$TPL_sim_n
        
        sim_freq <- rnbinom(n, size = 0.8273364, mu = 0.05318064)
        
        sim_claims <- lapply(sim_freq, function(x) rlnorm(x, meanlog = 6.949738, sdlog = 0.6255549))
        
        # Get claims with payments
        aux_sim_list <- lapply(1:length(sim_claims), function(x) {
            if (length(sim_claims[[x]]) > 0) {
                data.frame("sim" = x, "severity" = sim_claims[[x]])
            }
        })
        
        # Convert to df
        sim_claims_df <- bind_rows(aux_sim_list) %>%
            mutate(severity = round(severity, 2))
        
        # Create single limit column
        if (is.na(input$TPL_sim_claim_limit)) {
            single_limit <- sim_claims_df %>%
                mutate(severity_lim = severity)  
        } else {
            single_limit <- sim_claims_df %>%
                mutate(severity_lim = pmin(severity, input$TPL_sim_claim_limit))
        }
        
        # Create agg limit column
        agg_limit <- single_limit %>%
            group_by(sim) %>%
            summarise(n_claims = n(),
                      total_gross = sum(severity),
                      total_net_single = sum(severity_lim))
        
        if (is.na(input$TPL_sim_group_limit)) {
            agg_limit <- agg_limit %>% mutate(total_net_agg = total_net_single)
        } else {
            agg_limit <- agg_limit %>% mutate(total_net_agg = pmin(total_net_single, input$TPL_sim_group_limit))
        }
                
        agg_limit %>%
            mutate(total_ceded = total_gross - total_net_agg)

    })
    
    #iRate
    iRate_country <- reactive({

        complete_df <- i_rate %>% 
            filter(country == input$iRate_select_country)
        
        # Complete df to ts
        complete_df_ts <- ts(complete_df$longt_i, start = complete_df$dt[1], frequency = 12)
        
        df <- complete_df
        
        if(nrow(complete_df) > 500){
            df <- complete_df %>% tail(400)
        }
        
        # df to ts
        df_ts <- ts(df$longt_i, start = df$dt[1], frequency = 12)
        
        # Forecast
        end_train <- df$dt[(round(nrow(df)*0.8))]
        train <- window(df_ts, end = end_train)
        train_lenght <- length(train)
        h <- length(df_ts) - length(train)
        error_mesures <- c('RMSE', 'MAE', 'MPE', 'MAPE')
        .lambda = 0
        
        pred.arima <- forecast(auto.arima(train, seasonal = T, biasadj=F, lambda = .lambda), h = h)
        pred.ets <- forecast(ets(train, lambda = .lambda, biasadj=F), h = h)
        pred.stl <- stlf(train, method = 'ets', lambda = .lambda, h = h, biasadj=F)
        pred.nnar <- forecast(nnetar(train, lambda = .lambda, biasadj=F), h=h)
        pred.tbats <- forecast(tbats(train, lambda = .lambda, biasadj=F), h=h)
        pred.agg <- (pred.ets[["mean"]] + pred.arima[["mean"]] +
                         pred.stl[["mean"]] + pred.nnar[["mean"]] + pred.tbats[["mean"]])/5
        
        inspect.test <- cbind(ETS = accuracy(pred.ets, df_ts)["Test set", error_mesures],
                              ARIMA = accuracy(pred.arima, df_ts)["Test set", error_mesures],
                              `STL-ETS` = accuracy(pred.stl, df_ts)["Test set", error_mesures],
                              NNAR = accuracy(pred.nnar, df_ts)["Test set", error_mesures],
                              TBATS = accuracy(pred.tbats, df_ts)["Test set", error_mesures],
                              Combination = accuracy(pred.agg, df_ts)["Test set", error_mesures]) %>% t()
        
        list(complete_df_ts = complete_df_ts, 
             df_ts = df_ts,
             train_lenght = train_lenght,
             forecasts = list(pred.arima = pred.arima,
                              pred.ets = pred.ets,
                              pred.stl = pred.stl,
                              pred.nnar = pred.nnar,
                              pred.tbats = pred.tbats,
                              pred.agg = pred.agg),
             results = inspect.test)
        
    })
    
    iRate_model <- eventReactive(input$iRate_run, {
        
        model <- switch(input$iRate_select_model,
                        'ARIMA' = auto.arima,
                        'ETS' = ets,
                        'STL' = stlf,
                        'TBATS' = tbats,
                        'NNAR' = nnetar,
                        'Combine' = 'Combine'
                        )
        train_len <- iRate_country()$train_lenght
        
        train <- iRate_country()$df_ts %>% tail(train_len)
        h <- input$iRate_h
        .lambda <- 0
        
        if(is.character(model)){
            pred.arima <- forecast(auto.arima(train, biasadj=F, lambda = .lambda), h = h)
            pred.ets <- forecast(ets(train, lambda = .lambda, biasadj=F), h = h)
            pred.stl <- stlf(train, method = 'ets', lambda = .lambda, h = h, biasadj=F)
            pred.nnar <- forecast(nnetar(train, lambda = .lambda, biasadj=F), h=h)
            pred.tbats <- forecast(tbats(train, lambda = .lambda, biasadj=F), h=h)
            pred <- (pred.ets[["mean"]] + pred.arima[["mean"]] +
                             pred.stl[["mean"]] + pred.nnar[["mean"]] + pred.tbats[["mean"]])/5
            
            pred <- list(mean = pred, method = 'Combine')
            
        } else if(input$iRate_select_model == 'STL'){
            
            pred <- model(train, biasadj=F, lambda = .lambda, h = h)
            
        } else {
            
            pred <- forecast(model(train, biasadj=F, lambda = .lambda), h = h)
            
        }
        
        pred
        
    })
    
    # IBNR
    ibnr_ovw_tri <- reactive({
        
        if(input$ibnr_ovw_cum){
            
            tri <- ibnr_tri 
            
        } else {
            
            tri <- ibnr_tri %>% cum2incr()
            
        }
         
        tri %>% as.data.frame(na.rm = T) %>%
            pivot_wider(names_from = 'dev', values_from = 'value')
        
    })
    
    ibnr_cl <- reactive({
        
        link <- ata(ibnr_tri)
        
        link_tri <- suppressWarnings(link %>% 
            as.triangle() %>%
            as.data.frame(na.rm = T) %>% 
            pivot_wider(names_from = 'dev', values_from = 'value') %>% 
            mutate_if(is.numeric, round, 2)
        )
        
        result <- tibble(vwtd = c(attr(link, "vwtd")),
               smpl =  c(attr(link, "smpl"))) %>%
            mutate_if(is.numeric, round, 2) %>% 
            t() %>% 
            as.data.frame()
        
        linkratios <- c(attr(ata(ibnr_tri), "vwtd"), tail = 1.05)
        LDF <- rev(cumprod(rev(linkratios)))
        names(LDF) <- colnames(ibnr_tri) 
        currentEval <- getLatestCumulative(ibnr_tri)
        EstdUlt <- currentEval * rev(LDF)
        Exhibit <- data.frame(currentEval, LDF = round(rev(LDF), 3), EstdUlt)
        
        list(link_tri = link_tri, result = result, exhibit = Exhibit)
        
    })
    
    ibnr_mack <- reactive({
        
        mack <- MackChainLadder(ibnr_tri, est.sigma = "Mack")
        
        mack_df <- summary(mack)$ByOrigin %>%
            apply(2, round, 2) %>% 
            as.data.frame() %>%
            rownames_to_column('origin')
            
        
        mack_bar <- mack_df %>%
            dplyr::select(origin, Latest, IBNR) %>% 
            pivot_longer(-origin, names_to = 'Ultimate', values_to = 'value') %>% 
            ggplot(aes(x = origin, y = value, fill = Ultimate)) +
            geom_bar(stat = "identity") +
            theme_bw()
        
        mack_resvsfitt <- mack$Models %>% map(function(x){
            stand.residuals <- x$residuals
            fitted.values <- x$fitted.values
            tibble(stand.residuals, fitted.values)
        }) %>%
            do.call(rbind.data.frame, .) %>% 
            ggplot(aes(x = fitted.values, y = stand.residuals)) +
            geom_point() +
            geom_smooth(method = "glm")
        
        mack_resvsori <- mack$Models %>% map(function(x){
            stand.residuals <- x$residuals
            origin <- x$residuals %>% names()
            tibble(stand.residuals, origin)
        }) %>%
            do.call(rbind.data.frame, .) %>%
            ggplot(aes(x = origin, y = stand.residuals)) +
            geom_point() +
            geom_smooth(method = "glm")
        
        # Forecast plot
        
        .aux_mack_ovw <- mack$Triangle %>% 
            as.data.frame(na.rm = T) %>% 
            transmute(origin_dev = paste(origin, dev), type = 'latest')
        
        .error_mack_ovw <- mack$Mack.S.E %>% 
            as.triangle() %>%  
            as.data.frame(na.rm = T) %>%  
            transmute(origin_dev = paste(origin, dev), error = value)
        
        mack_ovw <- mack$FullTriangle %>% 
            as.data.frame(na.rm = T) %>%
            mutate(origin_dev = paste(origin, dev)) %>% 
            left_join(.aux_mack_ovw, by = 'origin_dev') %>%
            left_join(.error_mack_ovw, by = 'origin_dev') %>%
            mutate(type = type %>% replace_na('forecast')) %>% 
            ggplot(aes(x = dev, y = value, color = type)) +
            geom_point() +
            geom_line() +
            geom_errorbar(aes(ymin = value-error, ymax = value+error), width=.2,
                          position=position_dodge(0.05)) +
            facet_wrap( ~ origin, nrow = 3) +
            theme_bw()
        
       list(mack = mack, mack_df = mack_df, mack_bar = mack_bar, mack_resvsfitt = mack_resvsfitt,
             mack_resvsori = mack_resvsori, mack_ovw = mack_ovw)
    })
    
    ibnr_boots <- reactive({
        
        boots <- BootChainLadder(ibnr_tri, R = 999, process.distr = "gamma")
        
        df <- summary(boots)$ByOrigin %>%
            apply(2, round, 2) %>% 
            as.data.frame() %>%
            rownames_to_column('origin')
        
        histo <- boots$IBNR.Totals %>% 
            as.data.frame() %>% 
            setNames('IBNR') %>%
            ggplot(aes(x = IBNR)) +
            geom_histogram() +
            theme_bw()
        
        ecdf <- boots$IBNR.Totals %>%
            as.data.frame() %>% 
            setNames('IBNR') %>%
            ggplot(aes(IBNR)) +
            stat_ecdf(geom = "step") +
            theme_bw()
        
        boots_aux <- boots$Triangle %>% 
            as.data.frame(na.rm = T) %>% 
            pivot_wider(names_from = 'dev', values_from = 'value') %>% 
            transmute(origin, latest = apply(.[,-1], 1, max, na.rm = T)) 
        
        ult <- boots$IBNR.ByOrigin %>% 
            matrix(nrow = length(.[,,1]), byrow = F) %>%
            t() %>% 
            as.data.frame() %>%
            setNames(boots$Triangle %>% as.data.frame() %>% .$origin %>% unique()) %>% 
            pivot_longer(everything(), names_to = 'origin', values_to = 'value') %>% 
            left_join(boots_aux, by = 'origin') %>%
            mutate(value = value + latest) %>%
            ggplot(aes(x = origin, y = value)) +
            geom_boxplot()
        
       ibnr <-  boots$IBNR.ByOrigin %>% 
            matrix(nrow = length(.[,,1]), byrow = F) %>%
            t() %>% 
            as.data.frame() %>%
            setNames(boots$Triangle %>% as.data.frame() %>% .$origin %>% unique()) %>% 
            pivot_longer(everything(), names_to = 'origin', values_to = 'value') %>% 
            ggplot(aes(x = origin, y = value)) +
            geom_boxplot()
         
        q <- quantile(boots, c(0.75,0.95,0.99, 0.995))$ByOrigin %>%
            apply(2, round, 2) %>% 
            as.data.frame() %>%
            rownames_to_column('origin')
        
        list(df = df, histo = histo, ecdf = ecdf, ult = ult, ibnr = ibnr, q = q)
        
    })
    
    # EM
    EM_weather <- reactive({
        
        df <- EM_states %>% filter(state == input$EM_state)
        
        url <- paste0('https://api.darksky.net/forecast/',
                      EM_darksy_key, '/',
                      df$lat, ',',
                      df$long,
                      '?lang=es&units=si')

        apicall <- fromJSON(url)
        
        # validate(
        #     need(!is.na(apicall), "No winning trades")
        # )
        
        curr <- apicall$currently
        hour <- apicall$hourly$data %>% 
            mutate(time = as_datetime(time, tz = 'America/Caracas'))
        day <- apicall$daily$data %>% 
            mutate(time = as_datetime(time, tz = 'America/Caracas'))
        
        sbt_hour <- table(hour$summary) %>% which.max() %>% names()
        
        hourly_chart <- highchart() %>%
            hc_title(text = 'Pronóstico de lluvia a 48h', align = 'left') %>% 
            hc_subtitle(text = sbt_hour, align = 'left') %>%
            hc_xAxis(categories = hour(hour$time)) %>% 
            hc_yAxis_multiples(
                list(title = list(text = "Prob"), lineWidth = 3, ceiling = 1),
                list(title = list(text = "Ml/Hora"), showLastLabel = T, opposite = T)
            ) %>% 
            hc_add_series(hour$precipProbability, type = "line", yAxis = 0, name = 'Probabilidad') %>% 
            hc_add_series(hour$precipIntensity, type = "column", yAxis = 1, name = 'Intensidad') %>% 
            hc_exporting(
                enabled = TRUE
            )
        
        sbt_day <- table(day$summary) %>% which.max() %>% names()
        
        daily_chart <- highchart() %>%
            hc_title(text = 'Pronóstico de lluvia a 8d', align = 'left') %>% 
            hc_subtitle(text = sbt_day, align = 'left') %>%
            hc_xAxis(categories = date(day$time)) %>% 
            hc_yAxis_multiples(
                list(title = list(text = "Prob"), lineWidth = 3, ceiling = 1),
                list(title = list(text = "Ml/Hora"), showLastLabel = T, opposite = T)
            ) %>% 
            hc_add_series(day$precipProbability, type = "line", yAxis = 0, name = 'Probabilidad') %>% 
            hc_add_series(day$precipIntensity, type = "column", yAxis = 1, name = 'Intensidad')  %>% 
            hc_exporting(
                enabled = TRUE
            )
        
        list(state = df, curr = curr, hourly_chart = hourly_chart, daily_chart = daily_chart)
        
    })
    
    EM_twt_rest <- eventReactive(input$EM_twt_go, {
        
        query <- paste(input$EM_twt_region, input$EM_twt_subject, sep = ' ')
        
        twt <- tryCatch({
            search_tweets(query, n = 2000, token = EM_token, include_rts = FALSE) %>% 
                mutate(created_at = created_at - hours(4))
            },
            error = function(cond){
                message('Conection Error')
                return(FALSE)
                })
        
        shiny::validate(
            need(is.data.frame(twt), "Error de conexión, expere 10 segundos e intente nuevamente")
        )
        
        list(twt = twt)
        
    })
    
    # Value & info Boxes----
        # TPL
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
    
        # EM
    output$EM_curr_exp <- renderValueBox({
        
        exposure <- EM_weather()$state$exp
        
        valueBox('Expuestos', value = format(round(exposure, 0), big.mark = ',', nsmal = 0),
                 icon = icon('signal', lib = 'font-awesome'), color = 'green')
        
    })
    
    output$EM_curr_SA <- renderValueBox({
        
        sa <- EM_weather()$state$sa
        
        valueBox('Exposición', value = format(round(sa, 0), big.mark = ',', nsmal = 0),
                 icon = icon('dollar-sign', lib = 'font-awesome'), color = 'green')
        
    })
    
    output$EM_curr_prob <- renderInfoBox({
        
        p <- EM_weather()$curr$precipProbability
        
        valueBox('Prob. de Precipitación', value = sprintf("%s%%", format(round(p*100, 0), big.mark = ',', nsmal = 0)),
                icon = icon('umbrella', lib = 'font-awesome'))
        
    })
    
    output$EM_curr_rain <- renderInfoBox({
        
        int <- round(EM_weather()$curr$precipIntensity, 2)
        s <- EM_weather()$curr$summary
        
        valueBox(value = sprintf("%s Ml/hora", format(int, big.mark = ',', nsmal = 2)),
                subtitle = 'Precipitación',
                icon = icon('cloud', lib = 'font-awesome'))
        
    })
    
    output$EM_curr_wind <- renderInfoBox({
        
        w <- EM_weather()$curr$windSpeed

        valueBox('Velocidad del Viento', value = sprintf("%s Mt/seg", format(w, big.mark = ',', nsmal = 2)),
                icon = icon('bolt', lib = 'font-awesome'))
        
    })    
    
    output$EM_curr_temp <- renderInfoBox({
        
        temp <- EM_weather()$curr$temperature

        valueBox('Temperatura', value = sprintf("%s Cº", format(temp, big.mark = ',', nsmal = 2)),
                icon = icon('sun', lib = 'font-awesome'))
        
    })
    
    # Plots----
    # TPL
    output$TPL_bar_drivAge <- renderHighchart({
        
        l <- TPL_list()$bar_drivAge
        
        hchart(l, "column", hcaes(x = drivAge, y = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Edad del Conductor") %>% 
            hc_yAxis(title = list(text = TPL_list()$y_labels)) %>% 
            hc_exporting(
                enabled = TRUE
            )
    })
    
    output$TPL_bar_vehBrand <- renderHighchart({
        
        l <- TPL_list()$bar_vehBrand
        
        hchart(l, "treemap", hcaes(x = vehBrand, value = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Marca de Vehiculo") %>% 
            hc_exporting(
                enabled = TRUE
            )
    })
    
    output$TPL_bar_vehGas <- renderHighchart({
        
        l <- TPL_list()$bar_vehGas
        
        hchart(l, "column", hcaes(x = vehGas, y = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Tipo de Vehiculo") %>% 
            hc_yAxis(title = list(text = TPL_list()$y_labels)) %>% 
            hc_exporting(
                enabled = TRUE
            )
    })
    
    output$TPL_bar_vehAge <- renderHighchart({
        
        l <- TPL_list()$bar_vehAge
        
        hchart(l, "column", hcaes(x = vehAge, y = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Año de Vehiculo") %>% 
            hc_yAxis(title = list(text = TPL_list()$y_labels)) %>% 
            hc_exporting(
                enabled = TRUE
            )
    })
    
    output$TPL_map <- renderHighchart({
        
        l <- TPL_list()$mapsSelection
        
        highchart() %>% 
            hc_add_series_map(french_map, l, value = input$TPL_select_var,
                              joinBy = c("name", "region_before_2016"), name = TPL_list()$y_labels,
                              dataLabels = list(enabled = TRUE, format = '{point.name}'),
                              borderColor = "#FAFAFA", borderWidth = 0.1,
                              tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = "")) %>%
            hc_title(text = "Distribucion de siniestros por Region") %>% 
            hc_exporting(
                enabled = TRUE
            )
        
    })
    
    output$TPL_histo_sim_net <- renderHighchart({
        
        df <- TPL_sim_df()
        
        hist(df$total_net_agg, breaks = 'Sturges', plot = F) %>% 
            hchart() %>% 
            hc_title(text = 'Perdidas Retenidas: Netas de Reaseguro') %>% 
            hc_legend(enabled = FALSE) %>% 
            hc_xAxis(title = list(text = 'Perdidas Netas')) %>% 
            hc_yAxis(title = list(text = 'Numero de Observaciones'))
        
    })
    
    output$TPL_histo_sim_gross <- renderHighchart({
        
        df <- TPL_sim_df()
        
        hist(df$total_gross, breaks = 'Scott', plot = F) %>% 
            hchart() %>% 
            hc_title(text = 'Perdidas Totales: Retenido + Cedido') %>% 
            hc_legend(enabled = FALSE) %>% 
            hc_xAxis(title = list(text = 'Perdidas Brutas')) %>% 
            hc_yAxis(title = list(text = 'Numero de Observaciones'))
    })
    
    output$TPL_histo_sim_ceded <- renderHighchart({
        
        df <- TPL_sim_df()
        
        hist(df$total_ceded, breaks = 'Sturges', plot = F) %>% 
            hchart() %>% 
            hc_title(text = 'Perdidas Cedidas: Exceso de Cobertura') %>% 
            hc_legend(enabled = FALSE) %>% 
            hc_xAxis(title = list(text = 'Perdidas Cedidas')) %>% 
            hc_yAxis(title = list(text = 'Numero de Observaciones'))
        
    })
    
    # iRate
    output$iRate_heatmap <- renderHighchart({
        
        hm_plot <- i_rate %>%
            transmute(dt,
                      longt_i, 
                      country, 
                      year = year(dt),
                      quarter = as.yearqtr(paste0(year, '-', lubridate::quarter(dt)))) %>%
            # filter(year > 1970) %>% 
            group_by(country, quarter) %>% 
            summarise(longt_i = last(longt_i))
        
        hchart(hm_plot, "heatmap", hcaes(x = quarter, y = country, value = longt_i)) %>% 
            hc_title(text = "Quarter Long Term Interest Rate") %>% 
            hc_legend(layout = "vertical", verticalAlign = "top",
                      align = "right", valueDecimals = 0)
        
    })
    
    output$iRate_stl <- renderHighchart({
        
        exploratory <- iRate_country()$df_ts
        
        # STL diagnosis
        stl(exploratory, "per") %>%
            hchart() %>% 
            hc_title(text = 'Seasonal Trend LOESS Decomposition')
        
    })
    
    output$iRate_diff_plot <- renderHighchart({
        
        exploratory <- iRate_country()$df_ts
        
        # Diff Plot
        hchart(diff(exploratory)) %>% 
            hc_title(text = 'Diff (1)') %>% 
            hc_legend(enabled = FALSE)

        
    }) 
    
    output$iRate_ACF <- renderHighchart({
        
        exploratory <- iRate_country()$df_ts
        
        # ACF & PACF
        acf(diff(exploratory), plot = F) %>% 
            hchart() %>% 
            hc_title(text = 'ACF') %>% 
            hc_legend(enabled = FALSE)
        
    })
    
    output$iRate_PACF <- renderHighchart({
        
        exploratory <- iRate_country()$df_ts
        
        # ACF & PACF
        pacf(diff(exploratory), plot = F) %>% 
            hchart() %>%
            hc_title(text = 'PACF') %>% 
            hc_legend(enabled = FALSE)
        
    })
    
    output$iRate_agg_forecast <- renderHighchart({
        
        d <- iRate_country()$df_ts
        
        l <- iRate_country()$forecasts
        
        d %>% hchart() %>% 
            hc_add_series(l$pred.arima$mean, name = l$pred.arima$method) %>% 
            hc_add_series(l$pred.ets$mean, name = l$pred.ets$method) %>% 
            hc_add_series(l$pred.tbats$mean, name = l$pred.tbats$method) %>% 
            hc_add_series(l$pred.nnar$mean, name = l$pred.nnar$method) %>% 
            hc_add_series(l$pred.stl, name = l$pred.stl$method) %>% 
            hc_add_series(l$pred.agg, name = 'Combination') %>% 
            hc_title(text = 'Combine Forecast')
        
    })   
    
    output$iRate_indv_forecast <- renderHighchart({
        d <- iRate_country()$df_ts
        l <- iRate_model()
        
        d %>% hchart() %>% 
            hc_add_series(l$mean, name = l$method)  %>% 
            hc_title(text = 'Forecast')
        
    })
    
    # IBNR
    output$ibnr_ovw_plot <- renderPlotly({
        
        ovw <- ibnr_tri %>% as.data.frame(na.rm = T) %>% 
            ggplot(aes(x = dev, y = value)) +
            geom_point() +
            geom_line() +
            facet_wrap( ~ origin, nrow = 3) +
            theme_bw()
        
        ggplotly(ovw) %>% 
            config(displayModeBar = F)    
        
    })
    
    output$ibnr_mack_bar <- renderPlotly({
        
        p <- ibnr_mack()$mack_bar
        
        ggplotly(p) %>% 
            config(displayModeBar = F)
        
    })
    
    output$ibnr_mack_resvfit <- renderPlotly({
        
        p <- ibnr_mack()$mack_resvsfitt
        
        ggplotly(p) %>% 
            config(displayModeBar = F)
        
        
    })
    
    output$ibnr_mack_resvsori <- renderPlotly({
        
        l <- ibnr_mack()
        
        ggplotly(l$mack_resvsori) %>% 
            config(displayModeBar = F)
        
    })
    
    output$ibnr_mack_ovw <- renderPlotly({
        
        p <- ibnr_mack()$mack_ovw
        
        ggplotly(p) %>% 
            config(displayModeBar = F)
        
    })
    
    output$ibnr_boots_histo <- renderPlotly({
        
        p <- ibnr_boots()$histo
        
        ggplotly(p) %>% 
            config(displayModeBar = F)
        
    })
    
    output$ibnr_boots_ecdf <- renderPlot({
        
        ibnr_boots()$ecdf
        
    })
    
    output$ibnr_boots_ult <- renderPlotly({
        
        p <- ibnr_boots()$ult
        
        ggplotly(p) %>% 
            config(displayModeBar = F)
        
    })
    
    output$ibnr_boots_ibnr <- renderPlotly({
        
        p <- ibnr_boots()$ibnr
        
        ggplotly(p) %>% 
            config(displayModeBar = F)
        
    })
    
    # EM
    output$EM_hour_rain_plot <- renderHighchart({
        
        EM_weather()$hourly_chart
    })
    
    output$EM_day_rain_plot <- renderHighchart({
        
        EM_weather()$daily_chart
    })
    
    output$EM_twt_lineplot <- renderPlotly({
        
        t <- EM_twt_rest()$twt 
        
        p <- t %>% 
            ts_plot(input$EM_twt_plot_time,  trim = 1) +
            theme_minimal() +
            theme(plot.title = ggplot2::element_text(face = "bold")) +
            labs(
                x = NULL, y = NULL,
                caption = "\nFuente: Twitter's REST API via rtweet"
            )
        
        ggplotly(p)
    })
    
    # DataTables-----
    # Detail Table TPL
    output$TPL_table <- DT::renderDataTable(

        DT::datatable({out_table},
                      rownames = T,
                      filter = 'top',
                      options = list(searching = T,
                                     pageLength = 10,
                                     info = T,
                                     scrollX = TRUE,
                                     columnDefs = list(list(className = 'dt-center', targets = '_all'))
                      )
        )
    )
    
    # iRate comparative forecast results
    output$iRate_comparative_table <- DT::renderDataTable(
        DT::datatable({iRate_country()$results},
                      options = list(dom = 't',
                                     columnDefs = list(list(className = 'dt-center', targets = '_all'))
                      )
        )
    )
    
    # Overview Triangle
    output$ibnr_ovw_tri_table <- DT::renderDataTable(
        DT::datatable({ibnr_ovw_tri()},
                      options = list(dom = 't',
                                     columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                     ordering = F,
                                     pageLength = nrow(ibnr_ovw_tri())
                      ),
                      rownames = FALSE
        )
    )
    
    # Mack Result
    output$ibnr_mack_result <- DT::renderDataTable(
        DT::datatable({ibnr_mack()$mack_df},
                      options = list(dom = 't',
                                     columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                     ordering = F,
                                     pageLength = nrow(ibnr_mack()$mack_df)
                      ),
                      rownames = FALSE
        )
    )
        
    # Boots Result
    output$ibnr_boots_result <- DT::renderDataTable(
        DT::datatable({ibnr_boots()$df},
                      options = list(dom = 't',
                                     columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                     ordering = F,
                                     pageLength = nrow(ibnr_boots()$df)
                      ),
                      rownames = FALSE
        )
    )        

        # Boots Q
    output$ibnr_boots_q <- DT::renderDataTable(
        DT::datatable({ibnr_boots()$q},
                      options = list(dom = 't',
                                     columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                     ordering = F,
                                     pageLength = nrow(ibnr_boots()$q)
                      ),
                      rownames = FALSE
        )
    )
    
    # CL triangle
    output$ibnr_cl_tri <- DT::renderDataTable(
        DT::datatable({ibnr_cl()$link_tri},
                      options = list(dom = 't',
                                     columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                     ordering = F,
                                     pageLength = nrow(ibnr_cl()$link_tri)
                      ),
                      rownames = FALSE
        )
    )

    # CL results
    output$ibnr_cl_tri_result <- DT::renderDataTable(
        DT::datatable({ibnr_cl()$result},
                      options = list(dom = 't',
                                     columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                     ordering = F,
                                     pageLength = nrow(ibnr_cl()$result)
                      ),
                      rownames = FALSE,
                      colnames = rep("", ncol(ibnr_cl()$result))
        )
    )
    
    # CL results
    output$ibnr_cl_result <- DT::renderDataTable(
        DT::datatable({ibnr_cl()$exhibit},
                      options = list(dom = 't',
                                     columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                     ordering = F,
                                     pageLength = nrow(ibnr_cl()$exhibit)
                      ),
                      rownames = FALSE
        )
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
