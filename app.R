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
library(DT)
library(zoo)
library(xts)
library(forecast)

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
                                 T ~ '>=10 años'),
              drivAge = cut(DrivAge, c(18,21,26,31,41,51, 71, 101), right = F) %>% as.character(),
              bonusMalus = as.integer(pmin(BonusMalus, 150)),
              density = Density,
              vehGas = VehGas,
              vehBrand = VehBrand %>% as.character(),
              exposure = Exposure,
              claimN = ClaimNb,
              region_id = str_remove(Region, 'R')
    ) %>% 
    left_join(map_division %>% dplyr::select(n, department, region_before_2016), by = c('region_id' = 'n')) %>% 
    left_join(group_sev, by = 'IDpol')

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

    # ts-InterestRate----
i_rate <- read_csv('data/ts-interest-rate/long_term_interest_rates.csv') %>% 
    mutate(dt = as.yearmon(time))

iRate_country_choices <- unique(i_rate$country)

# UI----
ui <- shinyUI(
    dashboardPage(
        # Application title
        dashboardHeader(title = 'Arametrics Dashboard'),
        
        # Sidebar----
        dashboardSidebar(
            tags$a(
                   tags$img(src = "am-logo-white.png", height = 100, width = 200)),
            br(),
            br(),
            
            sidebarMenu(
                # Setting id makes input$tabs give the tabName of currently-selected tab
                id = "tabs",
                
                menuItem("Motor Portfolio TPL", tabName = "TPL_motor", icon = icon("bar-chart-o")),
                menuItem("Long-Term IRate Forecast", tabName = "iRate", icon = icon("th"))
            )
            
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
                                     fluidRow(
                                         highchartOutput("iRate_heatmap", height = '500px') %>% withSpinner()
                                         ),
                                     fluidRow( 
                                         br(),
                                         highchartOutput("iRate_stl") %>% withSpinner()   
                                         ),
                                     fluidRow(
                                         br(),
                                         highchartOutput("iRate_diff_plot") %>% withSpinner(),
                                         
                                         fluidRow(
                                             br(),
                                             column(6,
                                                    highchartOutput("iRate_ACF") %>% withSpinner()
                                                    ),
                                             column(6,
                                                    highchartOutput("iRate_PACF") %>% withSpinner()
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
                                # box(highchartOutput("iRate_inv_forecast") %>% withSpinner(), width = 12)
                                highchartOutput('iRate_indv_forecast') %>% withSpinner()
                            )
                        )
                    
                )
            )

        )
    )
)

# Server----
server <- function(input, output) {
    
    #Reactives----
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
    
    # ValueBox----
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
    
    # Plots----
    # TPL
    output$TPL_bar_drivAge <- renderHighchart({
        
        l <- TPL_list()$bar_drivAge
        
        hchart(l, "column", hcaes(x = drivAge, y = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Edad del Conductor") %>% 
            hc_yAxis(title = list(text = TPL_list()$y_labels))
    })
    
    output$TPL_bar_vehBrand <- renderHighchart({
        
        l <- TPL_list()$bar_vehBrand
        
        hchart(l, "treemap", hcaes(x = vehBrand, value = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Marca de Vehiculo")
    })
    
    output$TPL_bar_vehGas <- renderHighchart({
        
        l <- TPL_list()$bar_vehGas
        
        hchart(l, "column", hcaes(x = vehGas, y = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Tipo de Vehiculo") %>% 
            hc_yAxis(title = list(text = TPL_list()$y_labels))
    })
    
    output$TPL_bar_vehAge <- renderHighchart({
        
        l <- TPL_list()$bar_vehAge
        
        hchart(l, "column", hcaes(x = vehAge, y = value)) %>% 
            hc_xAxis(title = '') %>%
            hc_title(text = "Año de Vehiculo") %>% 
            hc_yAxis(title = list(text = TPL_list()$y_labels))
    })
    
    output$TPL_map <- renderHighchart({
        
        l <- TPL_list()$mapsSelection
        
        highchart() %>% 
            hc_add_series_map(french_map, l, value = input$TPL_select_var,
                              joinBy = c("name", "region_before_2016"), name = TPL_list()$y_labels,
                              dataLabels = list(enabled = TRUE, format = '{point.name}'),
                              borderColor = "#FAFAFA", borderWidth = 0.1,
                              tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = "")) %>%
            hc_title(text = "Distribucion de siniestros por Region")
        
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
    
    output$iRate_comparative_table <- DT::renderDataTable(
        DT::datatable({iRate_country()$results},
                      options = list(dom = 't',
                                     columnDefs = list(list(className = 'dt-center', targets = '_all'))
                      )
        )
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
