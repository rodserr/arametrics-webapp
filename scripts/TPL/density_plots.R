# x <- TPL_claims %>% na.omit()
# quantile(x$claimAmount, .99)
# x <- x %>% filter(claimAmount < 5000)
# 
# tapply(x$claimAmount, x$vehGas, density) %>%
#     reduce(.f = hc_add_series, .init = highchart(), type = 'area') %>% 
#     hc_title(text = "Tipo de Vehiculo") %>% 
#     hc_xAxis(min = 0)