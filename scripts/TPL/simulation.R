# Libraries----
library(tidyverse)
library(lubridate)
library(MASS)
library(fitdistrplus)
# library(CASdatasets)
library(actuar)
library(highcharter)

# Data----
data(freMTPL2freq)
data(freMTPL2sev)

freq <- freMTPL2freq
sev <- freMTPL2sev

# PreProcess & fit----
# Remove extreme claims amounts 
(tresh_amount <- quantile(sev$ClaimAmount, .99))
claimAmountsScale <- sev %>% filter(ClaimAmount < tresh_amount) %>% dplyr::select(ClaimAmount) %>% pull()

# Group claim numbers >4 to 4
claimFreqScale <- as.numeric(freq$ClaimNb)
claimFreqScale <- pmin(claimFreqScale, 4)
table(claimFreqScale)

# Fit Parameters
# Frequency
par_freq <- fitdist(claimFreqScale, 'nbinom', method = 'mle')
par_freq_min <- mde(claimFreqScale, pnbinom, start = list(size = par_freq$estimate[1], mu = par_freq$estimate[2]),
                   measure = 'CvM')

# Severity
par_sev <- fitdist(claimAmountsScale, 'lnorm', method = 'mle')
par_sev_min <- mde(claimAmountsScale, plnorm, start = list(meanlog = par_sev$estimate[1], sdlog = par_sev$estimate[2]),
                      measure = 'CvM')

# Simulation----
# Get Simulation
n <- 200000

set.seed(123)
table(sim_freq <- rnbinom(n, size = par_freq_min$estimate[1],
                    mu = par_freq_min$estimate[2]))

summary(sim_sev <- rlnorm(n, meanlog = par_sev_min$estimate[1], sdlog = par_sev_min$estimate[2]))

sim_claims <- lapply(sim_freq, function(x) rlnorm(x, meanlog = par_sev_min$estimate[1], sdlog = par_sev_min$estimate[2]))

aux_sim <- lapply(1:length(sim_claims), function(x) {
  if (length(sim_claims[[x]]) > 0) {
    data.frame("sim" = x, "severity" = sim_claims[[x]])
  }
})

sim_claims_df <- bind_rows(aux_sim) %>%
  mutate(severity = round(severity, 2))
  

ind_limit <- 1000
pol_limit <- 3000

if (is.na(ind_limit)) {
  single_limit <- sim_claims_df %>%
    mutate(severity_lim = severity)  
} else {
  single_limit <- sim_claims_df %>%
    mutate(severity_lim = pmin(severity, ind_limit))
}

agg_limit <- single_limit %>%
  group_by(sim) %>%
  summarise(n_claims = n(),
            total_gross = sum(severity),
            total_net_single = sum(severity_lim))

if (is.na(pol_limit)) {
  agg_limit <- agg_limit %>% mutate(total_net_agg = total_net_single)
} else {
  agg_limit <- agg_limit %>% mutate(total_net_agg = pmin(total_net_single, pol_limit))
}

sim_claims_final <- agg_limit %>%
  mutate(total_ceded = total_gross - total_net_agg)

# Plots----
hist(sim_claims_final$total_gross, breaks = 'Sturges', plot = F) %>% hchart()
hist(sim_claims_final$total_net_agg, breaks = 'Sturges', plot = F) %>% hchart()
hist(sim_claims_final$total_ceded, breaks = 'Sturges', plot = F) %>% hchart()
