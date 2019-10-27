library(tidyverse)
# library(CASdatasets)

# French Maps
french_map <- jsonlite::fromJSON('data/french-motor-TPL/fr-all.geo.json', simplifyVector = F)
map_division <- read_csv('data/french-motor-TPL/departments.csv')

# Remove outliers
freq$ClaimNb <- pmin(freq$ClaimNb, 4)
freq$Exposure <- pmin(freq$Exposure, 1)

# Group severity by IDpol
group_sev <- sev %>% 
  group_by(IDpol) %>% 
  summarise(claimAmount = sum(ClaimAmount))

# Mutates & Joins
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

# Sample
sample_indx <- sample(1:nrow(TPL_claims), 200000, replace = F)

TPL_claims[sample_indx,] %>% write.csv('data/french-motor-TPL/claims.csv', row.names = F)
