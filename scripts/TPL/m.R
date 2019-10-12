library(tidyverse)
library(lubridate)
library(naniar)
library(CASdatasets)
library(ROCR)
library(caret)

# Data----
data(freMTPL2freq)
data(freMTPL2sev)

freq <- freMTPL2freq
sev <- freMTPL2sev

# Inspect & PreProcess----
freq2 %>% gg_miss_var()
summary(freq)

freq$ClaimNb <- pmin(freq$ClaimNb, 4)
freq$Exposure <- pmin(freq$Exposure, 1)

freq2 <- freq %>% 
  transmute(IDpol,
            area = as.integer(Area),
            vehPower = as.factor(pmin(VehPower, 9)),
            vehAge = case_when(VehAge == 0 ~ 1,
                               VehAge < 10 ~ 2,
                               T ~ 3) %>% as.factor() %>% relevel(ref = 2),
            drivAge = cut(DrivAge, c(18,21,26,31,41,51, 71, 101), right = F) %>% 
              as.factor() %>% relevel(ref = 5),
            bonusMalus = as.integer(pmin(BonusMalus, 150)),
            density = log(Density),
            region = relevel(Region, ref = 'R24'),
            vehGas = as.factor(VehGas),
            vehBrand = VehBrand,
            exposure = Exposure,
            claimN = ClaimNb
  )

## severity

##poisson
summary(model.frequency_p <-glm(claimN ~vehPower + vehAge + drivAge + bonusMalus + vehBrand + 
                                  vehGas + density + region,
                                data=freq2, family=poisson)) 
with(model.frequency_p, cbind(res.deviance = deviance, df =
                                df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE))) ##prueba de pearson

##negative binomial
summary(model.frequency_nb <-glm.nb(claimN ~vehPower + vehAge + drivAge + bonusMalus + vehBrand + 
                                      vehGas + density + region,
                                    data=freq2)) ## aplicamos binomial negativa

pchisq(2*(logLik(model.frequency_nb)-logLik(model.frequency_p)),
       df=1, lower.tail= FALSE)

with(model.frequency_nb, cbind(res.deviance = deviance, df =
                                 df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE))) ##prueba de pearson 

## severity
s <- 10000
sev2 <- left_join(sev, freq2, by = 'IDpol') %>% 
  mutate(standard = as.factor(ClaimAmount < s)) %>% 
  na.omit()

set.seed(171)
ll_s <- sample(c(1:nrow(sev2)), round(0.9*nrow(sev2)), replace = FALSE)
learn_s <- sev2[ll_s,]
test_s <- sev2[-ll_s,]
(n_l_s <- nrow(learn_s))
(n_t_s <- nrow(test_s))
indx_sev <- which(learn_s$standard == T)

##log gamma
summary(model.severity_g <- glm(ClaimAmount ~vehPower + vehAge + drivAge + bonusMalus + vehBrand + 
                                  vehGas + density + region,
                                data=learn_s[indx_sev,], family = Gamma("log"))) ##aplicamos modelo gamma con link = log


##relativities
rel <- data.frame(rating.factor =
                    c(rep("Vehicle Power", nlevels(freq2$vehPower)), rep("Vehicle Age",
                                                                         nlevels(freq2$vehAge)),
                      rep("Divers Age", nlevels(freq2$drivAge)), rep("Bonus Malus", nlevels(freq2$bonusMalus)),
                      rep("Vehicle Brand", nlevels(freq2$vehBrand)), rep("Vehicle Gas", nlevels(freq2$vehGas)),
                      rep("Density", nlevels(freq2$density)), rep("Region", nlevels(freq2$region))),
                  class = c(levels(freq2$vehPower),levels(freq2$vehAge), levels(freq2$drivAge),
                            levels(freq2$bonusMalus), levels(freq2$vehBrand), levels(freq2$vehGas),
                            levels(freq2$density), levels(freq2$region)),
                  stringsAsFactors = FALSE)

print(rel)


rels <- coef(model.frequency_p)
rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )
rel$rels.frequency <- c(c(1, rels[1:5]), c(rels[6], 1, rels[7]), c(rels[8:11], 1, rels[12:13]),
                        c(rels[14]), c(rels[15:24]), c(1, rels[25]), c(rels[26]), c(rels[27:47]))
rels <- coef(model.severity_g)
rels <- exp(rels[1] + rels[-1])/exp(rels[1])
rel$rels.severity <- c(c(1, rels[1:5]), c(rels[6], 1, rels[7]), c(rels[8:11], 1, rels[12:13]),
                       c(rels[14]), c(rels[15:24]), c(1, rels[25]), c(rels[26]), c(rels[27:47]))
rel$rels.pure.premium <- with(rel, rels.frequency * rels.severity)
