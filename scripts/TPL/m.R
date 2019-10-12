# Libraries----
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

# Frequency Model----
# Split Learn-Test
set.seed(100)
ll <- sample(c(1:nrow(freq2)), round(0.9*nrow(freq2)), replace = FALSE)
learn <- freq2[ll,]
test <- freq2[-ll,]
(n_l <- nrow(learn))
(n_t <- nrow(test))

glm1 <- glm(claimN ~ vehPower + vehAge + drivAge + bonusMalus + vehBrand + 
              vehGas + density + region, 
            data = learn, offset = log(exposure), family = poisson())

summary(glm1)

# Evaluate
Poisson.Deviance <- function(pred, obs){
  2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
}

Gamma.Deviance <- function(pred, obs){
  2*((sum(pred)-sum(obs)/sum(obs))-sum(log((obs/pred)^(obs))))/length(pred)
}


learn$fitGLM <- fitted(glm1)
test$fitGLM <- predict(glm1, newdata=test, type="response")

# in-sample and out-of-sample losses (in 10^(-2))
(insampleGLM <- 100*Poisson.Deviance(learn$fitGLM, learn$claimN))
100*Poisson.Deviance(test$fitGLM, test$claimN)

# Severity Model----
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

logReg <- glm(standard ~ drivAge, data = learn_s, family = binomial)
summary(logReg)
x <- predict(logReg, learn_s, type = "response")
pred <- prediction(x, learn_s$standard)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

confusionMatrix(data = ifelse(x < 0.982, T, F) %>% as.factor(),
                reference = learn_s$standard)

indx_sev <- which(learn_s$standard == T)
gam1 <- glm(ClaimAmount ~ drivAge + density, data = learn_s[indx_sev,],
            family = Gamma(link = "log"))

gam2 <- glm(ClaimAmount ~ drivAge + density, data = learn_s[-indx_sev,],
            family = Gamma(link = "log"))

summary(gam1)
summary(gam2)

A <- predict(gam1, newdata = test_s, type = "response")
B <- predict(gam2, newdata = test_s, type = "response")
C <- predict(logReg, newdata = test_s, type = "response")

sev_test <- A*C+B*(1-C)

eval <- cbind(claimA = test_s$ClaimAmount, sev_test) %>% as.data.frame()
  

learn_s$fitGLM <- fitted(glm1)
test$fitGLM <- predict(glm1, newdata=test, type="response")

# in-sample and out-of-sample losses (in 10^(-2))
(insampleGLM <- 100*Poisson.Deviance(learn$fitGLM, learn$claimN))
100*Poisson.Deviance(test$fitGLM, test$claimN)
