# Comparison of 30% reduction in the original scale and log scale
# 30% reduction in log scase
library(tidyverse)
library(broom)

<<<<<<< HEAD
mean = 3.6
reduction = .3
sd = .66
nt = 12 # Use some even integer
nc = 4

reductioncomp <- function(mean, reduction, sd, nt = 50, nc = 50){
  trmt <- as.factor(c(rep("T", 2*nt), rep("C", 2*nc)))
  time <- as.factor(c( rep("T0", nt), rep("T1", nt), rep("T0", nc), rep("T1", nc) ))
  scase <- c( rnorm(nt, mean, sd), rnorm(nt, mean*(1-reduction), sd), rnorm(2*nc, mean, sd))
  dat <- tibble(treatment = trmt, time = time, y = scase) %>% mutate(yoriginal = exp(y))
  fit <- lm(y ~ treatment*time, data = dat)
  estimates<- summary(fit)$coefficients
  model1 <- tidy(fit)
  vcov <- vcov(fit)[4,4]
  xxinv <-solve(t(model.matrix(fit)) %*%model.matrix(fit))[4,4]
  varchg_true <- sd^2 * xxinv
  mean = dat %>% group_by(treatment, time) %>% dplyr::summarise(mean = median(yoriginal)) %>%
  with(., as.vector(mean))
  meanl =   dat %>% group_by(treatment, time) %>% dplyr::summarise(mean = mean(y)) %>%
    with(., as.vector(mean))
  meanC = 1- mean[2]/mean[1]
  meanT = 1- mean[4]/mean[3]
  meanCL = 1- meanl[2]/meanl[1]
  meanTL = 1- meanl[4]/meanl[3]
  changeLlog <- exp(model1 %>% filter(term == "treatmentT:timeT1") %>% select(estimate))[1,1]
  return(list(meanC = meanC, meanT = meanT, meanCL = meanCL, menaTL = meanTL, GMratio = changeLlog, varchg=vcov, median = mean, meanl = meanl,
              xxinv = xxinv, varchg_true = varchg_true, estimate = estimates, data=dat))
=======
mean = 4.6
reduction = 0.3
sd = 0.44
nt = 50 # Use some even integer
nc = 50

reductioncomp <- function(mean, reduction, sd){
  trmt <- as.factor(c(rep("T", nt), rep("C", nc)))
  time <- as.factor(c( rep("T0", nt/2), rep("T1", nt/2), rep("T0", nc/2), rep("T1", nc/2) ))
  scase <- c( rnorm(nt/2, mean, sd), rnorm(nt/2, mean*(1-reduction), sd), rnorm(nc, mean, sd))
  dat <- tibble(treatment = trmt, time = time, y = scase) %>% mutate(yoriginal = exp(y))
  model1 <- tidy(lm(y ~ treatment*time, data = dat))
  mean = dat %>% group_by(treatment, time) %>% summarise(mean = mean(yoriginal)) %>%
  with(., as.vector(mean))
  meanC = mean[2]/mean[1]
  meanT = mean[4]/mean[3]
  changeLlog <- exp(model1 %>% filter(term == "treatmentT:timeT1") %>% select(estimate))[1,1]
>>>>>>> 2426f4f2200432c62edc008ed4c29acb4bbb686a
}



<<<<<<< HEAD
data <- reductioncomp(mean = 3.6, sd = .7, reduction = .3, nt = 12, nc = 4)


reductioncomp(mean = 4.6, sd = .44, reduction = .3)$data %>%
  ggplot(aes(x = time, y = (scase), fill = treatment)) +
=======

tibble(treatment = trmt, time = time, scase = scase) %>%
  ggplot(aes(x = time, y = exp(scase), fill = treatment)) +
>>>>>>> 2426f4f2200432c62edc008ed4c29acb4bbb686a
  geom_boxplot() +
  geom_point(position = position_jitterdodge(),  size = 2) +
  theme_minimal() +
  labs(title = "30% reduction in log scase")
<<<<<<< HEAD

convert_to_lognormal(meanx = 37, varx =870.25)
reductioncomp(mean = 3.36, sd = .70, reduction = .3)
convert_to_lognormal(meanx = 37, varx =870.25)
=======
>>>>>>> 2426f4f2200432c62edc008ed4c29acb4bbb686a
