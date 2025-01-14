# Comparison of 30% reduction in the original scale and log scale
# 30% reduction in log scase
library(tidyverse)
library(broom)

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
}




tibble(treatment = trmt, time = time, scase = scase) %>%
  ggplot(aes(x = time, y = exp(scase), fill = treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(),  size = 2) +
  theme_minimal() +
  labs(title = "30% reduction in log scase")
