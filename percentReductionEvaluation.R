# Comparison of 30% reduction in the original scale and log scale
# 30% reduction in log scase
library(tidyverse)
library(broom)
start_time <- Sys.time()
#
mean = 3
reduction = .1
sd = .05
nt = 6 # Use some even integer
nc = 11

reductioncomp <- function(mean, reduction=.3, sd, nt = 50, nc = 50){
  trmt <- as.factor(c(rep("T", 2*nt), rep("C", 2*nc)))
  time <- as.factor(c( rep("T0", nt), rep("T1", nt), rep("T0", nc), rep("T1", nc) ))
  scase <- c( rnorm(nt, mean, sd), rnorm(nt, mean*(1-reduction), sd), rnorm(2*nc, mean, sd))
  # scase, outcome in log scale
  dat <- tibble(treatment = trmt, time = time, y = scase) %>% mutate(yoriginal = exp(y))
  fit <- lm(y ~ treatment*time, data = dat)
  estimates<- summary(fit)$coefficients

  model1 <- tidy(fit)
  vcov <- vcov(fit)[4,4]
  xxinv <-solve(t(model.matrix(fit)) %*%model.matrix(fit))[4,4]
  varchg_true <- sd^2 * xxinv

  mean_original = dat %>% group_by(treatment, time) %>% dplyr::summarise(mean = median(yoriginal)) %>%
  with(., as.vector(mean))


  reduction_original_percent<- tibble(reduction_original_percent = round((mean_original[4]/mean_original[3])/(mean_original[2]/mean_original[1])), 3) # reduction in original scale in percent)

  # meanl =   dat %>% group_by(treatment, time) %>% dplyr::summarise(mean = mean(y)) %>%
  #   with(., as.vector(mean))
  # reduction_log = exp((meanl[4]-meanl[3])-(meanl[2]-meanl[1])) #reduction in log scale in percent
  # results coinside with the model estimate

  # meanC = 1- mean[2]/mean[1]
  # meanT = 1- mean[4]/mean[3]
  # meanCL = 1- meanl[2]/meanl[1]
  # meanTL = 1- meanl[4]/meanl[3]
  reduction_log_percent <- round((exp(model1 %>% filter(term == "treatmentT:timeT1") %>% select(estimate))[1,1]), 3)
  pvalue = round((model1 %>% filter(term == "treatmentT:timeT1") %>% select(p.value))[1,1], 4)
  input = tibble(Mean_logscale = mean, gen_reduction = reduction, gen_sd = sd, nt = nt, nc = nc)
  output <- cbind(input,
                  reduction_original_percent,
                   reduction_log_percent,
                   pvalue = pvalue)
  output
  return(output)
  # return(list(meanC = meanC, meanT = meanT, meanCL = meanCL, menaTL = meanTL, GMratio = changeLlog, varchg=vcov, median = mean, meanl = meanl,
  #             xxinv = xxinv, varchg_true = varchg_true, estimate = estimates, data=dat))
}

# convert_to_lognormal(meanx = 146.2, varx =82.6)
# reductioncomp(mean = 3.36, sd = .70, reduction = .3)
# convert_to_lognormal(meanx = 37, varx =870.25)
sim.grid <- expand.grid(mean = seq(3, 6, 0.5),
                        reduction = c(.1, .2, .3, .4),
                        sd = c(0.05, .1, .2, .3, .4, .5, .6, .7),
                        nt = c(6, 9, 12),
                        nc = c(11))

source("goparallel.R")
goparallel(ncores = 10)

#### Run Parallel processing
parallel::clusterExport(cl = cl,
                        varlist = c("reductioncomp", "sim.grid"))
parallel::clusterEvalQ(cl = cl,
                       expr = {require(broom)
                       })

sim_ALS_output<- bind_rows( #future_map_dfr under purrr and furrr lib
  parallel::parApply(
    cl = cl,
    X = matrix(1: nrow(sim.grid)),#10),
    MARGIN = 1,
    FUN = function(x) {

      bind_rows(lapply(1:500, function(y) {
        reductioncomp(
          mean = sim.grid$mean[x],
          reduction = sim.grid$reduction[x],
          sd = sim.grid$sd[x],
          nt = sim.grid$nt[x],
          nc = sim.grid$nc[x]
        )
      }))
    }
  ))

parallel::stopCluster(cl)
end_time <- Sys.time()
runtime <- (end_time - start_time) # in hours
save.image("./ALS_SIM_02052025.RData")

