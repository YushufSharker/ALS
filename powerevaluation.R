library(tidyverse)




x <- pwr.t2n.test(n1 = 12, n2 = 12, d = -.36/0.21, sig.level = .10, alternative = "two.sided")

# This idea needs to be modified 01/17/2025
pboIncRate <-seq(0, 1.4, 0.05) #incrate
trmtIncRate <-seq(0, 1.4, 0.05)
ratedf <- expand.grid(pboIncRate, trmtIncRate)
names(ratedf) <- c("pboIncRate", "trmtIncRate")
Pbobase = 47.6
trmtbase = 47.6
ratedf %>% mutate(pboch = pboIncRate*Pbobase,
                  trtch = trmtIncRate*trmtbase,
                  rat = trtch/pboch) %>%
  filter(pboch  != 0, trtch !=0) %>%
  mutate(logpbo = log(pboch),
         logtrmt = log(trtch),
         lograt = log(rat)) %>%
  with(., write.csv(., "C:/Users/bvi5314/OneDrive - Takeda/Documents/Study/UNC 13A/multiple.csv", row.names = FALSE))

# Get the threshod of reduction for different SD
# change the SD and sample size for n2, and take 1-exponent of the outcome gives
# the threshold
x <- do.call(rbind, lapply(seq(-.8, 0, length.out = 100), function(i)
  pwr.t2n.test(
    n1 = 12,
    n2 = 12,
    d =  i / 0.3,
    sig.level = .10,
    alternative = "less"
  )$power))
tibble(x) %>% mutate(y = seq(-.8, 0, length.out = 100)) %>% with(., max(y[which(x > .8)], na.rm = TRUE))
#####################################

x <- do.call(rbind, lapply(seq(.05, .7, length.out = 1000), function(i)
  pwr.t2n.test(
    n1 = 12,
    n2 = 12,
    d =  -0.3566749 / i,
    sig.level = .10,
    alternative = "less"
  )$power))
tibble(x) %>% mutate(y = seq(.05, .7, length.out = 1000)) %>% with(., max(y[which(x >=.8)], na.rm = TRUE))
# Effect sise entry [1] -0.1053605 -0.1625189 -0.2231436 -0.3566749


