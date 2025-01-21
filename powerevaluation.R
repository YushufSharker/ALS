library(tidyverse)

pwr.t2n.test(n1 = 12, n2 = 4, d = -.36/0.21, sig.level = .10, alternative = "two.sided")

<<<<<<< HEAD
# This idea needs to be modified 01/17/2025
=======

>>>>>>> 2426f4f2200432c62edc008ed4c29acb4bbb686a
pboIncRate <-seq(0, 1.4, 0.05)
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


log(28.56) / log(23.8) = 1.05
log(.1)
