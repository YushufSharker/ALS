# Post processing
library(tidyverse)
load("~/GitHub/ALS/ALS_SIM_02052025.RData")
sim_ALS_output %>%
  mutate(gen_sd = round(gen_sd , 2))%>%
  group_by(Mean_logscale, gen_reduction, gen_sd, nt, nc) %>%
  summarise(power = mean(p.value <.1),
            reduction_orig_scale = mean(reduction_original_percent),
            reduction_log_scale = mean(estimate)) %>%

  mutate(n = nt+nc) %>%
  filter(n == 16) %>%
 ggplot(aes(x = reduction_log_scale, y = reduction_orig_scale)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~ns(2)) +
  labs(x = "Reduction in log scale (%)", y = "reduction in original scale (%)", color = "sampleise") +
  facet_wrap(~gen_sd)
  theme_minimal()
# most of they they are on the line but some how estimates looks share when reduction was

 sim_ALS_output %>%
   mutate(gen_sd = round(gen_sd , 2))%>%
   mutate(gen_reduction = round(gen_reduction, 1))%>%
   #filter(gen_reduction == 0.2 ) %>%
   group_by(Mean_logscale, gen_reduction, gen_sd, nt, nc) %>%
   summarise(power = mean(p.value <.1),
             reduction_orig_percent = mean(reduction_original_percent),
             mean_estimated_percent = mean(estimate)) %>%
   mutate(n = nt+nc) %>%
   ggplot(aes(x = gen_sd, y = power,
              color = as.factor(Mean_logscale ))) +
   geom_point()+
   geom_line()+
   #geom_smooth(method = "lm", se =FALSE) +
   #stat_smooth(method = lm, formula = y~ns(x,3), se=FALSE) +
   geom_hline(yintercept = 0.1, linetype = "dashed")+
  geom_hline(yintercept = 0.8, linetype = "dotted")+
   xlab("Genomic SD") +
   scale_x_continuous(breaks = seq(0, .7, .1))+
  facet_wrap(~n*gen_reduction, scales = "free")

 # Figure

 sim_ALS_output %>%
   mutate(gen_sd = round(gen_sd , 2))%>%
   mutate(gen_reduction = round(gen_reduction, 1))%>%
   #filter(gen_reduction %in% c(.3) ) %>%
   group_by(gen_reduction, gen_sd, nt, nc) %>%
   summarise(power = mean(p.value <.1)) %>%
   mutate(n = nt+nc) %>%
   group_by(n) %>%
   mutate(cohensd = (log(gen_reduction)/gen_sd)) %>%
   filter(n>4) %>%
   ggplot(aes(x = gen_sd, y = power, color = as.factor(n))) +
   geom_point()+
   geom_line()+
   #geom_smooth(method = "lm", se =FALSE) +
   #stat_smooth(method = lm, formula = y~ns(x,3), se=FALSE) +
   geom_hline(yintercept = 0.1, linetype = "dashed")+
   geom_hline(yintercept = 0.8, linetype = "dotted")+
   xlab("Genomic SD") +
   scale_x_continuous(breaks = seq(0, .7, .05))+
   facet_wrap(~gen_reduction, scales = "free")

 # Table

 sim_ALS_output %>%
   mutate(gen_sd = round(gen_sd , 2))%>%
   mutate(gen_reduction = round(gen_reduction, 1))%>%
   filter(gen_reduction !=0 ) %>%
   group_by(gen_reduction, gen_sd, nt, nc) %>%
   summarise(power = mean(p.value <.1)) %>%
   mutate(n = nt+nc) %>%
   group_by(n) %>%
   mutate(cohensd = (log(1-gen_reduction)/gen_sd)) %>%
   filter(n>4, power > .8) %>%
   select(-nt, -nc) %>%
   group_by(gen_reduction, gen_sd, n) %>%
   summarise(power = mean(power),
             cohensd = mean(cohensd)) %>%
   filter(gen_sd>0.05, !gen_reduction %in% c(.1, .5)) %>%
   ungroup()%>%
   select(gen_reduction, gen_sd, n) %>%
   group_by(gen_reduction, gen_sd) %>%
   summarise(min_n = min(n)) %>%
   pivot_wider(names_from = gen_sd, values_from = min_n) %>%

 view()





 library(gt)
 library(dplyr)
 library(tidyr)

 # Sample dataframe
 df <- data.frame(
   genred = c("Red1", "Red2", "Red1", "Red2"),
   gensd = c("SD1", "SD1", "SD2", "SD2"),
   n = c(10, 20, 30, 40)
 )

 # Reshape and create the gt table
 df %>%
   pivot_wider(names_from = gensd, values_from = n) %>%
   gt(rowname_col = "genred")
