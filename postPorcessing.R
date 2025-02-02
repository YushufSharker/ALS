# Post processing

sim_ALS_output %>%
  mutate(gen_sd = round(gen_sd , 2))%>%
  group_by(Mean_logscale, gen_reduction, gen_sd, nt, nc) %>%
  summarise(power = mean(p.value <.1),
            reduction_orig_percent = mean(reduction_original_percent),
            mean_estimated_percent = mean(estimate)) %>%

  mutate(n = nt+nc) %>%
  filter(n == 8, gen_sd==.12) %>% view()
 ggplot(aes(x = reduction_orig_percent, y = mean_estimated_percent, color = as.factor(n))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Original reduction (%)", y = "Estimated reduction (%)", color = "sampleise") +
  theme_minimal()
# most of they they are on the line but some how estimates looks share when reduction was

 sim_ALS_output %>%
   mutate(gen_sd = round(gen_sd , 2))%>%
   mutate(gen_reduction = round(gen_reduction, 1))%>%
   filter(gen_reduction == 0.3 ) %>%
   group_by(Mean_logscale, gen_reduction, gen_sd, nt, nc) %>%
   summarise(power = mean(p.value <.1),
             reduction_orig_percent = mean(reduction_original_percent),
             mean_estimated_percent = mean(estimate)) %>%
   mutate(n = nt+nc) %>%
   filter(n == 16) %>%
   ggplot(aes(x = Mean_logscale, y = power,
              color = as.factor(gen_sd))) +
   geom_point()+
   geom_line()+
   #geom_smooth(method = "lm", se =FALSE) +
   #stat_smooth(method = lm, formula = y~ns(x,3), se=FALSE) +
   geom_hline(yintercept = 0.1, linetype = "dashed")+
  geom_hline(yintercept = 0.8, linetype = "dotted")
