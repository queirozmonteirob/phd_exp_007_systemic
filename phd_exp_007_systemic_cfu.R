# Bacterial Trajectory

my_data = count_data %>% 
  left_join(plan_data, by = join_by("sample_PlateID" == "sample_ID")) %>% 
  select(sample_PlateID, Rep, Age, Sex, Dilution, Volume, BACTERIA, Time_post_injection) %>% 
  mutate(across(c(Dilution, Volume, BACTERIA, Time_post_injection), as.numeric),
         across(where(is.character), as.factor),
         cfu_per_ml = (BACTERIA * Dilution * 1000)/(Volume * 250),
         log2_cfu = log2(cfu_per_ml),
         log2_cfu = ifelse(is.infinite(log2_cfu), NA, log2_cfu))

my_data %>% 
  filter(Time_post_injection < 20) %>% 
  ggplot(aes(Time_post_injection, log2_cfu, colour = Age)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  #facet_grid(~Sex) +
  labs(x = "Time (hours post-infection)",
       y = expression(log[2] ~ "(CFU/mL)")) +
  scale_x_continuous(limits = c(0, 16),
                     breaks = c(seq(0, 16, by = 2))) +
  scale_y_continuous(limits = c(0, 20)) +
  theme_cowplot() +
  theme(aspect.ratio = 1) 

