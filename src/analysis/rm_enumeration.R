library(dplyr)
library(ggplot2)
library(svglite)
library(tidyverse)
library(gghalves)


setwd("D:/OneDrive/projects/RTA_in_subitizing/src/analysis/")
#data <- readr::read_csv(file.choose()) #D:\OneDrive\projects\numerosity_closing_gap\data\enumeration\data_RMenumeration.csv

data <- readr::read_csv("D:/OneDrive/projects/RTA_in_subitizing/data/enumeration/data_RMenumeration.csv")

participantN = unique(data$participant)
participantN

pilot_list <- c("51778", "580416")

data <- data %>% 
  filter(!participant %in% pilot_list)

# check age, sex

df_check_age <- data %>%
  group_by(participant, age, sex) %>%
  tally() 

mean(data$age)

# remove trials based on RT?
# rt_min <- 0.015
# rt_max <- 4
# 
# data <- data %>%
#   mutate(
#     rt_ok = !is.na(key_resp.rt) & key_resp.rt >= rt_min & key_resp.rt <= rt_max
#   ) %>%
#   filter(rt_ok)


# add column configuration

data <- data %>%
  mutate(
    numerosity = as.numeric(numerosity),
    type = as.character(type),
    protectzonetype = as.character(protectzonetype),
    
    configuration = case_when(
      numerosity == 3 & type == "far"   ~ "far",
      numerosity == 3 & type == "close" ~ "close",
      
      numerosity == 4 & type == "far"   ~ "1+3",
      numerosity == 4 & type == "close" ~ "3+1",
      numerosity == 4 & type == "both"  ~ "2+2",
      
      numerosity == 5 & type == "far"   ~ "2+3",
      numerosity == 5 & type == "close" ~ "3+2",
      
      numerosity == 6 & type == "both"  ~ "3+3",
      
      TRUE ~ NA_character_
    ),
    
    arrangement = case_when(
      str_to_lower(protectzonetype) %in% c("radial", "r") ~ "radial",
      str_to_lower(protectzonetype) %in% c("tangential", "t") ~ "tangential",
      TRUE ~ protectzonetype
    ),
    
    configuration = factor(
      configuration,
      levels = c("far", "close", "1+3", "3+1", "2+2", "2+3", "3+2", "3+3")
    ),
    arrangement = factor(arrangement, levels = c("radial", "tangential"))
  )

# # check conditions
# data %>%
#   count(numerosity, type, configuration, sort = TRUE)

# error distribution----------------

error_distri <- ggplot() +
  
  geom_histogram(
    data = data,
    aes(x = deviation, 
        colour = arrangement, 
        fill = arrangement),
    stat = "bin",
    binwidth = 0.5,
    boundary = -0.5,
    alpha = 0.2,
    position = position_dodge(width = 0.5)
  )+
  facet_wrap(~ numerosity, nrow = 1) +
  
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2 ,3), limits = c(-3.5, 3.5)) +
  
  labs(
    x = "Deviation",
    y = "Count") +
  
  scale_fill_manual(labels = c("radial", "tangential"),
                    values = c("#BB5566", "#004488")) +
  
  my_plot_theme

error_distri


# proportion correct

prop_correct_by_parti <- data %>% 
  group_by(participant, numerosity, arrangement) %>% 
  summarise(
    prop_correct = mean(reportedN == numerosity, na.rm = TRUE),
    .groups = "drop"
  )


prop_correct_across_parti <- prop_correct_by_parti %>% 
  group_by(numerosity, arrangement) %>% 
  summarise(
    avg_prop_correct = mean(prop_correct),
    sd = sd(prop_correct),
    n = n()
  ) %>% 
  mutate(
    sem = sd/sqrt(n),
    ci =qt(0.975,(n-1)) * sem
  )
        

plot_prop_correct <- ggplot() + 
  
  geom_point(
    data = prop_correct_across_parti,
    aes(x = numerosity,
        y = avg_prop_correct,
        color = arrangement,
        shape = arrangement),
    
    position = position_dodge(0.8),
    stat = "identity",
    size = 4,
    alpha = 0.9) +
  
  geom_errorbar(
    data = prop_correct_across_parti,
    aes(
      x = numerosity,
      y = avg_prop_correct,
      ymin = avg_prop_correct - ci,
      ymax = avg_prop_correct + ci,
      group = arrangement
    ),
    position = position_dodge(width = 0.8),
    stat = "identity",
    width = 0,
    color = "black",
    size = 0.8,
    alpha = 1
  ) +
  
  geom_line(
    data = prop_correct_across_parti,
    aes(x = numerosity,
        y = avg_prop_correct,
        color = arrangement),
    
    position = position_dodge(0.8),
    stat = "identity",
    size = 1,
    alpha = 0.9
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488")) +
  
  
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1)) +
  
  labs(
    x = "Set size",
    y = "Propotion correct"
    
  ) +
  my_plot_theme

plot_prop_correct


# # proportion correct at different visual field
# 
# prop_correct_by_parti2 <- data %>% 
#   group_by(participant, numerosity, arrangement, rotation_index) %>% 
#   summarise(
#     prop_correct = mean(reportedN == numerosity, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# 
# prop_correct_across_parti2 <- prop_correct_by_parti2 %>% 
#   group_by(numerosity, arrangement, rotation_index) %>% 
#   summarise(
#     avg_prop_correct = mean(prop_correct),
#     sd = sd(prop_correct),
#     n = n(),
#     .groups = "drop"
#   ) %>% 
#   mutate(
#     sem = sd/sqrt(n),
#     ci =qt(0.975,(n-1)) * sem
#   )
# 
# 
# 
# plot_prop_correct2 <- ggplot() + 
#   
#   geom_point(
#     data = prop_correct_across_parti2,
#     aes(x = numerosity,
#         y = avg_prop_correct,
#         color = arrangement,
#         shape = arrangement),
#     
#     position = position_dodge(0.8),
#     stat = "identity",
#     size = 4,
#     alpha = 0.9) +
#   
#   geom_errorbar(
#     data = prop_correct_across_parti2,
#     aes(
#       x = numerosity,
#       y = avg_prop_correct,
#       ymin = avg_prop_correct - ci,
#       ymax = avg_prop_correct + ci,
#       group = arrangement
#     ),
#     position = position_dodge(width = 0.8),
#     stat = "identity",
#     width = 0,
#     color = "black",
#     size = 0.8,
#     alpha = 1
#   ) +
#   
#   geom_line(
#     data = prop_correct_across_parti2,
#     aes(x = numerosity,
#         y = avg_prop_correct,
#         color = arrangement),
#     
#     position = position_dodge(0.8),
#     stat = "identity",
#     size = 1,
#     alpha = 0.9
#   ) +
#   
#   geom_hline(yintercept = 1, linetype = "dashed") +
#   
#   
#   scale_color_manual(labels = c("radial", "tangential"),
#                      values = c("#BB5566", "#004488")) +
#   
#   
#   scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1)) +
#   
#   labs(
#     x = "Set size",
#     y = "Propotion correct"
#     
#   ) +
#   my_plot_theme+
#   
#   facet_wrap(~rotation_index)
# 
# plot_prop_correct2



# ----deviation and cv --------------

data_by_subject <- data %>% 
  group_by(numerosity, configuration, participant, arrangement) %>% 
  summarise(
    n = n(),
    deviation = mean(deviation, na.rm = TRUE),
    rt = mean(key_resp.rt),
    # cv
    mean_reportedN = mean(reportedN, na.rm = TRUE),
    sd_reportedN = sd(reportedN, na.rm = TRUE),
    cv = sd_reportedN/mean_reportedN,
    # weber fraction
    weber_fraction = sd_reportedN / first(numerosity),
    .groups = 'drop'
  )


data_across_subject <- data_by_subject %>% 
  group_by(numerosity, configuration, arrangement) %>% 
  summarise(
    n = n(),
    mean_deviation = mean(deviation, na.rm = TRUE),
    sd_deviaiton = sd(deviation, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE),
    
    mean_cv = mean(cv, na.rm = TRUE),
    sd_cv = sd(cv, na.rm = TRUE),
    
    mean_weber_fraction = mean(weber_fraction, na.rm = TRUE),
    sd_weber_fraction = sd(weber_fraction, na.rm = TRUE),
    
    .groups = "drop"
  ) %>% 
  mutate(
    sem_deviation = sd_deviaiton / sqrt(n),
    ci_deviation = sem_deviation * qt(0.975, df = n - 1),
    
    sem_rt = sd_rt / sqrt(n),
    ci_rt = sem_rt * qt(0.975, df = n - 1),
    
    sem_cv = sd_cv / sqrt(n),
    ci_cv = sem_cv * qt(0.975, df = n - 1),
    
    sem_weber_fraction = sd_weber_fraction / sqrt(n),
    ci_weber_fraction = sem_weber_fraction * qt(0.975, df = n - 1)
  )


my_plot_theme <- theme(
  axis.title.x = element_text(color = "black", size = 14, face = "bold"),
  axis.title.y = element_text(color = "black", size = 14, face = "bold"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "grey"),
  axis.text.x = element_text(size = 12, face = "bold"),
  axis.text.y = element_text(size = 12, face = "bold"),
  legend.title = element_text(size = 12, face = "bold"),
  legend.text = element_text(size = 12),
  strip.text.x = element_text(size = 12, face = "bold"),
  strip.text.y = element_text(size = 12, face = "bold")
)


plot_deviation <- ggplot() + 
  
  geom_point(
    data = data_across_subject,
    aes(x = configuration,
        y = mean_deviation,
        color = arrangement,
        shape = arrangement),

    position = position_dodge(0.8),
    stat = "identity",
    size = 4,
    alpha = 0.9) +
  
  # geom_boxplot(
  #   data = data_by_subject,
  #   aes(
  #     x = configuration,
  #     y = deviation,
  #     color = arrangement,
  #     group = interaction(configuration, arrangement)
  #   ),
  #   position = position_dodge2(width = 0.8, preserve = 'single'),
  #   width = 0.6,
  #   alpha = 0.6,
  #   outlier.alpha = 0.3
  # ) +
  #     
  
  geom_half_violin(
    data = data_by_subject,
    aes(
      x = configuration,
      y = deviation,
      fill = arrangement
    ),
    side = "l",
    position = position_dodge(width = 0.8),
    alpha = 0.3,
    width = 1,
    color = "white",
    trim = FALSE
    
  ) +
  geom_errorbar(
    data = data_across_subject,
    aes(
      x = configuration,
      y = mean_deviation,
      ymin = mean_deviation - ci_deviation,
      ymax = mean_deviation + ci_deviation,
      group = arrangement
    ),
    position = position_dodge(width = 0.8),
    stat = "identity",
    width = 0,
    color = "black",
    size = 0.8,
    alpha = 1
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ numerosity, 
             scales = "free_x",
             nrow = 1,
             labeller = labeller(
               numerosity = c(
                 `3` = "Set size 3",
                 `4` = "Set size 4",
                 `5` = "Set size 5",
                 `6` = "Set size 6"
               )
             )) +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488")) +
  
  scale_fill_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488")) +
  
  scale_y_continuous(breaks = c(-2,-1,0,1,2), limits = c(-2, 2)) +
  
  labs(
    x = "Configuration",
    y = "Bias (deviaiton)"
  ) +
  my_plot_theme

plot_deviation


# ggsave(file = "plot_deviation.svg", plot = plot_deviation, width = 11.3, height = 3.4, units = "in")


plot_cv <- ggplot() + 
  
  geom_point(
    data = data_across_subject,
    aes(x = configuration,
        y = mean_cv,
        color = arrangement,
        shape = arrangement),
    
    position = position_dodge(0.8),
    stat = "identity",
    size = 4,
    alpha = 0.9) +
  
  
  geom_half_violin(
    data = data_by_subject,
    aes(
      x = configuration,
      y = cv,
      fill = arrangement
    ),
    side = "l",
    position = position_dodge(width = 0.8),
    alpha = 0.3,
    width = 1,
    color = "white",
    trim = FALSE
    
  ) +
  geom_errorbar(
    data = data_across_subject,
    aes(
      x = configuration,
      y = mean_cv,
      ymin = mean_cv - ci_cv,
      ymax = mean_cv + ci_cv,
      group = arrangement
    ),
    position = position_dodge(width = 0.8),
    stat = "identity",
    width = 0,
    color = "black",
    size = 0.8,
    alpha = 1
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ numerosity, 
             scales = "free_x",
             nrow = 1,
             labeller = labeller(
               numerosity = c(
                 `3` = "Set size 3",
                 `4` = "Set size 4",
                 `5` = "Set size 5",
                 `6` = "Set size 6"
               )
             )) +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488")) +
  
  scale_fill_manual(labels = c("radial", "tangential"),
                    values = c("#BB5566", "#004488")) +
  
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4), limits = c(-0.01, 0.4)) +
  
  labs(
    x = "Configuration",
    y = "Precision (coefficient of variation)"
  ) +
  my_plot_theme

plot_cv

# ggsave(file = "plot_cv.svg", plot = plot_cv, width = 11.3, height = 3.4, units = "in")


# the same plot collapse configuration

data_by_subject2 <- data %>% 
  group_by(numerosity, participant, arrangement) %>% 
  summarise(
    n = n(),
    deviation = mean(deviation, na.rm = TRUE),
    rt = mean(key_resp.rt),
    # cv
    mean_reportedN = mean(reportedN, na.rm = TRUE),
    sd_reportedN = sd(reportedN, na.rm = TRUE),
    cv = sd_reportedN/mean_reportedN,
    # weber fraction
    weber_fraction = sd_reportedN / first(numerosity),
    .groups = 'drop'
  )


data_across_subject2 <- data_by_subject2 %>% 
  group_by(numerosity, arrangement) %>% 
  summarise(
    n = n(),
    mean_deviation = mean(deviation, na.rm = TRUE),
    sd_deviaiton = sd(deviation, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE),
    
    mean_cv = mean(cv, na.rm = TRUE),
    sd_cv = sd(cv, na.rm = TRUE),
    
    mean_weber_fraction = mean(weber_fraction, na.rm = TRUE),
    sd_weber_fraction = sd(weber_fraction, na.rm = TRUE),
    
    .groups = "drop"
  ) %>% 
  mutate(
    sem_deviation = sd_deviaiton / sqrt(n),
    ci_deviation = sem_deviation * qt(0.975, df = n - 1),
    
    sem_rt = sd_rt / sqrt(n),
    ci_rt = sem_rt * qt(0.975, df = n - 1),
    
    sem_cv = sd_cv / sqrt(n),
    ci_cv = sem_cv * qt(0.975, df = n - 1),
    
    sem_weber_fraction = sd_weber_fraction / sqrt(n),
    ci_weber_fraction = sem_weber_fraction * qt(0.975, df = n - 1)
  )



plot_deviation <- ggplot() + 
  
  geom_point(
    data = data_across_subject2,
    aes(x = as.factor(numerosity),
        y = mean_deviation,
        color = arrangement,
        shape = arrangement),
    
    position = position_dodge(0.8),
    stat = "identity",
    size = 4,
    alpha = 0.9) +
  
  geom_half_violin(
    
    data = data_by_subject2,
    aes(
      x = as.factor(numerosity),
      y = deviation,
      fill = arrangement,
    ),
    side = "l",
    position = position_dodge(width = 0.8),
    alpha = 0.3,
    width = 1,
    color = "white",
    trim = FALSE
    
  ) +
  
  geom_errorbar(
    data = data_across_subject2,
    aes(
      x = as.factor(numerosity),
      y = mean_deviation,
      ymin = mean_deviation - ci_deviation,
      ymax = mean_deviation + ci_deviation,
      group = arrangement
    ),
    position = position_dodge(width = 0.8),
    stat = "identity",
    width = 0,
    color = "black",
    size = 0.8,
    alpha = 1
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488")) +
  
  scale_fill_manual(labels = c("radial", "tangential"),
                    values = c("#BB5566", "#004488")) +
  
  scale_y_continuous(breaks = c(-2,-1,0,1,2), limits = c(-2, 2)) +
  
  labs(
    x = "Set size",
    y = "Bias (deviaiton)"
  ) +
  my_plot_theme

plot_deviation

# ggsave(file = "plot_deviation2.svg", plot = plot_deviation, width = 8, height = 3.4, units = "in")

plot_cv <- ggplot() + 
  
  geom_point(
    data = data_across_subject2,
    aes(x = as.factor(numerosity),
        y = mean_cv,
        color = arrangement,
        shape = arrangement),
    
    position = position_dodge(0.8),
    stat = "identity",
    size = 4,
    alpha = 0.9) +
  
  geom_half_violin(
    
    data = data_by_subject2,
    aes(
      x = as.factor(numerosity),
      y = cv,
      fill = arrangement,
    ),
    side = "l",
    position = position_dodge(width = 0.8),
    alpha = 0.3,
    width = 1,
    color = "white",
    trim = FALSE
    
  ) +
  
  geom_errorbar(
    data = data_across_subject2,
    aes(
      x = as.factor(numerosity),
      y = mean_cv,
      ymin = mean_cv - ci_cv,
      ymax = mean_cv + ci_cv,
      group = arrangement
    ),
    position = position_dodge(width = 0.8),
    stat = "identity",
    width = 0,
    color = "black",
    size = 0.8,
    alpha = 1
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488")) +
  
  scale_fill_manual(labels = c("radial", "tangential"),
                    values = c("#BB5566", "#004488")) +
  
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4), limits = c(-0.01, 0.4)) +
  
  labs(
    x = "Set size",
    y = "Precision (coefficient of variation)"
  ) +
  my_plot_theme

plot_cv

# ggsave(file = "plot_cv2.svg", plot = plot_cv, width = 8, height = 3.4, units = "in")


# ===================LMM ==============

# data clean - for model - contrast coded 

data_clean <- data %>%
  mutate(
    participant = as.factor(participant),  
    numerosity  = as.factor(numerosity),
    arrangement = as.factor(arrangement)
  )

# contrast coding
data_clean <- data_clean %>%
  mutate(
    arrangement_c = ifelse(protectzonetype == "radial", -0.5, 0.5)
  )


# model comparison within each set size dv

m_ss3_full <- lme4::lmer(deviation ~ configuration * arrangement + 
                           (1 + arrangement | participant),
                         data = data_clean %>% filter(numerosity == 3))

m_ss3_reduced <- lme4::lmer(deviation ~ configuration + arrangement + 
                              (1 + arrangement | participant),
                            data = data_clean %>% filter(numerosity == 3))

anova(m_ss3_reduced, m_ss3_full)


emm  <- emmeans::emmeans(
  m_ss3_full,
  ~ arrangement | configuration)

emm 

contrasts  <- emmeans::contrast(
  emm,
  method = "pairwise",
  adjust = "holm"  
)
contrasts 






m_ss4_full <- lme4::lmer(deviation ~ configuration * arrangement + 
                     (1 + arrangement | participant),
                   data = data_clean %>% filter(numerosity == 4))

m_ss4_reduced <- lme4::lmer(deviation ~ configuration + arrangement + 
                        (1 + arrangement | participant),
                      data = data_clean %>% filter(numerosity == 4))

anova(m_ss4_reduced, m_ss4_full)


m_ss5_full <- lme4::lmer(deviation ~ configuration * arrangement + 
                           (1 + arrangement | participant),
                         data = data_clean %>% filter(numerosity == 5))

m_ss5_reduced <- lme4::lmer(deviation ~ configuration + arrangement + 
                              (1 + arrangement | participant),
                            data = data_clean %>% filter(numerosity == 5))

anova(m_ss5_reduced, m_ss5_full)


# LMM DV
lmm_dv_main <- lme4::lmer(
  deviation ~ arrangement_c * numerosity   + (1 + arrangement_c|participant),
  data = data_clean
)

lmm_dv2 <- lme4::lmer(
  deviation ~ arrangement_c + numerosity + (1 + arrangement_c|participant),
  data = data_clean
)

anova(lmm_dv_main, lmm_dv2)



sjPlot::tab_model(
  lmm_dv_main,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)


emm_arr_by_num  <- emmeans::emmeans(
  lmm_dv_main,
  ~ arrangement_c | numerosity)

emm_arr_by_num 

arrangement_contrasts  <- emmeans::contrast(
  emm_arr_by_num,
  method = "pairwise",
  adjust = "holm"  
)
arrangement_contrasts 


# model comparison within each set size cv

m_ss3_full <- lme4::lmer(cv ~ configuration * arrangement + 
                           (1 + arrangement | participant),
                         data = data_by_subject %>% filter(numerosity == 3))

m_ss3_reduced <- lme4::lmer(cv ~ configuration + arrangement + 
                              (1 + arrangement | participant),
                            data = data_by_subject %>% filter(numerosity == 3))

anova(m_ss3_reduced, m_ss3_full)


m_ss4_full <- lme4::lmer(cv ~ configuration * arrangement + 
                           (1 + arrangement | participant),
                         data = data_by_subject %>% filter(numerosity == 4))

m_ss4_reduced <- lme4::lmer(cv ~ configuration + arrangement + 
                              (1 + arrangement | participant),
                            data = data_by_subject %>% filter(numerosity == 4))

anova(m_ss4_reduced, m_ss4_full)


m_ss5_full <- lme4::lmer(cv ~ configuration * arrangement + 
                           (1 + arrangement | participant),
                         data = data_by_subject %>% filter(numerosity == 5))

m_ss5_reduced <- lme4::lmer(cv ~ configuration + arrangement + 
                              (1 + arrangement | participant),
                            data = data_by_subject %>% filter(numerosity == 5))

anova(m_ss5_reduced, m_ss5_full)


# LMM cv

# contrast coding
data_by_subject <- data_by_subject %>%
  mutate(
    numerosity = as.factor(numerosity),
    arrangement_c = ifelse(arrangement == "radial", -0.5, 0.5)
  )


lmm_cv_main <- lme4::lmer(
  cv ~ arrangement_c* numerosity   + (1 + arrangement_c|participant),
  data = data_by_subject
)

lmm_cv2 <- lme4::lmer(
  cv ~ arrangement_c+ numerosity + (1 + arrangement_c|participant),
  data = data_by_subject
)


anova(lmm_cv_main, lmm_cv2)


sjPlot::tab_model(
  lmm_cv2,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)


emm_arr_by_num  <- emmeans::emmeans(
  lmm_cv_main,
  ~ arrangement_c | numerosity)

emm_arr_by_num 

arrangement_contrasts  <- emmeans::contrast(
  emm_arr_by_num,
  method = "pairwise",
  adjust = "holm"  
)
arrangement_contrasts 








