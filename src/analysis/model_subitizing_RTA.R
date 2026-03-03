library(dplyr)
library(tidyr)
library(bbmle)  
library(ggplot2)
library(patchwork)

setwd("D:/OneDrive/projects/RTA_in_subitizing/src/analysis/")
#data <- readr::read_csv(file.choose()) #D:\OneDrive\projects\numerosity_closing_gap\data\enumeration\data_RMenumeration.csv

data <- readr::read_csv("D:/OneDrive/projects/RTA_in_subitizing/data/enumeration/data_RMenumeration.csv")

pilot_list <- c("51778", "580416")

data <- data %>% 
  filter(!participant %in% pilot_list)

data_clean <- data %>%
  filter(
    !is.na(deviation),
    !is.na(protectzonetype),
    !is.na(type),
    !is.na(participant),
    !is.na(key_resp.rt)
  ) %>%
  mutate(
    participant = as.factor(participant),  
    numerosity  = as.factor(numerosity),
    protectzonetype = as.factor(protectzonetype)
  )

# check RT distribution
rt_min <- 0.015
rt_max <- 4

data_clean <- data_clean %>%
  mutate(
    rt_ok = !is.na(key_resp.rt) & key_resp.rt >= rt_min & key_resp.rt <= rt_max
  ) %>%
  filter(rt_ok)

hist(data_clean$key_resp.rt)

# ============================================================
# BAYESIAN OBSERVER MODEL
# — Set-size-dependent noise and bias
# — Eccentricity-modulated noise
# — 6 free parameters
# ============================================================

N_PRIOR <- 4.5

# ============================================================
# MODEL SPECIFICATION
# ============================================================
# bias(n) = bias_int + bias_slope * (n - 4.5)
#   → captures overestimation at small n, underestimation at large n
#
# sigma(n, ecc) = sigma_base + sigma_n_slope * (n - 4.5) + sigma_ecc_slope * (ecc - mean_ecc)
#   → noise grows with set size (subitizing → estimation)
#   → noise grows with eccentricity (worse peripheral resolution)
#
# N_hat = w * N_prior + (1 - w) * (n + bias(n))
# Reported ~ Normal(N_hat, sigma(n, ecc))
#
# 6 free parameters:
#   bias_int        : overall bias
#   bias_slope      : bias modulation by set size --> how bias changes with increasing set size
#   log_sigma_base  : baseline noise
#   log_sigma_n     : noise growth with set size --> whehter noise increase linearly with increasing set size.
#                     try to capture from subitizing to estiamtion change
#   sigma_ecc       : noise growth with eccentricity (can be negative): if positive, noise increases with increasing
#                     eccentricities
#   w_logit         : prior weight


# Grand mean eccentricity for centering
MEAN_ECC <- mean(data_clean$avg_ecc, na.rm = TRUE)

nll_full_observer <- function(par, reported, true_n, ecc) {
  
  bias_int      <- par[1]
  bias_slope    <- par[2]
  sigma_base    <- exp(par[3])
  sigma_n_slope <- exp(par[4])
  sigma_ecc     <- par[5]
  w             <- 1 / (1 + exp(-par[6]))
  
  # bias (set size dependent)
  bias_n <- bias_int + bias_slope * (true_n - N_PRIOR)
  
  # set size and eccentricity dependent noise
  sigma_trial <- sigma_base + 
    sigma_n_slope * (true_n - N_PRIOR) + 
    sigma_ecc * (ecc - MEAN_ECC)
  sigma_trial <- pmax(sigma_trial, 0.05)
  
  # predicted mean
  mu <- w * N_PRIOR + (1 - w) * (true_n + bias_n)
  
  ll <- sum(dnorm(reported, mean = mu, sd = sigma_trial, log = TRUE))
  return(-ll)
}


# fit: per participant x arrangement
participants <- unique(data_clean$participant)
arrangements <- unique(data_clean$protectzonetype)

fit_list <- list()
idx <- 1

for (p in participants) {
  for (a in arrangements) {
    
    sub <- data_clean %>%
      filter(participant == p, protectzonetype == a)
    
    if (nrow(sub) < 10) next
    
    reported <- sub$reportedN
    true_n   <- as.numeric(as.character(sub$numerosity))
    ecc      <- sub$avg_ecc
    
    best_fit <- NULL
    best_nll <- Inf
    
    starts <- list(
      c(0,    -0.2, log(0.5), log(0.1),  0.01, 0),
      c(-0.2, -0.3, log(0.3), log(0.2),  0.05, 0.5),
      c(0.1,  -0.1, log(0.8), log(0.05), -0.01, -0.5)
    )
    
    for (s in starts) {
      fit <- tryCatch(
        optim(par      = s,
              fn       = nll_full_observer,
              reported = reported,
              true_n   = true_n,
              ecc      = ecc,
              method   = "Nelder-Mead",
              control  = list(maxit = 10000)),
        error = function(e) NULL
      )
      
      if (!is.null(fit) && fit$convergence == 0 && fit$value < best_nll) {
        best_fit <- fit
        best_nll <- fit$value
      }
    }
    
    if (!is.null(best_fit)) {
      fit_list[[idx]] <- data.frame(
        participant    = as.character(p),
        arrangement    = as.character(a),
        bias_int       = as.numeric(best_fit$par[1]),
        bias_slope     = as.numeric(best_fit$par[2]),
        sigma_base     = as.numeric(exp(best_fit$par[3])),
        sigma_n_slope  = as.numeric(exp(best_fit$par[4])),
        sigma_ecc      = as.numeric(best_fit$par[5]),
        w              = as.numeric(1 / (1 + exp(-best_fit$par[6]))),
        neg_loglik     = as.numeric(best_fit$value),
        AIC            = as.numeric(2 * 6 + 2 * best_fit$value),
        n_trials       = nrow(sub),
        converged      = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      fit_list[[idx]] <- data.frame(
        participant = as.character(p), arrangement = as.character(a),
        bias_int = NA_real_, bias_slope = NA_real_,
        sigma_base = NA_real_, sigma_n_slope = NA_real_,
        sigma_ecc = NA_real_, w = NA_real_,
        neg_loglik = NA_real_, AIC = NA_real_,
        n_trials = nrow(sub), converged = FALSE,
        stringsAsFactors = FALSE
      )
    }
    idx <- idx + 1
  }
}

fit_results_full <- bind_rows(fit_list)

# cat("Converged:", sum(fit_results_full$converged), "/", nrow(fit_results_full), "\n\n")


# stats

fit_wide <- fit_results_full %>%
  select(participant, 
         arrangement,
         bias_int, 
         bias_slope, 
         sigma_base, 
         sigma_n_slope, 
         sigma_ecc, 
         w) %>%
  pivot_wider(names_from = arrangement,
              values_from = c(bias_int, 
                              bias_slope,
                              sigma_base, 
                              sigma_n_slope, 
                              sigma_ecc,
                              w))

cat("\n BIAS INTERCEPT\n")
t1 <- t.test(fit_wide$bias_int_radial, fit_wide$bias_int_tangential, paired = TRUE)
t1

cat("\n BIAS SLOPE\n")
t2 <- t.test(fit_wide$bias_slope_radial, fit_wide$bias_slope_tangential, paired = TRUE)
t2

cat("\nSIGMA BASE\n")
t3 <- t.test(fit_wide$sigma_base_radial, fit_wide$sigma_base_tangential, paired = TRUE)
t3

cat("\nSIGMA SET-SIZE SLOPE\n")
t4 <- t.test(fit_wide$sigma_n_slope_radial, fit_wide$sigma_n_slope_tangential, paired = TRUE)
t4

cat("\nSIGMA ECCENTRICITY SLOPE \n")
t5 <- t.test(fit_wide$sigma_ecc_radial, fit_wide$sigma_ecc_tangential, paired = TRUE)
t5

cat("\nWEIGHT\n")
t6 <- t.test(fit_wide$w_radial, fit_wide$w_tangential, paired = TRUE)
t6


# model prediction vs. observed
# For the prediction plot, we evaluate at the mean eccentricity 
# of each condition (arrangement × set size)
ecc_by_condition <- data_clean %>%
  group_by(protectzonetype, numerosity) %>%
  summarise(mean_ecc = mean(avg_ecc, na.rm = TRUE), .groups = 'drop') %>%
  rename(arrangement = protectzonetype)

ecc_by_condition <- ecc_by_condition %>%
  mutate(numerosity = as.numeric(as.character(numerosity)))


pred_by_condition <- fit_results_full %>%
  crossing(numerosity = c(3, 4, 5, 6)) %>%
  left_join(ecc_by_condition, by = c("arrangement", "numerosity")) %>%
  mutate(
    true_n  = as.numeric(as.character(numerosity)),
    bias_n  = bias_int + bias_slope * (true_n - N_PRIOR),
    sigma_n = pmax(sigma_base + sigma_n_slope * (true_n - N_PRIOR) +
                     sigma_ecc * (mean_ecc - MEAN_ECC), 0.05),
    pred_response = w * N_PRIOR + (1 - w) * (true_n + bias_n),
    pred_dv = pred_response - true_n
  )

pred_means <- pred_by_condition %>%
  group_by(arrangement, numerosity) %>%
  summarise(pred_dv = mean(pred_dv), .groups = 'drop')

obs_means <- data_clean %>%
  group_by(protectzonetype, numerosity) %>%
  summarise(
    obs_dv = mean(deviation, na.rm = TRUE),
    obs_se = sd(deviation, na.rm = TRUE) / sqrt(n_distinct(participant)),
    .groups = 'drop'
  ) %>%
  rename(arrangement = protectzonetype)


obs_means <- obs_means %>%
  mutate(numerosity = as.numeric(as.character(numerosity)))


combined <- left_join(obs_means, pred_means, by = c("arrangement", "numerosity"))


p_fit <- ggplot()+
  geom_point(
    data = combined,
    aes(
      x = numerosity,
      y = obs_dv,
      color = arrangement,
      group = arrangement
    ),
    size = 3,
    alpha = 0.5,
    position = position_dodge(0.2)
  )+
  geom_errorbar(
    data = combined,
    aes(x = numerosity,
        y = obs_dv, 
        ymin = obs_dv - obs_se, 
        ymax = obs_dv + obs_se,
        color = arrangement,
        group = arrangement),
    width = 0.1,
    linewidth = 0.8,
    position = position_dodge(0.2)
    ) +
  geom_line(
    data = combined,
    aes(x = numerosity,
        y = pred_dv, 
        group = arrangement,
        color = arrangement),
    linewidth = 1.2,
    alpha = 0.5,
    linetype = "dashed") +
  
  geom_point(
    data = combined,
    aes(x = numerosity,
        y = pred_dv,
        color = arrangement,
        group = arrangement), 
    shape = 0, 
    size = 3,
    position = position_dodge(0.2))+
  
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  
  scale_color_manual(
    values = c("radial" = "#BB5566", 
               "tangential" = "#004488")) +
  
  labs(x = "Set Size", y = "Deviation Score",
       title = "Full Model: Predicted (dashed/open) vs Observed (solid)",
       color = "Arrangement") +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(angle = 0, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  ) 

p_fit
  
my_plot_theme <- theme(
  axis.title.x = element_text(color = "black", size = 14, face = "bold", margin = margin(t = 10)),
  axis.title.y = element_text(color = "black", size = 14, face = "bold", margin = margin(r = 10)),
  axis.text.x  = element_text(size = 12, face = "bold", color = "black"),
  axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
  axis.line    = element_line(colour = "black", linewidth = 0.8),
  panel.border     = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  strip.text       = element_text(size = 12, face = "bold"),
  legend.title     = element_text(size = 12, face = "bold"),
  legend.text      = element_text(size = 10),
  plot.title       = element_text(size = 16, face = "bold"),
  plot.subtitle    = element_text(size = 12, color = "grey30"),
  panel.spacing    = unit(1.5, "lines")
)

# Summary Data (Mean and SE)
param_summary <- fit_results_full %>%
  select(participant, 
         arrangement,
         bias_int, 
         bias_slope, 
         sigma_base, 
         sigma_n_slope, 
         sigma_ecc,
         w) %>%
  pivot_longer(cols = -c(participant, arrangement), 
               names_to = "parameter",
               values_to = "value") %>%
  group_by(parameter, 
           arrangement) %>%
  summarise(
    mean_val = mean(value),
    se_val = sd(value) / sqrt(n()),
    ci_lower = mean_val - (1.96*se_val),
    ci_upper = mean_val + (1.96*se_val),
    .groups = 'drop'
  )


p_params <- fit_results_full %>%
  select(participant,
         arrangement,
         bias_int, 
         bias_slope, 
         sigma_base, 
         sigma_n_slope,
         sigma_ecc, 
         w) %>%

  pivot_longer(cols = -c(participant, arrangement), 
               names_to = "parameter", 
               values_to = "value") %>%
  ggplot() +
  
  geom_point(
    aes(x = arrangement,
        y = value,
        group = participant,
        color = arrangement),
    alpha = 0.2
  ) +
  
  geom_line(
    aes(x = arrangement,
        y = value, 
        group = participant), 
            alpha = 0.15, 
    color = "grey40",
    linewidth = 0.4) +
  
  geom_line(data = param_summary, 
            aes(x = arrangement, 
                y = mean_val, 
                group = 1), 
            color = "black", 
            linewidth = 1,
            alpha = 0.5) +
  
  geom_point(data = param_summary, 
             aes(x = arrangement,
                 y = mean_val, 
                 color = arrangement), 
             size = 4,
             alpha = 0.8) +
  
  geom_errorbar(data = param_summary,
                aes(x = arrangement, 
                    ymin = ci_lower,
                    ymax = ci_upper, 
                    color = arrangement),
                width = 0.0, 
                linewidth = 1,
                size = 0.5) +
  
  scale_color_manual(
    values = c("radial" = "#BB5566", 
               "tangential" = "#004488")) +
  labs(
    title = "Model results: Individual vs. Group Mean",
    subtitle = "Thin lines = Participants; Bold black line = Group Mean (±95%CI)",
    x = "", y = "Estimated Parameter Value"
  ) +
  my_plot_theme + 
  
  theme(legend.position = "none") + 
  
  facet_wrap(~parameter, 
             scales = "free_y", 
             nrow = 2,
             labeller = labeller(parameter = c(
               "bias_int"      = "Bias Intercept",
               "bias_slope"    = "Bias Slope",
               "sigma_base"    = "Baseline Sigma",
               "sigma_n_slope" = "Noise Slope",
               "sigma_ecc"     = "Eccentricity",
               "w"             = "Weighting Factor"
             )))

p_params

# ggsave(file = "p_params.svg", plot = p_params, width = 8, height = 8, units = "in")


# Noise: Eccentricity x Set Size
# how 'sigma_trial' behaves for an 'average' participant

mean_params <- fit_results_full %>%
  group_by(arrangement) %>%
  summarise(across(bias_int:w, mean))

# prediction grid to visualize
viz_grid <- expand.grid(
  true_n = seq(3, 6, length.out = 30),
  ecc = seq(min(data_clean$avg_ecc),
            max(data_clean$avg_ecc), 
            length.out = 30),
  arrangement = c("radial", "tangential")
) %>%
  left_join(mean_params, by = "arrangement") %>%
  mutate(
    sigma_total = pmax(sigma_base + sigma_n_slope * (true_n - 4.5) + sigma_ecc * (ecc - mean(data_clean$avg_ecc)), 0.05)
  )

p_noise_heat <- 
  ggplot(data = viz_grid, 
         aes(x = true_n, 
             y = ecc, 
             fill = sigma_total)) +
  
  geom_tile() +
  
  scale_fill_viridis_c(option = "C", 
                       name = "Sensory Noise (σ)") +
  
  labs(x = "Set Size", y = "Eccentricity (deg)",
       title = "Predicted Sensory Uncertainty (Sigma)") +
  
  my_plot_theme +
  
  facet_wrap(~arrangement)
  
p_noise_heat


# ggsave(file = "p_noise_heat.svg", plot = p_noise_heat, width = 8, height = 4, units = "in")

# (p_params / p_noise_heat) + plot_layout(heights = c(2, 1))


# diff sigma = radial sigma - tangential Sigma
gap_grid <- viz_grid %>%
  select(true_n, ecc, arrangement, sigma_total) %>%
  pivot_wider(names_from = arrangement, values_from = sigma_total) %>%
  mutate(sigma_gap = radial - tangential)

p_gap_heat <- ggplot(data = gap_grid, 
                     aes(x = true_n, y = ecc, fill = sigma_gap)) +
  geom_raster(interpolate = TRUE) +
  
  scale_fill_gradient2(
    low = "#004488",
    mid = "white",
    high = "#BB5566",
    midpoint = 0,
    name = "Predicted Sensory Difference (R sigma - T sigma)"
  ) +
  labs(x = "Set Size", 
       y = "Eccentricity (deg)", 
       subtitle = "Red indicates higher noise in Radial vs. Tangential") +
  
  my_plot_theme +
  
  theme(axis.line = element_blank())

p_gap_heat


