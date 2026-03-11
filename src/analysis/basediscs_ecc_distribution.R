library(ggplot2)
library(dplyr)
library(tidyr)

setwd("D:/OneDrive/projects/RTA_in_subitizing/src/analysis/")

basediscs <- readr::read_csv("basediscs.csv")

pix_to_deg <- 0.0273


basediscs <- basediscs %>% 
  mutate(far_ecc_deg = far_ecc * pix_to_deg,
         close_ecc_deg = close_ecc * pix_to_deg)


summary_stats <- data.frame(
  variable = c("far_ecc_deg", "close_ecc_deg"),
  mean = c(mean(basediscs$far_ecc_deg, na.rm = TRUE),
           mean(basediscs$close_ecc_deg, na.rm = TRUE)),
  sd   = c(sd(basediscs$far_ecc_deg, na.rm = TRUE),
           sd(basediscs$close_ecc_deg, na.rm = TRUE)),
  min  = c(min(basediscs$far_ecc_deg, na.rm = TRUE),
           min(basediscs$close_ecc_deg, na.rm = TRUE)),
  max  = c(max(basediscs$far_ecc_deg, na.rm = TRUE),
           max(basediscs$close_ecc_deg, na.rm = TRUE))
)


summary_stats

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


plot_df <- basediscs %>%
  select(close_ecc_deg, far_ecc_deg) %>%
  pivot_longer(
    cols = everything(),
    names_to = "ecc_type",
    values_to = "ecc"
  )

base_disc_distr <- ggplot() +
  geom_histogram(
    data = plot_df,
    aes(x = ecc, 
        y = after_stat(density),
        fill = ecc_type),
    position = "identity",
    alpha = 0.4,
    bins = 50) +
    labs(
      x = "Eccentricity",
      y = "Density",
      title = "Distribution of close and far base disc eccentricity") +
  geom_density(
    data = plot_df,
    aes(x = ecc, 
        fill = ecc_type),
    alpha = 0.4) +
  
  scale_fill_manual(
    values = c("close_ecc_deg" = "#BB5566", 
               "far_ecc_deg" = "#004488")) +
  
  my_plot_theme
      
base_disc_distr


