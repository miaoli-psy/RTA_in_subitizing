# if (!require("devtools")) {
#   install.packages("devtools", dependencies = TRUE)}

# devtools::install_github("DejanDraschkow/mixedpower") 

library(dplyr)
library(mixedpower)

setwd("D:/OneDrive/projects/RTA_in_subitizing/src/power_analysis/")
#data <- readr::read_csv(file.choose()) #D:\OneDrive\projects\numerosity_closing_gap\data\enumeration\data_RMenumeration.csv

data <- readr::read_csv("D:/OneDrive/projects/RTA_in_subitizing/data/enumeration/data_RMenumeration.csv")

pilot_list <- c("51778", "580416")

data <- data %>% 
  filter(participant %in% pilot_list)

data_clean <- data %>%
  filter(
    !is.na(deviation),
    !is.na(protectzonetype),
    !is.na(type),
    !is.na(participant),
    !is.na(key_resp.rt)
  ) %>%
  mutate(
    numerosity  = as.factor(numerosity),
    protectzonetype = as.factor(protectzonetype)
  )


lmm_dv <- lme4::lmer(
  deviation ~ protectzonetype * numerosity + type  + (1 + protectzonetype|participant),
  data = data_clean
)


# estimate power ~20min
power <- mixedpower(model = lmm_dv, data = data_clean,
                    fixed_effects = c("protectzonetype", "numerosity", "type"),
                    simvar = "participant", steps = c(20, 25, 30, 40, 50),
                    critical_value = 2, n_sim = 1000)
