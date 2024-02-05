# Setup
## Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               tidyverse,
               janitor, broom, glue,
               knitr, kableExtra,
               brglm2, sandwich,
               effectsize, marginaleffects, performance,
               report, modelsummary,
               ggeffects, hrbrthemes, viridis, ggpubr,
               install = TRUE,
               update = FALSE)

## Data
load(file = file.path(here(), "data", "data_csn.RData"))

## Hyperparameters
stars <- c("*" = .05,
           "**" = .01,
           "***" = .001)

## Functions
source(file.path(here(), "functions", "f_modelsummary.R"))
source(file.path(here(), "functions", "f_p_adjust.R"))
source(file.path(here(), "functions", "f_p_adjust_transformer.R"))
source(file.path(here(), "functions", "f_fround.R"))

## Variables
can.vars.mx <- c( # Potential candidate alt main effect / moderator variables
  "can_sex"
)

vac.vars.jt <- c( # Potential job moderators (1)
  "vac_job_type",
  "vac_work_hours",
  "vac_shift_system"
)

vac.vars.lang <- c( # Potential job moderators (2)
  "vac_lang_dutch",
  "vac_lang_french",
  "vac_lang_english"
)

vac.vars.exp <- c( # Potential job moderators (3)
  "vac_experience_requested"
)

can.vars.occ <- c( # Potential job moderators (4)
  "can_occupation"
)

vac.vars.c <- c( # Vacancy controls
  "vac_province"
)

gpt.vars.c <- c( # GPT-3.5 controls
  "gpt_temperature_fct"
)

## Model parameters
lnr.frml <- "gpt_score ~ "
model.link <- ". ~ ."
