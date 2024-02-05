# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               dplyr, stringr, knitr, forcats,
               ggplot2, hrbrthemes, viridis, colorspace,
               kableExtra,
               ggpubr,
               install = TRUE,
               update = FALSE)

# Data
load(file = file.path(here(), "data", "data_csn.RData"))

# Functions
source(file.path(here(), "functions", "f_fround.R"))

# Hyperparameters
## Fonts
extrafont::loadfonts(quiet = TRUE)
available_fonts <- extrafont::fonttable()$Family
primary <- "UGent Panno Text"
secondary <- "Arial"

## Check for primary UGent font, else rely on secondary option
family <-
  if (primary %in% available_fonts) {
    primary
  } else {
    secondary
  }

## Graph
blue <- "#1E64C8"

device <- "png"
dpi <- 1000
bg <- "white"

title.size <- 11
text.size <- 10
axis.size <- 10
axis.title.size <- 10
caption.size <- 8
ann.size <- 8

colopt <- "viridis"
colb <- .1
cole <- .6

colors <- viridis(2, begin = colb, end = cole, option = colopt)
minority_color <- colors[2]
majority_color <- colors[1]
