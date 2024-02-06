# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, install = TRUE, update = FALSE)


# Setup
source(file.path(here(), "code", "3a_csn_het_job.R"))
source(file.path(here(), "code", "plots", "csn_fig_setup.R"))


# Functions
save <- function(plot, name, w = 17, h = 17, device){
  ggsave(
    filename = paste0("csn_figA_", name, ".", device),
    plot = plot,
    device = device,
    path = file.path(here(), "figures"),
    width = w,
    height = h,
    units = "cm",
    dpi = 1000,
    bg = "white"
  )
}

predplot <- function(dat_obj, cat, facet_var,
                     tsize = 11, xtitle = "Identity"){
  x_col <- sym(paste0("can_", cat))
  
  facet_formula <- as.formula(paste0(". ~ ", facet_var))
  
  pp <- ggplot(dat_obj[[cat]], aes(
    y = estimate,
    x = !!x_col,
    colour = !!x_col,
    group = !!x_col
  )) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    facet_wrap(facet_formula) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_colour_viridis_d(
      begin = .2,
      end = .8,
      option = "mako"
    ) +
    scale_fill_viridis_d(
      begin = .2,
      end = .8,
      option = "mako"
    ) +
    theme(
      text = element_text(family = family, size = tsize),
      title = element_text(family = family),
      plot.title = element_text(
        family = family,
        hjust = .5,
        margin = margin(t = 0, r = 0, b = 10, l = 0),
        colour = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        colour = "gray90", linewidth = 0.5),
      axis.ticks.y = element_blank(),
      axis.text = element_text(
        family = family,
        colour = "black"),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 10, b = 0, l = 0),
        angle = 90,
        colour = "black",
        vjust = .5,
        hjust = .5),
      axis.title.x = element_text(
        angle = 0,
        margin = margin(t = 10, r = 0, b = 10/2, l =0),
        colour = "black"),
      plot.caption = element_text(
        family = family,
        hjust = 0,
        colour = "black"),
      plot.caption.position = "panel",
      plot.title.position = "plot",
      panel.spacing = unit(15, "points"),
      legend.position = "none"
    ) +
    labs(
      x = xtitle,
      y = "Mean GPT-3.5 Interview Invitation Score")
  
  return(pp)
}


# Predictions
het.pred <- list()

## Function to generate predictions by different heterogeneity variables
generate_predictions <- function(het_var, key) {
  categories <- c("minority", "identity", "sex")
  pred_list <- list()
  
  for (cat in categories) {
    pred_list[[cat]] <- 
      predictions(
        list.het.job[[cat]],
        vcov = ~vac_uid,
        by = c(paste0("can_", cat), het_var),
        conf_level = .95
      )
  }
  
  het.pred[[key]] <<- pred_list
}

## Define heterogeneity variables and their corresponding keys
het.vars <- list(
  occ = "can_occupation",
  jt = "vac_job_type",
  wh = "vac_work_hours",
  ss = "vac_shift_system",
  du = "vac_lang_dutch",
  fr = "vac_lang_french",
  en = "vac_lang_english",
  exp = "vac_experience_requested"
)

## Generate predictions for each heterogeneity variable and store
for (key in names(het.vars)) {
  generate_predictions(het.vars[[key]], key)
}


# Plots
## Function to generate and save plots for a given category and heterogeneity variable key
generate_and_save_plots <- function(key, cat, xtitle) {
  dat_obj <- het.pred[[key]]
  facet_var <- het.vars[[key]]
  
  varlen <- length(unique(data.csn[[facet_var]]))
  
  if (varlen > 10) {
    h <- 18
    tsize <- 9
  } else if(varlen > 6){
    h <- 12
    tsize <- 11
  } else if(varlen > 3){
    h <- 8
    tsize <- 11
  } else{
    h <- 6
    tsize <- 11
  }
  
  pp <- predplot(dat_obj = dat_obj, cat = cat, facet_var = facet_var,
                 tsize = tsize, xtitle = xtitle)
  
  name <- sprintf("%s_%s", cat, key)
  
  save(
    plot = pp,
    h = h,
    name = name,
    device = "png")
}

## Categories and their respective titles for plotting
categories <- list(
  sex = "Gender Identity",
  minority = "Ethnic Minority",
  identity = "Ethnic Identity"
)

## Iterate over the list of heterogeneity variables to generate and save plots
for (key in names(het.vars)) {
  for (cat in names(categories)) {
    generate_and_save_plots(key, cat, categories[[cat]])
  }
}
