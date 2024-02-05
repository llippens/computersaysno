# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, install = TRUE, update = FALSE)

# Setup
source(file.path(here(), "code", "plots", "csn_fig_setup.R"))
source(file.path(here(), "functions", "f_ggplot_prediction_lnr.R"))

# Model
model.link <- ". ~ ."

gpt.vars.c <- c( # GPT3 controls
  "gpt_temperature_fct"
)

lnr.frml <- "gpt_score ~ "

data.csn$can_identity <- fct_relevel(data.csn$can_identity,
                                    data.csn %>%
                                      group_by(can_identity) %>%
                                      summarise(penalty = mean(gpt_score)) %>%
                                      arrange(-penalty) %>%
                                      select(can_identity) %>%
                                      pull() %>%
                                      as.character()
)

lnr.base.i <- lm(paste0(lnr.frml, "can_identity"),
                 data = data.csn)

lnr.i <- lnr.base.i %>%
  update(as.formula(
    paste(model.link,
          paste0(gpt.vars.c, collapse = " + "),
          sep = " + ")
  ))

# Plot
lnr.i.plot <- lnr.i %>%
  ggplot_prediction_lnr(term = "can_identity",
                        colours = c(majority_color,
                                    rep(minority_color, 8)))

print(lnr.i.plot)

# Save
save <- function(plot, device){
  ggsave(
    filename = paste0("csn_fig2.", device),
    plot = plot,
    device = device,
    path = file.path(here(), "figures"),
    width = 12.7,
    height = 6,
    units = "cm",
    dpi = 1000,
    bg = "white"
  )
}

save(lnr.i.plot, device = "png")