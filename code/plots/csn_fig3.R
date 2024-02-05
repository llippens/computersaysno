# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, install = TRUE, update = FALSE)

# Setup
source(file.path(here(), "code", "plots", "csn_fig_setup.R"))

# Frequencies
name_freq_table <- data.csn %>%
  group_by(can_identity, can_sex, can_name) %>%
  summarise(freq = n(),
            mscore = mean(gpt_score)) %>%
  ungroup()

labels <- name_freq_table %>%
  group_by(can_identity, can_sex) %>%
  summarise(
    min_mscore = min(mscore),
    max_mscore = max(mscore),
    name_min = can_name[which.min(mscore)],
    name_max = can_name[which.max(mscore)]
  )

id_means <- data.csn %>%
  group_by(can_identity, can_sex) %>%
  summarise(mscore = mean(gpt_score)) %>%
  ungroup()

# Plot
name.score.plot <- function(data, idmeans, labelsdata, title){
  ggplot(data = data,
         aes(x = can_identity, y = mscore, colour = can_identity)) +
    geom_jitter(width = .1, height = 0, alpha = .25) +
    geom_point(data = idmeans,
               shape = 108, size = 5) +
    scale_y_continuous(limits = c(55, 75),
                       breaks = seq(55, 75, 5)) +
    scale_colour_manual(values = c(majority_color,
                                   rep(minority_color, 8))) +
    coord_flip() +
    theme_minimal() +
    theme(plot.title = element_text(family = family,
                                    hjust = 0.5,
                                    colour = "gray20"),
          text = element_text(family = family, size = 11),
          panel.grid.major = element_line(
            colour = "gray90", linewidth = 0.25),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_line(
            colour = "gray90", linewidth = .3),
          axis.ticks.y = element_blank(),
          axis.text = element_text(family = family, size = 11),
          axis.title.x = element_text(
            margin = margin(t = 10, r = 0, b = 0, l = 0),
            colour = "gray20",
            size = 12),
          strip.text = element_text(
            size = 11,
            angle = 0 
          ),
          plot.title.position = "panel",
          panel.spacing = unit(10, "points"),
          legend.position = "none") +
    labs(x = "",
         y = "Mean GPT-3.5 Interview Invitation Score",
         title = title)
}

name.score.arr.plot <- ggarrange(
  name.score.plot(data = name_freq_table %>%
                    filter(can_sex == "Male"),
                  idmeans = id_means %>%
                    filter(can_sex == "Male"),
                  labelsdata = labels %>%
                    filter(can_sex == "Male"),
                  title = "Male") +
    theme(plot.margin = margin(t = 10, r = 10, b = 10*1.5, l = 0)),
  name.score.plot(data = name_freq_table %>%
                    filter(can_sex == "Female"),
                  idmeans = id_means %>%
                    filter(can_sex == "Female"),
                  labelsdata = labels %>%
                    filter(can_sex == "Female"),
                  title = "Female") +
    theme(plot.margin = margin(t = 10*1.5, r = 0, b = 10, l = 0)),
  align = "v",
  ncol = 1,
  nrow = 2,
  labels = c("A", "B"),
  font.label = list(size = 10,
                    color = "gray20",
                    face = "bold",
                    family = "helvetica")
)

print(name.score.arr.plot)

# Save
save <- function(plot, device){
  ggsave(
    filename = paste0("csn_fig3", ".", device),
    plot = name.score.arr.plot,
    path = file.path(here(), "figures"),
    device = device,
    width = 17,
    height = 16,
    units = "cm",
    dpi = dpi,
    bg = bg
  )
}

save(dual_histogram, device = "png")