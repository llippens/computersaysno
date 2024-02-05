# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, install = TRUE, update = FALSE)

# Setup
source(file.path(here(), "code", "plots", "csn_fig_setup.R"))

# Frequencies
score_freqs <- data.csn %>%
  group_by(can_minority, gpt_score) %>%
  summarise(freq = n())

total_counts <- score_freqs %>%
  group_by(can_minority) %>%
  summarise(total = sum(freq))

score_freqs <- left_join(score_freqs, total_counts, by = "can_minority") %>%
  ungroup()

score_freqs <- score_freqs %>%
  mutate(relative_freq = freq / total)

binned_data <- score_freqs %>%
          mutate(can_minority = case_when(can_minority == "Yes" ~ "Minority",
                                          can_minority == "No" ~ "Majority"),
                 score_bin = cut_interval(gpt_score, length = 5)) %>%
          arrange(score_bin) %>%
          group_by(can_minority, score_bin) %>%
          summarise(relative_freq = sum(relative_freq)) %>%
          mutate(score_bin = case_when(can_minority == "Majority" &
                                         score_bin == "[5,10]" ~
                                         "(5,10]",
                                       TRUE ~ score_bin)) %>%
  mutate(score_mid = seq(2.5, 97.5, 5))

custom_breaks <- seq(2.5, 97.5, 5)
custom_labels <- binned_data$score_bin %>% unique()
names(custom_labels) <- custom_breaks

# Plot
dual_histogram <-
  ggplot(binned_data, aes(x = score_mid, y = relative_freq,
                          colour = can_minority, fill = can_minority)) +
  geom_bar(stat = "identity", position = position_dodge(width = -2),
           width = 5, alpha = .25,
           linewidth = .5) +
  scale_x_continuous(breaks = custom_breaks,
                     labels = custom_labels,
                     limits = c(0, 100)) +
  scale_y_percent(breaks = seq(0, .6, .1)) +
  geom_vline(xintercept = seq(0, 100, 10),
             color = "gray90", linewidth = 0.25) +
  #scale_fill_manual(values = colors) +
  #scale_colour_manual(values = colors) +
  scale_fill_viridis(begin = colb, end = cole,
                     discrete = TRUE, option = colopt) +
  scale_colour_viridis(begin = colb, end = cole,
                       discrete = TRUE, option = colopt) +
  labs(x = "GPT-3.5 Interview Invitation Score",
       y = paste0("Relative", "\n",
                  "Frequency")) +
  geom_label(aes(x = 64.5, y = .32, label = "Other"),
             color = minority_color, hjust = 1, family = family,
             fill = "white",
             label.padding = unit(0, "lines"),
             label.r = unit(0, "lines"),
             label.size = NA,
             size = 3) +
  geom_label(aes(x = 70.5, y = .35, label = "Dutch"),
             color = majority_color, hjust = 0, family = family,
             fill = "white",
             label.padding = unit(0, "lines"),
             label.r = unit(0, "lines"),
             label.size = NA,
             size = 3) +
  theme_minimal() + 
  theme(title = element_text(family = family),
        plot.title = element_text(
          family = family,
          size = title.size,
          hjust = .5,
          margin = margin(t = 0, r = 0, b = text.size, l = 0),
          colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(
          colour = "gray90", linewidth = 0.25),
        axis.ticks = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(
          family = family,
          size = text.size,
          colour = "black"),
        axis.text.x = element_text(
          size = 6.5),
        axis.title.y = element_text(
          margin = margin(t = 0, r = text.size, b = 0, l = 0),
          angle = 0,
          colour = "black",
          size = axis.title.size,
          vjust = 1,
          hjust = 0),
        axis.title.x = element_text(
          angle = 0,
          margin = margin(t = text.size, r = 0, b = text.size, l =0),
          colour = "black",
          size = axis.title.size),
        plot.caption = element_text(
          family = family,
          size = caption.size,
          hjust = 0,
          colour = "black"),
        plot.caption.position = "panel",
        plot.title.position = "plot",
        panel.spacing = unit(10, "points"),
        legend.position = "none",
        plot.margin = margin(t = text.size*1.5, r = 0, b = 0, l = 0))

print(dual_histogram)

# Save
save <- function(plot, device){
  ggsave(
    filename = paste0("csn_fig1.", device),
    plot = plot,
    device = device,
    path = file.path(here(), "figures"),
    width = 17,
    height = 6.5,
    units = "cm",
    dpi = 1000,
    bg = "white"
  )
}

save(dual_histogram, device = "png")