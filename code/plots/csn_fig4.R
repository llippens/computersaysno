# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, install = TRUE, update = FALSE)

# Setup
source(file.path(here(), "code", "plots", "csn_fig_setup.R"))
source(file.path(here(), "code", "4_csn_sensitivity.R"))

save <- function(plot, name, w = 12, h = 12, device){
  ggsave(
    filename = paste0("csn_fig4", name, ".", device),
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


# Sensitivity
## Sensitivity by ethnic minority
p.min <- ggplot(data = predictions.t %>%
                  filter(base == "minority"),
                aes(x = cutoff,
                    y = predicted, 
                    ymin = conf.low, ymax = conf.high,
                    colour = identity,
                    fill = identity)) +
  geom_line(linewidth = .75) +
  geom_ribbon(alpha = 0.15, colour = NA) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_percent(breaks = seq(0,1,.1),
                  minor_breaks = NULL,
                  accuracy = 1) +
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  geom_label(data = predictions.t %>%
               filter(base == "minority",
                      cutoff == 60,
                      identity == "Yes"),
             aes(label = label,
                 y = .67),
             fill = "white",
             label.padding = unit(0.05, "lines"),
             label.r = unit(0, "lines"),
             label.size = NA,
             hjust = 0.5,
             family = family,
             size = 3) +
  geom_label(data = predictions.t %>%
               filter(base == "minority",
                      cutoff == 60,
                      identity == "No"),
             aes(label = label,
                 y = .78),
             fill = "white",
             label.padding = unit(0, "lines"),
             label.r = unit(0, "lines"),
             label.size = NA,
             hjust = 0.5,
             family = family,
             size = 3) +
  theme_minimal() +
  theme(title = element_text(family = family),
        text = element_text(family = family, size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          colour = "gray90", linewidth = 0.25),
        axis.ticks.x = element_line(
          colour = "gray90", linewidth = .3),
        axis.ticks.y = element_blank(),
        axis.text = element_text(family = family, size = 11),
        axis.title.y = element_text(
          margin = margin(t = 0, r = 10, b = 0, l = 0),
          angle = 90,
          colour = "gray20",
          size = 12,
          vjust = .5,
          hjust = .5),
        axis.title.x = element_text(
          angle = 0,
          margin = margin(t = 10, r = 0, b = 0, l =0),
          colour = "gray20",
          size = 12),
        strip.text.y.right = element_text(
          angle = 0, hjust = 0.5),
        plot.title.position = "plot",
        panel.spacing = unit(10, "points"),
        legend.position = "none"
  ) +
  labs(x = "Score cutoff",
       y = paste0("Mean GPT-3.5 Interview Invitation Probability"))

print(p.min)

save(p.min, name = "_min_sens", device = "png")

## Sensitivity by ethnic identity
data.ide <- predictions.t %>%
  filter(base == "identity") %>%
  mutate(identity =
           fct_relevel(as.factor(identity),
                       predictions.t %>%
                         filter(cutoff == 55, !(identity %in% c("No", "Yes"))) %>%
                         arrange(-predicted) %>% select(identity) %>%
                         pull()
           ))

p.ide <- ggplot(data = data.ide,
                aes(x = cutoff,
                    y = predicted, 
                    ymin = conf.low, ymax = conf.high,
                    colour = identity,
                    fill = identity)) +
  geom_line(linewidth = .75) +
  geom_ribbon(alpha = 0.15,
              colour = NA) +
  scale_x_continuous(limits = c(49, 80),
                     breaks = seq(0, 100, 10)) +
  scale_y_percent(breaks = seq(0,1,.1),
                  minor_breaks = NULL,
                  accuracy = 1) +
  scale_colour_manual(values =
                        c(colors[[1]], rep(colors[[2]], 8))) +
  scale_fill_manual(values =
                      c(colors[[1]], rep(colors[[2]], 8))) +
  geom_label(data = data.ide %>%
               filter(base == "identity",
                      cutoff == 69) %>%
               arrange(-predicted),
             aes(label = identity,
                 y = c(.76, seq(.73, .59, -.02)),
                 x = 70),
             fill = "white",
             label.padding = unit(0.05, "lines"),
             label.r = unit(0, "lines"),
             label.size = NA,
             hjust = 0,
             family = family,
             size = 2.5) +
  coord_cartesian(ylim = c(.10, .80)) +
  theme_minimal() +
  theme(title = element_text(family = family),
        text = element_text(family = family, size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          colour = "gray90", linewidth = 0.25),
        axis.ticks.x = element_line(
          colour = "gray90", linewidth = .3),
        axis.ticks.y = element_blank(),
        axis.text = element_text(family = family, size = 11),
        axis.title.y = element_text(
          margin = margin(t = 0, r = 10, b = 0, l = 0),
          angle = 90,
          colour = "gray20",
          size = 12,
          vjust = .5,
          hjust = .5),
        axis.title.x = element_text(
          angle = 0,
          margin = margin(t = 10, r = 0, b = 0, l =0),
          colour = "gray20",
          size = 12),
        strip.text.y.right = element_text(
          angle = 0, hjust = 0.5),
        plot.title.position = "plot",
        panel.spacing = unit(10, "points"),
        legend.position = "none"
  ) +
  labs(x = "Score cutoff",
       y = paste0("Mean GPT-3.5 Interview Invitation Probability"))

print(p.ide)

save(p.ide, w = 8, name = "_ide_sens", device = "png")

## Arranged plots
p.arr <- ggarrange(
  p.min +
    theme(plot.margin = margin(t = 11, r = 11*1.5, b = 11, l = 0)),
  p.ide +
    theme(plot.margin = margin(t = 11, r = 11, b = 11, l = 11*1.5)),
  align = "v",
  ncol = 2,
  nrow = 1,
  widths = c(1.3,1),
  labels = c("A", "B"),
  font.label = list(size = 11,
                    color = "gray20",
                    face = "bold",
                    family = "helvetica")
)

print(p.arr)

save(p.arr, w = 17, h = 12, name = NULL, device = "png")
