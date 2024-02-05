if (!require("pacman")) install.packages("pacman")
pacman::p_load(tibble, dplyr, glue, install = TRUE, update = FALSE)

p_adj_transformer <- function(base_data, i, mtd = "holm") {
  as_tibble(modelsummary_cstm(base_data, out = "data.frame", est = "{estimate}", bs.type = "none")) %>%
    rename(estimate = `(1)`) %>%
    left_join(p_adjust_cstm(base_data, bs.type = "none", mtd = mtd), by = "term") %>%
    mutate(stars = case_when(p_adjust < .001 ~ "***",
                             p_adjust < .01 ~ "**",
                             p_adjust < .05 ~ "*",
                             p_adjust >= .05 ~ "",
                             is.na(p_adjust) ~ "")) %>%
    relocate(stars, .after = p_adjust)
}