# Conditional personal and social norms

dat.l <- dat |>
  select(id, condition, match, d_score, cee_1:cne_5) |>
  pivot_longer(
    cols = cee_1:cne_5,
    names_to = c("measure", "level"),
    names_pattern = "(.*)_(.*)",
    values_to = "value"
  ) %>%
  mutate(level = as.factor(level))

# ---- Computing summary statistics ----

dat.l_summ <- dat.l %>%
  mutate(condition = relevel(as.factor(condition), ref = "baseline")) %>%
  group_by(condition, measure, level) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    cil = mean - qt(.975, df = n - 1) * se,
    ciu = mean + qt(.975, df = n - 1) * se
  ) %>%
  ungroup() %>%
  mutate(
    condition = factor(
      condition,
      levels = c("baseline", "cp", "norm"),
      labels = c("Baseline", "CP", "Norm")
    ),
    level = factor(
      level,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "0\u201310%",
        "10\u201320%",
        "20\u201330%",
        "30\u201340%",
        "40\u201350%"
      )
    )
  )

# ---- Conditional empirical expectations (Panel A) ----

panel_a <- dat.l_summ %>%
  filter(measure == "cee") %>%
  ggplot(aes(x = level, y = mean, color = condition, group = condition)) +
  geom_rect(
    aes(xmin = 2.5, xmax = 3.5, ymin = 30, ymax = 60),
    fill = "grey",
    alpha = 0.05,
    colour = NA
  ) +
  geom_point(aes(shape = condition), position = position_dodge(.3), size = 2) +
  geom_line(position = position_dodge(.3), alpha = .2) +
  geom_errorbar(
    aes(ymin = cil, ymax = ciu),
    width = .1,
    position = position_dodge(.3)
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(35, 55, 5), limits = c(30, 60)) +
  scale_color_manual(values = c("#E69F00", "#0072B2", "#009E73")) +
  labs(x = "", y = "", title = "Conditional empirical expectations") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10, hjust = .5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# ---- Conditional personal normative beliefs (Panel B) ----

panel_b <- dat.l_summ %>%
  filter(measure == "cpn") %>%
  ggplot(aes(x = level, y = mean, color = condition, group = condition)) +
  geom_rect(
    aes(xmin = 2.5, xmax = 3.5, ymin = 2.1, ymax = 3.1),
    fill = "grey",
    alpha = 0.05,
    colour = NA
  ) +
  geom_point(aes(shape = condition), position = position_dodge(.3), size = 2) +
  geom_line(position = position_dodge(.3), alpha = .2) +
  geom_errorbar(
    aes(ymin = cil, ymax = ciu),
    width = .1,
    position = position_dodge(.3)
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(2.2, 3, .2), limits = c(2.1, 3.1)) +
  scale_color_manual(values = c("#E69F00", "#0072B2", "#009E73")) +
  labs(x = "", y = "", title = "Conditional personal normative beliefs") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10, hjust = .5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# ---- Conditional normative expectations (Panel C) ----

panel_c <- dat.l_summ %>%
  filter(measure == "cne") %>%
  ggplot(aes(x = level, y = mean, color = condition, group = condition)) +
  geom_rect(
    aes(xmin = 2.5, xmax = 3.5, ymin = 2.1, ymax = 3.1),
    fill = "grey",
    alpha = 0.05,
    colour = NA
  ) +
  geom_point(aes(shape = condition), position = position_dodge(.3), size = 2) +
  geom_line(position = position_dodge(.3), alpha = .2) +
  geom_errorbar(
    aes(ymin = cil, ymax = ciu),
    width = .1,
    position = position_dodge(.3)
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(2.2, 3, .2), limits = c(2.1, 3.1)) +
  scale_color_manual(values = c("#E69F00", "#0072B2", "#009E73")) +
  labs(x = "", y = "", title = "Conditional normative expectations") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10, hjust = .5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# ---- Combine plots ----

library(patchwork)
panel_a <- panel_a + theme(legend.position = "none")
panel_c <- panel_c + theme(legend.position = "none")
fig <- panel_a + panel_b + panel_c + plot_annotation(tag_levels = "A")

# ---- Output ----

# ggsave(
#   filename = "Figures/fig_conditional_norms.png",
#   plot = fig,
#   width = 10,
#   height = 4.5,
#   units = "in",
#   dpi = "retina"
# )
