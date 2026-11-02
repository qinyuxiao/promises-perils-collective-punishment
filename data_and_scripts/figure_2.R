# This script creates the figure that shows the pilot study's data

library(ggtext)

# ---- Mind Game behaviors (Panel A) ----

conditions <- c("Baseline", "CP", "Norm")
d_ests <- c(0.3966, 0.3164, 0.2146)
lowers <- c(0.3430, 0.2625, 0.1627)
uppers <- c(0.4501, 0.3703, 0.2664)
ds <- data.frame(conditions, d_ests, lowers, uppers)

panel_a <- ds |>
  ggplot(aes(x = conditions, y = d_ests)) +
  geom_pointrange(ymin = lowers, ymax = uppers) +
  geom_signif(
    comparisons = list(c(1, 2), c(2, 3)),
    annotations = c("*", "**"),
    y_position = c(0.46, 0.4)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.8, 0.1),
    limits = c(0.1, 0.5),
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "",
    title = "Mind Game Behaviors",
    subtitle = expression("estimated proportion of dishonest individuals ("*italic("d")*")")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(8, units = "pt"),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

# ---- Computing summary statistics for norm measures ----

dat_summ <- dat |>
  mutate(
    condition = factor(
      condition,
      levels = c("baseline", "cp", "norm"),
      labels = c("Baseline", "CP", "Norm")
    )
  ) |>
  group_by(condition) |>
  summarise(
    n = n(),
    ee_mean = mean(ee),
    ee_sd = sd(ee),
    ee_se = ee_sd / sqrt(n),
    ee_cil = ee_mean - qt(0.975, df = n - 1) * ee_se,
    ee_ciu = ee_mean + qt(0.975, df = n - 1) * ee_se
  )

# ---- Empirical expectations (Panel B) ----

panel_b <- dat |>
  mutate(
    condition = factor(
      condition,
      levels = c("baseline", "cp", "norm"),
      labels = c("Baseline", "CP", "Norm")
    )
  ) |>
  ggplot(aes(x = condition, y = ee)) +
  stat_eye(
    side = "both",
    point_interval = NULL,
    width = 0.5,
    fill = "#7DB8E8"
  ) +
  geom_pointrange(
    data = dat_summ,
    aes(x = condition, y = ee_mean, ymin = ee_cil, ymax = ee_ciu),
    size = 0.2
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "",
    title = "Empirical Expectations",
    subtitle = "number of participants (out of 100) that would lie"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(8, units = "pt"),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

# ---- Personal normative beliefs and normative expectations (Panel C) ----

bottom_texts <- c(
  "Personal Normative Belief" = "personal appropriateness of lying",
  "Normative Expectation" = "social appropriateness of lying"
)

lab_fun <- function(x) {
  paste0(
    "<span style='font-size:14pt; display:block; text-align:center;'>",
    x,
    "</span><br>",
    "<span style='font-size:0pt; line-height:3pt;'> </span><br>",
    "<span style='font-size:10pt; display:block; text-align:center;'>",
    bottom_texts[x],
    "</span><br>",
    "<span style='font-size:0pt; line-height:0.5pt;'> </span>"
  )
}

panel_c <- dat |>
  select(id, condition, pn, ne) |>
  mutate(
    condition = factor(
      condition,
      levels = c("baseline", "cp", "norm"),
      labels = c("Baseline", "CP", "Norm")
    ),
    across(
      c(pn, ne),
      function(x) factor(
        x, levels = 6:1,
        labels = c(
          "Very (Socially) Appropriate",
          "(Socially) Appropriate",
          "Somewhat (Socially) Appropriate",
          "Somewhat (Socially) Inappropriate",
          "(Socially) Inappropriate",
          "Very (Socially) Inappropriate"
        )
      )
    )
  ) |>
  pivot_longer(
    cols = c(pn, ne),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = factor(
      measure,
      levels = c("pn", "ne"),
      labels = c("Personal Normative Belief", "Normative Expectation")
    )
  ) |>
  ggplot(aes(x = condition, fill = value)) +
  geom_bar(position = "fill", width = 0.7) +
  facet_wrap(
    ~ measure,
    nrow = 1,
    labeller = labeller(measure = lab_fun)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    labels = scales::percent_format(scale = 100),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c(
    "#E1F3FF", "#B3DAFF", "#7DB8E8", "#4A90D9", "#1F5FBF", "#08306B"
  )) +
  labs(
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(8, units = "pt"),
    axis.text.x = element_text(size = 14),
    strip.text = element_markdown(hjust = 0.5, vjust = 0.5)
  )

# ---- Combine plots ----

panel_a_b <- cowplot::plot_grid(
  panel_a, panel_b,
  nrow = 1,
  rel_widths = c(1, 1),
  labels = c("A", "B")
)

fig <- cowplot::plot_grid(
  panel_a_b, panel_c,
  ncol = 1,
  rel_heights = c(1, 1),
  labels = c("", "C")
)

# ---- Output ----

# ggsave(
#   filename = "output/fig_2.png",
#   plot = fig,
#   width = 8,
#   height = 8,
#   units = "in",
#   dpi = "retina"
# )
