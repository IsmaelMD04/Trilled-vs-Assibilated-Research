library(tidyverse)
library(ggplot2)

# Load data from analysis: t7, fa, attitude_by_region
load("data/viz_objects.RData")

# Trill condition colors
trill_colors <- c("0" = "#E07A5F", "1" = "#3D6B8C")
trill_labels <- c("0" = "Assibilated", "1" = "Trilled")

region_colors <- c(
  "Coast"  = "#5B8DB8",
  "Cuenca" = "#C97C4A",
  "Loja"   = "#6BAE75",
  "Quito"  = "#9B6BB5"
)

poster_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle    = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.title       = element_text(face = "bold", size = 12),
    axis.text        = element_text(size = 11),
    legend.title     = element_text(face = "bold", size = 11),
    legend.text      = element_text(size = 11),
    strip.text       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(10, 15, 10, 15)
  )


# Status by trill condition
# Violin + boxplot showing the overall prestige effect of the trill.

t7 |>
  mutate(trill = factor(trill)) |>
  ggplot(aes(x = trill, y = status, fill = trill)) +
  geom_violin(alpha = 0.6, trim = TRUE, color = NA) +
  geom_boxplot(width = 0.18, outlier.shape = NA,
               color = "white", fill = "white", alpha = 0.5) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  scale_fill_manual(values = trill_colors, labels = trill_labels) +
  scale_x_discrete(labels = trill_labels) +
  labs(
    title    = "Perceived Status by /r/ pronounciation",
    subtitle = "Trilled speakers rated as higher status across all listeners",
    x        = "/r/ pronounciation",
    y        = "Perceived Status (composite z-score)",
    fill     = "/r/ pronounciation"
  ) +
  poster_theme +
  theme(legend.position = "none")

ggsave("images/plot1_status_by_trill.png", width = 6, height = 5, dpi = 300)


# Status by trill x region
# Grouped boxplot showing the status effect is uniform across regions.

t7 |>
  mutate(trill = factor(trill)) |>
  ggplot(aes(x = Region, y = status, fill = trill)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21,
               outlier.size = 1.5, outlier.alpha = 0.4,
               position = position_dodge(0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3,
               color = "white", position = position_dodge(0.75)) +
  scale_fill_manual(values = trill_colors, labels = trill_labels) +
  labs(
    title    = "Perceived Status by /r/ pronounciation and Listener Region",
    subtitle = "Status boost from trill is consistent across all regions",
    x        = "Listener Region",
    y        = "Perceived Status (composite z-score)",
    fill     = "/r/ pronounciation"
  ) +
  poster_theme

ggsave("images/plot2_status_by_trill_region.png", width = 8, height = 5, dpi = 300)


# % guessed Loja by trill x region
# Grouped bar with error bars showing correct Loja attribution by region.

loja_summary <- t7 |>
  mutate(trill = factor(trill)) |>
  group_by(Region, trill) |>
  summarize(
    pct_loja = mean(guessed_loja, na.rm = TRUE),
    n        = n(),
    se       = sqrt(pct_loja * (1 - pct_loja) / n),
    .groups  = "drop"
  )

loja_summary |>
  ggplot(aes(x = Region, y = pct_loja, fill = trill)) +
  geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = pct_loja - se, ymax = pct_loja + se),
                position = position_dodge(0.75),
                width = 0.2, color = "gray30") +
  scale_fill_manual(values = trill_colors, labels = trill_labels) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.65)) +
  labs(
    title    = "Correct Loja Attribution by /r/ pronounciation and Listener Region",
    subtitle = "Trill nearly doubles odds of correct identification (OR = 1.73, p < .001)",
    x        = "Listener Region",
    y        = "% Guessing Loja",
    fill     = "/r/ pronounciation"
  ) +
  poster_theme

ggsave("images/plot3_loja_attribution_by_region.png", width = 8, height = 5, dpi = 300)


# PLOT 4: LOJA VS QUITO ATTRIBUTION BY TRILL CONDITION
# Side-by-side bar showing trill = Loja cue, not Quito cue.

t7 |>
  mutate(trill = factor(trill)) |>
  group_by(trill) |>
  summarize(
    Loja  = mean(guessed_loja,  na.rm = TRUE),
    Quito = mean(guessed_quito, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(cols = c(Loja, Quito),
               names_to  = "Attributed_City",
               values_to = "proportion") |>
  ggplot(aes(x = Attributed_City, y = proportion, fill = trill)) +
  geom_col(position = position_dodge(0.65), width = 0.55, alpha = 0.9) +
  scale_fill_manual(values = trill_colors, labels = trill_labels) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.5)) +
  labs(
    title    = "Speaker Attribution: Loja vs. Quito by /r/ pronounciation",
    subtitle = "Trill increases Loja attribution but not Quito attribution",
    x        = "Attributed City",
    y        = "Proportion of Responses",
    fill     = "/r/ pronounciation"
  ) +
  poster_theme

ggsave("images/plot4_loja_vs_quito_attribution.png", width = 6, height = 5, dpi = 300)


# PLOT 5: FACTOR ANALYSIS LOADINGS
# Lollipop plot showing the two-factor structure from fa (ClaraDraft.R line 162).

as.data.frame(fa$loadings[, 1:2]) |>
  rownames_to_column("Variable") |>
  rename(Status = Factor1, Gendered_Affect = Factor2) |>
  pivot_longer(cols = c(Status, Gendered_Affect),
               names_to  = "Factor",
               values_to = "Loading") |>
  mutate(
    Variable = recode(Variable,
                      "masculinity" = "Masculinity",
                      "nice"        = "Niceness",
                      "class"       = "Class",
                      "urban"       = "Urban",
                      "edu"         = "Education",
                      "age"         = "Age"
    ),
    Factor = recode(Factor,
                    "Status"          = "Factor 1: Status",
                    "Gendered_Affect" = "Factor 2: Gendered Affect"
    )
  ) |>
  ggplot(aes(x = Loading, y = reorder(Variable, Loading), color = Factor)) +
  geom_segment(aes(x = 0, xend = Loading,
                   y = reorder(Variable, Loading),
                   yend = reorder(Variable, Loading)),
               linewidth = 1.2, alpha = 0.7) +
  geom_point(size = 4) +
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") +
  facet_wrap(~ Factor, scales = "free_x") +
  scale_color_manual(values = c(
    "Factor 1: Status"          = "#3D6B8C",
    "Factor 2: Gendered Affect" = "#E07A5F"
  )) +
  labs(
    title    = "Factor Analysis Loadings",
    subtitle = "Varimax rotation — 2 factors extracted from listener ratings",
    x        = "Loading",
    y        = "Rating Variable",
    color    = "Factor"
  ) +
  poster_theme +
  theme(legend.position = "none")

ggsave("images/plot5_factor_loadings.png", width = 9, height = 5, dpi = 300)


# PLOT 6: NICENESS AND MASCULINITY BY TRILL × REGION
# Faceted bar showing no significant personality effect by region or trill.

attitude_by_region |>
  mutate(trill = factor(trill)) |>
  select(Region, trill, mean_nice, mean_masculinity) |>
  pivot_longer(cols = c(mean_nice, mean_masculinity),
               names_to  = "Dimension",
               values_to = "Mean_Rating") |>
  mutate(Dimension = recode(Dimension,
                            "mean_nice"        = "Niceness",
                            "mean_masculinity" = "Masculinity"
  )) |>
  ggplot(aes(x = Region, y = Mean_Rating, fill = trill)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.9) +
  scale_fill_manual(values = trill_colors, labels = trill_labels) +
  scale_y_continuous(limits = c(0, 6)) +
  facet_wrap(~ Dimension, scales = "free_y") +
  labs(
    title    = "Personality Ratings by /r/ pronounciation and Listener Region",
    subtitle = "No significant trill effect on niceness or masculinity by region",
    x        = "Listener Region",
    y        = "Mean Rating (1-6 scale)",
    fill     = "/r/ pronounciation"
  ) +
  poster_theme

ggsave("images/plot6_personality_by_region.png", width = 10, height = 5, dpi = 300)

# PLOT 7: MOSAIC PLOT — PREDICTED ORIGIN BY LISTENER REGION
# Shows how each listener region distributes their origin guesses.
# Interpretation: wider columns = more listeners from that region.

t7 |>
  group_by(Region, predictedOrigin) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(Region) |>
  mutate(
    prop          = n / sum(n),
    predictedOrigin = factor(predictedOrigin,
                             levels = c("Other", "Quito", "Cuenca", "Loja"))
  ) |>
  ggplot(aes(x = Region, y = prop, fill = predictedOrigin)) +
  geom_col(width = 0.85, alpha = 0.9) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c(
    "Loja"   = "#6BAE75",
    "Quito"  = "#9B6BB5",
    "Cuenca" = "#C97C4A",
    "Other"  = "#B0B0B0"
  )) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title    = "Where Do Listeners Think Speakers Are From?",
    subtitle = "Each bar = one listener region. Color height = proportion guessing that city.",
    x        = "Listener Region",
    y        = "Proportion",
    fill     = "Guessed Origin"
  ) +
  poster_theme

ggsave("images/plot7_mosaic_origin_by_region.png", width = 8, height = 6, dpi = 300)


# PLOT 8: MOSAIC-STYLE — ORIGIN GUESSES BY TRILL × REGION
t7 |>
  mutate(trill = factor(trill, labels = c("Assibilated", "Trilled"))) |>
  group_by(Region, trill, predictedOrigin) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(Region, trill) |>
  mutate(
    prop            = n / sum(n),
    predictedOrigin = factor(predictedOrigin,
                             levels = c("Other", "Quito", "Cuenca", "Loja"))
  ) |>
  ggplot(aes(x = trill, y = prop, fill = predictedOrigin)) +
  geom_col(width = 0.85, alpha = 0.9) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c(
    "Loja"   = "#6BAE75",
    "Quito"  = "#9B6BB5",
    "Cuenca" = "#C97C4A",
    "Other"  = "#B0B0B0"
  )) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ Region, nrow = 1) +
  labs(
    title    = "Origin Guesses by Trill Condition Within Each Listener Region",
    subtitle = "Left = Assibilated, Right = Trilled. Green (Loja) grows with trill.",
    x        = "/r/ pronounciation",
    y        = "Proportion",
    fill     = "Guessed Origin"
  ) +
  poster_theme

ggsave("images/plot8_mosaic_origin_trill_by_region.png", width = 12, height = 6, dpi = 300)


# PLOT 9: BUBBLE CHART — ATTITUDE RATINGS BY TRILL × REGION
# Each bubble = one region × trill condition combination.
# X axis = mean status, Y axis = mean niceness.
# Bubble size = mean masculinity (larger = more masculine).
# Color = trill condition. Shape = region.
# Gives a single view of all three attitude dimensions simultaneously.

attitude_by_region |>
  mutate(trill = factor(trill, labels = c("Assibilated", "Trilled"))) |>
  ggplot(aes(x = mean_status, y = mean_nice,
             size = mean_masculinity,
             color = trill, shape = Region)) +
  geom_point(alpha = 0.85) +
  scale_size_continuous(range = c(5, 16), name = "Mean Masculinity") +
  scale_color_manual(values = c(
    "Assibilated" = "#E07A5F",
    "Trilled"     = "#3D6B8C"
  )) +
  scale_shape_manual(values = c(
    "Coast"  = 16,
    "Cuenca" = 17,
    "Loja"   = 15,
    "Quito"  = 18
  )) +
  labs(
    title    = "Attitude Profiles by /r/ pronounciation and Listener Region",
    subtitle = "X = perceived status, Y = niceness, bubble size = masculinity",
    x        = "Mean Perceived Status (z-score)",
    y        = "Mean Niceness Rating",
    color    = "/r/ pronounciation",
    shape    = "Listener Region"
  ) +
  poster_theme

ggsave("images/plot9_bubble_attitudes.png", width = 8, height = 6, dpi = 300)


# PLOT 10: SLOPE GRAPH — MEAN STATUS SHIFT FROM ASSIBILATED TO TRILLED BY REGION
# Each line connects a region's mean status for assibilated (left)
# to trilled (right). Shows the direction and size of the status shift
# for each listener group simultaneously.
# Accessible: lines going up = trill raises status for that region.

attitude_by_region |>
  mutate(
    trill       = factor(trill, labels = c("Assibilated", "Trilled")),
    trill_label = trill
  ) |>
  ggplot(aes(x = trill, y = mean_status,
             group = Region, color = Region)) +
  geom_line(linewidth = 1.4, alpha = 0.85) +
  geom_point(size = 4) +
  geom_text(
    data = . %>% filter(trill == "Trilled"),
    aes(label = Region),
    hjust = -0.2, fontface = "bold", size = 4
  ) +
  scale_color_manual(values = region_colors) +
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.3))) +
  labs(
    title    = "Status Shift: Assibilated vs. Trilled by Listener Region",
    subtitle = "All regions rate trilled speakers as higher status — lines slope upward",
    x        = "/r/ pronounciation",
    y        = "Mean Perceived Status (z-score)",
    color    = "Listener Region"
  ) +
  poster_theme +
  theme(legend.position = "none")

ggsave("images/plot10_slopegraph_status.png", width = 7, height = 6, dpi = 300)


# PLOT 11: HEATMAP — ORIGIN ATTRIBUTION RATES BY REGION × GUESSED CITY
# Rows = listener regions, columns = guessed cities.
# Color intensity = proportion of guesses going to that city.
# Readable at a glance: darker = more guesses to that city from that region.

t7 |>
  group_by(Region, predictedOrigin) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(Region) |>
  mutate(proportion = n / sum(n)) |>
  ungroup() |>
  mutate(predictedOrigin = factor(predictedOrigin,
                                  levels = c("Loja", "Cuenca", "Quito", "Other"))) |>
  ggplot(aes(x = predictedOrigin, y = Region, fill = proportion)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
            color = "white", fontface = "bold", size = 5) +
  scale_fill_gradientn(
    colors = c("#D9EAD3", "#6BAE75", "#2D6A4F"),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title    = "Where Does Each Listener Region Think Speakers Are From?",
    subtitle = "Percentage of guesses from each listener group directed to each city",
    x        = "Guessed Speaker Origin",
    y        = "Listener Region",
    fill     = "Proportion"
  ) +
  poster_theme

ggsave("images/plot11_heatmap_origin.png", width = 7, height = 5, dpi = 300)


# PLOT 12: HEATMAP — ORIGIN ATTRIBUTION BY REGION × TRILL CONDITION
# Extends plot 11 by splitting by trill condition.
# Shows within each listener region how the trill shifts origin guesses.

t7 |>
  mutate(trill = factor(trill, labels = c("Assibilated", "Trilled"))) |>
  group_by(Region, trill, predictedOrigin) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(Region, trill) |>
  mutate(proportion = n / sum(n)) |>
  ungroup() |>
  mutate(predictedOrigin = factor(predictedOrigin,
                                  levels = c("Loja", "Cuenca", "Quito", "Other"))) |>
  ggplot(aes(x = predictedOrigin, y = Region, fill = proportion)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradientn(
    colors = c("#D9EAD3", "#6BAE75", "#2D6A4F"),
    labels = scales::percent_format(accuracy = 1)
  ) +
  facet_wrap(~ trill) +
  labs(
    title    = "Origin Attribution by Trill Condition and Listener Region",
    subtitle = "Compare left (assibilated) to right (trilled) — green deepens for Loja with trill",
    x        = "Guessed Speaker Origin",
    y        = "Listener Region",
    fill     = "Proportion"
  ) +
  poster_theme

ggsave("images/plot12_heatmap_origin_by_trill.png", width = 11, height = 5, dpi = 300)
