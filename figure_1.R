# assumes that qa_results exist
library(dplyr)
library(tidyr)
library(ggplot2)

# data
qa_results <- readRDS("qa_results_10_400_5.rds")

summary_alpha <- bind_rows(
  qa_results %>% mutate(sample = "Full"),
  qa_results %>% filter(context_sensitive) %>% mutate(sample = "Subset")
) %>%
  mutate(sample = factor(sample, levels = c("Subset", "Full"))) %>%
  group_by(sample, alpha) %>%
  summarise(
    n = n(),
    mean_acc = mean(acc, na.rm = TRUE),
    se_acc = sd(acc, na.rm = TRUE) / sqrt(n),
    mean_H = mean(H, na.rm = TRUE),
    se_H = sd(H, na.rm = TRUE) / sqrt(n),
    mean_conf = mean(conf, na.rm = TRUE),
    .groups = "drop"
  )

df_long <- summary_alpha %>%
  pivot_longer(
    cols = c(mean_acc, mean_H, mean_conf),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    se = case_when(
      metric == "mean_acc" ~ se_acc,
      metric == "mean_H" ~ se_H,
      TRUE ~ NA_real_
    ),
    metric = recode(
      metric,
      mean_acc = "Accuracy",
      mean_H = "Entropy",
      mean_conf = "Confidence"
    ),
    alpha = factor(alpha, levels = c(1, 0.5, 0.3, 0.1, 0))
  )

p <- ggplot(
  df_long,
  aes(
    x = alpha,
    y = value,
    color = sample,
    linetype = sample,
    shape = sample,
    group = sample
  )
) +
  geom_line(aes(linewidth = sample)) +

  geom_point(aes(size = sample, fill = sample)) +

  facet_wrap(~metric, nrow = 1) +
  scale_y_continuous(limits = c(-0.02, 1.13), breaks = seq(0, 1, by = 0.2)) +

  labs(
    x = expression(alpha ~ "(" * all %->% none * ")"),
    y = NULL,
    color = NULL,
    linetype = NULL,
    shape = NULL,
    linewidth = NULL,
    size = NULL
  ) +

  # Option A: black full, faded grey dashed subset
  scale_color_manual(
    values = c("Full" = "black", "Subset" = "grey60"),
    breaks = c("Full", "Subset")
  ) +
  # Option B (navy/steel blue): uncomment and comment out Option A above
  # scale_color_manual(values = c("Full" = "#1a1a2e", "Subset" = "#6e9ab5")) +

  scale_linetype_manual(
    values = c("Full" = "solid", "Subset" = "dashed"),
    breaks = c("Full", "Subset")
  ) +
  scale_shape_manual(
    values = c("Full" = 16, "Subset" = 21),
    breaks = c("Full", "Subset")
  ) +
  scale_fill_manual(
    values = c("Full" = "black", "Subset" = "white"),
    guide = "none"
  ) +
  scale_linewidth_manual(
    values = c("Full" = 0.7, "Subset" = 0.5),
    breaks = c("Full", "Subset")
  ) +
  scale_size_manual(
    values = c("Full" = 1.7, "Subset" = 1.5),
    breaks = c("Full", "Subset")
  ) +

  theme_minimal(base_size = 12) +
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    legend.box.spacing = unit(1, "pt"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave("figure_1.pdf", plot = p, width = 10, height = 4)
