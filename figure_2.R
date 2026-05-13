library(dplyr)
library(ggplot2)

# data
qa_results <- readRDS("qa_results_10_400_5.rds")

# Build full + subset properly
df_full <- qa_results %>% mutate(sample = "Full")
df_subset <- qa_results %>%
  filter(context_sensitive) %>%
  mutate(sample = "Subset")
df_plot <- bind_rows(df_full, df_subset)

# Summary
summary_all <- df_plot %>%
  group_by(sample, alpha) %>%
  summarise(
    mean_acc = mean(acc, na.rm = TRUE),
    mean_conf = mean(conf, na.rm = TRUE),
    mean_H = mean(H, na.rm = TRUE),
    se_acc = sd(acc, na.rm = TRUE) / sqrt(n()),
    se_H = sd(H, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  group_by(sample) %>%
  mutate(
    H0 = mean_H[alpha == 1],
    delta_H = mean_H - H0,
    H_base = mean_H[alpha == 0],
    rho_R = pmax(H_base - mean_H, 0) / H_base,
    miscal = mean_acc - mean_conf
  ) %>%
  ungroup() %>%
  mutate(alpha = factor(alpha, levels = c(1, 0.5, 0.3, 0.1, 0))) %>%
  arrange(match(sample, c("Subset", "Full")), alpha)

legend_scales <- list(
  scale_color_manual(
    name = "Sample",
    values = c("Full" = "black", "Subset" = "grey60"),
    breaks = c("Full", "Subset")
  ),
  scale_linetype_manual(
    name = "Sample",
    values = c("Full" = "solid", "Subset" = "dashed"),
    breaks = c("Full", "Subset")
  ),
  scale_shape_manual(
    name = "Sample",
    values = c("Full" = 16, "Subset" = 21),
    breaks = c("Full", "Subset")
  ),
  scale_fill_manual(
    values = c("Full" = "black", "Subset" = "white"),
    guide = "none"
  ),
  scale_linewidth_manual(
    name = "Sample",
    values = c("Full" = 0.7, "Subset" = 0.5),
    breaks = c("Full", "Subset")
  ),
  scale_size_manual(
    name = "Sample",
    values = c("Full" = 1.7, "Subset" = 1.5),
    breaks = c("Full", "Subset")
  )
)

pA <- ggplot(
  summary_all,
  aes(
    x = alpha,
    y = rho_R,
    color = sample,
    linetype = sample,
    shape = sample,
    fill = sample,
    group = sample
  )
) +
  geom_line(aes(linewidth = sample)) +
  geom_point(aes(size = sample)) +
  coord_fixed(ratio = 1) +
  legend_scales +
  scale_y_continuous(
    limits = c(-0.02, 1.13),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = 0.03)
  ) +
  labs(y = expression("Resolution ratio (" * rho[R] * ")")) +
  labs(x = expression(alpha ~ "(" * all %->% none * ")")) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

pC <- ggplot(
  summary_all,
  aes(
    x = alpha,
    y = -miscal,
    color = sample,
    linetype = sample,
    shape = sample,
    fill = sample,
    group = sample
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_line(aes(linewidth = sample)) +
  geom_point(aes(size = sample)) +
  coord_fixed(ratio = 1) +
  legend_scales +
  scale_y_continuous(
    limits = c(-0.02, 1.13),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = 0.03)
  ) +
  labs(y = "Overconfidence (c - acc)") +
  labs(x = expression(alpha ~ "(" * all %->% none * ")")) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

pB <- ggplot(
  summary_all,
  aes(
    x = mean_conf,
    y = mean_acc,
    color = sample,
    linetype = sample,
    shape = sample,
    fill = sample,
    group = sample
  )
) +
  annotate(
    "segment",
    x = 0,
    y = 0,
    xend = 1,
    yend = 1,
    linetype = "dashed",
    linewidth = 0.4,
    color = "grey50",
    alpha = 0.6
  ) +
  geom_line(
    data = subset(summary_all, sample == "Subset"),
    aes(linewidth = sample)
  ) +
  geom_point(
    data = subset(summary_all, sample == "Subset"),
    aes(size = sample)
  ) +
  geom_line(
    data = subset(summary_all, sample == "Full"),
    aes(linewidth = sample)
  ) +
  geom_point(data = subset(summary_all, sample == "Full"), aes(size = sample)) +

  geom_text(
    data = subset(summary_all, sample == "Full" & alpha == 1),
    aes(label = paste0("α = ", alpha)),
    hjust = 1.2,
    vjust = 0.5,
    size = 3.5,
    color = "black",
    show.legend = FALSE
  ) +
  geom_text(
    data = subset(summary_all, sample == "Full" & alpha == 0),
    aes(label = paste0("α = ", alpha)),
    hjust = -0.2,
    vjust = 0.5,
    size = 3.5,
    color = "black",
    show.legend = FALSE
  ) +

  annotate(
    "rect",
    xmin = 0,
    xmax = 1.0,
    ymin = 0,
    ymax = 1.0,
    fill = "grey95",
    alpha = 0.2
  ) +

  legend_scales +

  coord_fixed(ratio = 1) +
  scale_x_continuous(
    limits = c(-0.02, 1.13),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = 0.03)
  ) +
  scale_y_continuous(
    limits = c(-0.02, 1.13),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = 0.03)
  ) +

  labs(x = "Confidence (c)", y = "Accuracy (acc)") +

  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box.spacing = unit(1, "pt"),
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

library(patchwork)

pA <- pA +
  annotate(
    "text",
    x = 0,
    y = 1.13,
    label = " A",
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 4
  )
pB <- pB +
  annotate(
    "text",
    x = 0,
    y = 1.13,
    label = "B",
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 4
  )
pC <- pC +
  annotate(
    "text",
    x = 0,
    y = 1.13,
    label = " C",
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 4
  )

p2 <- pA +
  pB +
  pC +
  plot_layout(nrow = 1)


ggsave("figure_2.pdf", p2, width = 10, height = 4, device = cairo_pdf)
