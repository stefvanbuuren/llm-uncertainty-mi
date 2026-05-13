library(dplyr)
library(ggplot2)

qa <- readRDS("qa_results_10_400_5.rds")

H_none_df <- qa %>% filter(alpha == 0) %>% select(id, H_none = H)

df <- qa %>%
  left_join(H_none_df, by = "id") %>%
  mutate(
    rho_R  = if_else(H_none > 0, pmax(H_none - H, 0) / H_none, 0),
    sample = if_else(context_sensitive, "Context-sensitive", "Full sample")
  ) %>%
  filter(alpha %in% c(0.1, 0.3, 0.5)) %>%
  mutate(alpha = factor(alpha, levels = c(0.1, 0.3, 0.5)))

# 1x4: Full-conf, Full-RR, CS-conf, CS-RR
df_long <- bind_rows(
  df %>% filter(sample == "Full sample")        %>% mutate(panel = "Full sample",        predictor = "conf", x = conf),
  df %>% filter(sample == "Full sample")        %>% mutate(panel = "Full sample",        predictor = "rho",  x = rho_R),
  df %>% filter(sample == "Context-sensitive")  %>% mutate(panel = "Context-sensitive",  predictor = "conf", x = conf),
  df %>% filter(sample == "Context-sensitive")  %>% mutate(panel = "Context-sensitive",  predictor = "rho",  x = rho_R)
) %>%
  mutate(panel_id = factor(
    paste(panel, predictor),
    levels = c("Full sample conf", "Full sample rho",
               "Context-sensitive conf", "Context-sensitive rho")
  ))

make_panel <- function(data, x_lab, sample_label, show_y = TRUE, show_legend = FALSE) {
  ggplot(data, aes(x = x, y = acc, color = alpha)) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.8, span = 0.9) +
    annotate("text", x = 0.02, y = 0.97, label = sample_label,
             hjust = 0, vjust = 1, size = 3.5, fontface = "plain") +
    scale_color_viridis_d(option = "D", end = 0.85,
                          labels = c("0.1", "0.3", "0.5")) +
    guides(color = guide_legend(title = expression(alpha))) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    coord_fixed(ratio = 1) +
    labs(x = x_lab, y = if (show_y) "Accuracy" else NULL) +
    theme_bw(base_size = 11) +
    theme(
      legend.position  = if (show_legend) "right" else "none",
      panel.grid.minor = element_blank(),
      axis.title.y     = if (show_y) element_text() else element_blank(),
      axis.text.y      = if (show_y) element_text() else element_blank()
    )
}

p1 <- make_panel(df_long %>% filter(panel_id == "Full sample conf"),
                 "Confidence (c)", "Full sample", show_y = TRUE)
p2 <- make_panel(df_long %>% filter(panel_id == "Full sample rho"),
                 expression("Resolution ratio " * rho[R](alpha)), "Full sample", show_y = FALSE)
p3 <- make_panel(df_long %>% filter(panel_id == "Context-sensitive conf"),
                 "Confidence (c)", "Context-sensitive", show_y = FALSE)
p4 <- make_panel(df_long %>% filter(panel_id == "Context-sensitive rho"),
                 expression("Resolution ratio " * rho[R](alpha)), "Context-sensitive", show_y = FALSE,
                 show_legend = TRUE)

library(patchwork)
p <- p1 + p2 + p3 + p4 + plot_layout(nrow = 1)

ggsave("figure_3.pdf", p, width = 11, height = 3.5, device = "pdf")
cat("Saved figure_3.pdf\n")
