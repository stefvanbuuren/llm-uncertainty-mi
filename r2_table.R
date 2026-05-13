library(dplyr)

qa_stats <- readRDS("qa_results_10_400_5.rds")

r2_lm <- function(data) {
  data.frame(
    H_pass1    = summary(lm(pass_1 ~ H,    data = data))[["r.squared"]],
    H_pass5    = summary(lm(pass_5 ~ H,    data = data))[["r.squared"]],
    H_pass8    = summary(lm(pass_8 ~ H,    data = data))[["r.squared"]],
    conf_pass1 = summary(lm(pass_1 ~ conf, data = data))[["r.squared"]],
    conf_pass5 = summary(lm(pass_5 ~ conf, data = data))[["r.squared"]],
    conf_pass8 = summary(lm(pass_8 ~ conf, data = data))[["r.squared"]]
  )
}

full_r2   <- r2_lm(qa_stats)
cs_r2     <- r2_lm(dplyr::filter(qa_stats, context_sensitive))

# Accuracy-only CS subset: delta_acc_global >= 0.6, no entropy filter
cs_acc_ids <- qa_stats %>%
  dplyr::filter(alpha == 0, delta_acc_global >= 0.6) %>%
  dplyr::pull(id)
cs_acc_r2 <- r2_lm(dplyr::filter(qa_stats, id %in% cs_acc_ids))

n_full   <- dplyr::n_distinct(qa_stats$id)
n_cs     <- dplyr::n_distinct(dplyr::filter(qa_stats, context_sensitive)$id)
n_cs_acc <- length(cs_acc_ids)

cat("Full sample R^2 (n =", n_full, "questions):\n")
print(round(full_r2, 3))

cat("\nCS subset R^2 (delta>=0.6 AND H>=0.5, n =", n_cs, "questions):\n")
print(round(cs_r2, 3))

cat("\nAcc-only subset R^2 (delta>=0.6 only, n =", n_cs_acc, "questions):\n")
print(round(cs_acc_r2, 3))

# Formatted table for the paper
cat("\n\n--- Formatted for paper ---\n")
fmt <- function(r2) sprintf("%.3f", r2)

cat(sprintf("%-10s  %6s  %6s  %6s\n", "", "Full", "Acc-CS", "CS"))
for (row in list(
  list(label="pass@1", h=c(full_r2$H_pass1, cs_acc_r2$H_pass1, cs_r2$H_pass1),
                conf=c(full_r2$conf_pass1, cs_acc_r2$conf_pass1, cs_r2$conf_pass1)),
  list(label="pass@5", h=c(full_r2$H_pass5, cs_acc_r2$H_pass5, cs_r2$H_pass5),
                conf=c(full_r2$conf_pass5, cs_acc_r2$conf_pass5, cs_r2$conf_pass5)),
  list(label="pass@8", h=c(full_r2$H_pass8, cs_acc_r2$H_pass8, cs_r2$H_pass8),
                conf=c(full_r2$conf_pass8, cs_acc_r2$conf_pass8, cs_r2$conf_pass8))
)) {
  cat(sprintf("H     %-6s  %s  %s  %s\n", row$label,
              fmt(row$h[1]), fmt(row$h[2]), fmt(row$h[3])))
  cat(sprintf("conf  %-6s  %s  %s  %s\n", row$label,
              fmt(row$conf[1]), fmt(row$conf[2]), fmt(row$conf[3])))
}