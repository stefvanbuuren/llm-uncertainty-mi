# Sample LLM responses for each question across 5 evidence levels.
# Requires: Python with the `datasets` package (via reticulate), OPENAI_API_KEY env var.
# Loads the SQuAD validation split via the HuggingFace datasets library.
# Output: qa_results_10_400_5.rds (pre-computed version included in repo)

library(reticulate)
library(httr)
library(jsonlite)
library(dplyr)

source("helpers.R")

set.seed(1L)

py_require("datasets")
datasets <- import("datasets")
ds <- datasets$load_dataset("squad", split = "validation")

builtins <- import_builtins()
question <- py_to_r(builtins$list(ds[["question"]]))
context  <- py_to_r(builtins$list(ds[["context"]]))
truth    <- py_to_r(builtins$list(ds[["answers"]]))

qa <- data.frame(
  question = as.character(question),
  context  = as.character(context),
  truth    = vapply(truth, function(x) x$text[[1]], character(1)),
  stringsAsFactors = FALSE
)

# Context-level deduplication: sample one question per unique context
context_id    <- match(qa$context, unique(qa$context))
qa$context_id <- context_id

one_per_context <- do.call(
  rbind,
  lapply(split(seq_len(nrow(qa)), qa$context_id), function(idx) {
    qa[sample(idx, 1), , drop = FALSE]
  })
)
one_per_context <- one_per_context[sample(nrow(one_per_context)), ]

n_to      <- min(400, nrow(one_per_context))
qa_sample <- one_per_context[seq_len(n_to), ]

m         <- 10
cache_dir <- "cache_draws"
dir.create(cache_dir, showWarnings = FALSE)

qa_draws <- do.call(
  rbind,
  lapply(seq_len(n_to), function(i) {
    cache_file <- file.path(cache_dir, paste0("draw_", i, ".rds"))

    if (file.exists(cache_file)) {
      cat("Skipping", i, "(cached)\n")
      return(readRDS(cache_file))
    }

    cat("Processing", i, "\n")

    draws <- evaluate_question_draws(
      qa_sample$question[i],
      qa_sample$context[i],
      qa_sample$truth[i],
      m = m
    )

    draws$id         <- i
    draws$context_id <- qa_sample$context_id[i]
    draws$question   <- qa_sample$question[i]
    draws$truth      <- qa_sample$truth[i]

    saveRDS(draws, cache_file)
    Sys.sleep(10)
    draws
  })
)

all_files <- list.files(cache_dir, pattern = "^draw_[0-9]+[.]rds$", full.names = TRUE)
qa_draws  <- do.call(rbind, lapply(all_files, readRDS))
qa_draws  <- qa_draws[order(as.numeric(qa_draws$id)), ]

qa_results <- qa_draws %>%
  group_by(id) %>%
  group_modify(~ evaluate_question(compute_stats_from_draws(.x))) %>%
  ungroup()

saveRDS(qa_results, "qa_results_10_400_5.rds")
