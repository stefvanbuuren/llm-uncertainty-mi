library(dplyr)
library(httr)
library(jsonlite)

clean_answer <- function(x) {
  x <- tolower(x)
  x <- gsub("\\b(the|a|an)\\b", "", x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x
}

is_correct <- function(pred, truth) {
  mapply(
    function(p, t) {
      p <- clean_answer(p)
      t <- clean_answer(t)
      p == t ||
        grepl(p, t, fixed = TRUE) ||
        grepl(t, p, fixed = TRUE)
    },
    pred,
    truth
  )
}

truncate_context <- function(context, alpha) {
  if (alpha <= 0) return("")
  if (alpha >= 1) return(context)
  n <- nchar(context)
  k <- max(1, floor(alpha * n))
  substr(context, 1, k)
}

make_prompt <- function(question, context) {
  paste0(
    "Give only the final answer as a short phrase (1-3 words).\n",
    "Do not write a sentence.\n",
    "Do not add explanation.\n\n",
    "Context: ",
    context,
    "\n\nQuestion: ",
    question,
    "\nAnswer:"
  )
}

sample_llm <- function(q, context, m = 10) {
  prompt <- make_prompt(q, context)

  responses <- vapply(
    seq_len(m),
    function(j) {
      out <- tryCatch(
        {
          res <- ask_llm(
            prompt,
            temperature = 0.7,
            top_p = 0.9,
            seed = sample.int(1e9, 1)
          )
          if (is.null(res) || length(res) == 0 || is.na(res)) NA_character_
          else as.character(res)
        },
        error = function(e) NA_character_
      )
      if (is.na(out)) cat("WARNING: empty LLM response at draw", j, "\n")
      Sys.sleep(2)
      out
    },
    character(1)
  )

  responses
}

evaluate_question_draws <- function(q, context, truth, m = 10) {
  alpha_grid <- c(1, 0.5, 0.3, 0.1, 0)

  all_draws <- do.call(
    rbind,
    lapply(alpha_grid, function(alpha) {
      context_mod <- truncate_context(context, alpha)
      responses <- sample_llm(q, context_mod, m = m)
      clean_response <- clean_answer(responses)
      clean_truth <- clean_answer(truth)

      data.frame(
        alpha = alpha,
        draw = seq_len(m),
        response = responses,
        clean_response = clean_response,
        clean_truth = clean_truth,
        correct = as.integer(is_correct(clean_response, clean_truth)),
        stringsAsFactors = FALSE
      )
    })
  )

  all_draws
}

classify_regime <- function(
  acc_full,
  acc_none,
  H_full,
  H_none,
  delta,
  tau_delta = 0.6,
  tau_H = 0.5,
  tau_small = 0.2
) {
  if (is.na(acc_full) || is.na(acc_none)) {
    return(data.frame(
      regime = "unknown",
      regime_refined = "unknown",
      stringsAsFactors = FALSE
    ))
  }

  regime <- if (acc_full == 1 && acc_none == 1) {
    "memorized"
  } else if (acc_full == 1 && acc_none == 0 && H_none < 0.1) {
    "biased"
  } else if (acc_full == 1 && H_none > tau_H) {
    "uncertain"
  } else {
    "other"
  }

  regime_refined <- if (delta < -tau_small) {
    "degradation"
  } else if (acc_full < 1) {
    "extraction_failure"
  } else if (abs(delta) < tau_small) {
    "weak_dependence"
  } else if (acc_none == 0 && H_none < 0.1) {
    "confident_wrong"
  } else if (delta > tau_small && delta < 1) {
    "partial_dependence"
  } else if (acc_full == 1 && acc_none == 1) {
    "memorized"
  } else if (acc_full == 1 && acc_none == 0 && H_none < 0.1) {
    "biased"
  } else if (acc_full == 1 && H_none > tau_H) {
    "uncertain"
  } else {
    "other_misc"
  }

  context_sensitive <- (delta >= tau_delta) && (H_none >= tau_H) && (H_full <= 0.05)

  data.frame(
    regime = regime,
    regime_refined = regime_refined,
    context_sensitive = context_sensitive,
    stringsAsFactors = FALSE
  )
}

evaluate_question <- function(stats, tau_delta = 0.6, tau_H = 0.5, tau_small = 0.2) {
  acc_full <- stats$acc[stats$alpha == max(stats$alpha)]
  acc_none <- stats$acc[stats$alpha == min(stats$alpha)]
  H_full   <- stats$H[stats$alpha == max(stats$alpha)]
  H_none   <- stats$H[stats$alpha == min(stats$alpha)]
  delta    <- stats$delta_acc_global[1]

  class <- classify_regime(
    acc_full = acc_full, acc_none = acc_none,
    H_full = H_full, H_none = H_none, delta = delta,
    tau_delta = tau_delta, tau_H = tau_H, tau_small = tau_small
  )

  cbind(stats, class)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

ask_llm <- function(
  prompt,
  model = "gpt-4o-mini",
  temperature = 0.7,
  top_p = 0.9,
  max_tokens = 10,
  seed = NULL,
  max_retries = 3
) {
  body <- list(
    model = model,
    messages = list(list(role = "user", content = prompt)),
    temperature = temperature,
    top_p = top_p,
    max_tokens = max_tokens
  )
  if (!is.null(seed)) body$seed <- seed

  for (attempt in seq_len(max_retries)) {
    res <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        "Content-Type" = "application/json"
      ),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "raw"
    )

    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    obj <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

    if (!is.null(obj$error)) {
      code <- obj$error$code %||% ""
      if (grepl("rate_limit", code) || httr::status_code(res) == 429L) {
        wait <- 10 * 2^(attempt - 1)
        cat("Rate limited, retrying in", wait, "s (attempt", attempt, "of", max_retries, ")\n")
        Sys.sleep(wait)
        next
      }
      cat("API error:", obj$error$message, "\n")
      return(NA_character_)
    }

    answer <- tryCatch(
      obj$choices[[1]]$message$content,
      error = function(e) NA_character_
    )

    if (is.null(answer) || length(answer) == 0 || !nzchar(answer, keepNA = TRUE)) {
      return(NA_character_)
    }

    ans <- trimws(answer)
    return(if (length(ans) == 0) NA_character_ else ans)
  }

  cat("Max retries exceeded\n")
  NA_character_
}

# Unbiased pass@k estimator (Chen et al. 2021, eq. 1).
pass_at_k <- function(n, c, k) {
  if (n < k) return(NA_real_)
  1 - prod((n - c - seq_len(k) + 1) / (n - seq_len(k) + 1))
}

compute_stats_from_draws <- function(draws) {
  stats <- draws %>%
    group_by(alpha) %>%
    summarise(
      m = n(),
      acc = mean(correct, na.rm = TRUE),
      pass_1 = pass_at_k(sum(!is.na(clean_response)), sum(correct == 1, na.rm = TRUE), 1),
      pass_5 = pass_at_k(sum(!is.na(clean_response)), sum(correct == 1, na.rm = TRUE), 5),
      pass_8 = pass_at_k(sum(!is.na(clean_response)), sum(correct == 1, na.rm = TRUE), 8),
      H = {
        p <- table(clean_response[!is.na(clean_response)]) / n()
        if (length(p) == 0) NA_real_ else -sum(p * log(p))
      },
      conf = {
        p <- table(clean_response[!is.na(clean_response)]) / n()
        if (length(p) == 0) NA_real_ else max(p)
      },
      .groups = "drop"
    ) %>%
    arrange(desc(alpha)) %>%
    mutate(
      delta_acc_global = max(acc) - min(acc),
      delta_H_global   = max(H)   - min(H)
    )

  stats
}
