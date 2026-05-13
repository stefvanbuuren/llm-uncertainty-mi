# llm-uncertainty-mi

Code and data for the paper **LLMs as Implicit Imputers: Uncertainty Should Scale with Missing Information** (NeurIPS 2026).

We argue that an LLM generating answers under incomplete context acts as an implicit imputer, and show empirically that response entropy, unlike sampling-based confidence, increases with context removal in a manner consistent with the multiple imputation criterion that uncertainty should scale with missing information.

## Contents

| File | Description |
|---|---|
| `main.R` | Samples LLM responses across 5 evidence levels for 400 questions |
| `helpers.R` | Core functions: answer cleaning, sampling, regime classification |
| `figure_1.R` | Accuracy, entropy, and confidence as a function of context completeness |
| `figure_2.R` | Resolution ratio, calibration plot, and overconfidence |
| `figure_3.R` | Accuracy vs. confidence and resolution ratio (loess curves) |
| `r2_table.R` | R² table comparing entropy and confidence as predictors of accuracy |
| `qa_results_10_400_5.rds` | Pre-computed results (400 questions, m=10 draws, 5 alpha levels) |

## Reproducing the figures

Pre-computed results are included, so figures can be reproduced without re-running the API:

```r
Rscript figure_1.R
Rscript figure_2.R
Rscript figure_3.R
Rscript r2_table.R
```

## Re-running the sampling pipeline

Requires R packages `reticulate`, `httr`, `jsonlite`, `dplyr`, and a Python environment with the `datasets` package. Set your OpenAI API key:

```bash
export OPENAI_API_KEY=your_key_here
Rscript main.R
```

### Rate-limit settings

Two parameters control API request pacing. Adjust them to match your tier:

**Between-question delay** (`main.R`, line 75):
```r
Sys.sleep(10)  # seconds between questions; reduce for higher-tier keys
```

**Per-draw delay** (`helpers.R`, line 69):
```r
Sys.sleep(2)   # seconds between individual draws within a question
```

**Automatic retry on 429 errors** (`helpers.R`, lines 187, 215):
```r
max_retries = 3          # number of retry attempts
wait <- 10 * 2^(attempt - 1)  # exponential backoff: 10s, 20s, 40s
```

For Tier 1 keys the defaults work reliably. For higher tiers, reducing `Sys.sleep(10)` to 2–3 seconds will substantially cut runtime (400 questions × 5 levels × 10 draws ≈ 20,000 API calls).

## Note on dataset

The paper refers to the Natural Questions (NQ) dataset. The experiments were in fact run on the **SQuAD** validation split (`squad`, HuggingFace `datasets`), which has the same `(question, context, answer)` structure. This discrepancy will be corrected in the next revision.
