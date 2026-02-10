source("R/utils.R")
source("R/data_fetch.R")
source("R/features.R")
source("R/choice_model.R")
source("R/portfolio_opt.R")
source("R/backtest.R")
source("R/regime.R")
source("R/pipeline.R")

args <- commandArgs(trailingOnly = TRUE)
start <- if (length(args) >= 1) as.Date(args[1]) else Sys.Date() - 365 * 5
end <- if (length(args) >= 2) as.Date(args[2]) else Sys.Date()

res <- run_pipeline(
  start = start,
  end = end,
  max_weight = 0.2,
  drawdown_limit = 0.08,
  tail_window = 52,
  momentum_window = 13,
  n_factors = 3,
  use_echoice2 = TRUE,
  use_opt = TRUE,
  use_regimes = TRUE,
  regime_rates_ticker = "TLT",
  regime_credit_ticker = "HYG",
  regime_ig_ticker = "LQD",
  regime_rate_threshold = 0,
  regime_credit_threshold = 0,
  risk_off_ticker = "BIL"
)

dir.create("data", showWarnings = FALSE)
saveRDS(res, file = "data/results.rds")

cat("Saved results to data/results.rds\n")
