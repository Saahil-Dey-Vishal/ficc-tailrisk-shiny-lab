compute_regimes <- function(momentum_xts,
                            rates_ticker = "TLT",
                            credit_ticker = "HYG",
                            ig_ticker = "LQD",
                            rate_threshold = 0,
                            credit_threshold = 0) {
  if (!requireNamespace("xts", quietly = TRUE)) {
    stop("Package 'xts' is required for regime computation.")
  }

  to_scalar_ticker <- function(x, fallback) {
    if (is.null(x) || length(x) == 0) return(fallback)
    x <- as.character(x[[1]])
    if (is.na(x) || !nzchar(x)) return(fallback)
    x
  }

  rates_ticker <- to_scalar_ticker(rates_ticker, "TLT")
  credit_ticker <- to_scalar_ticker(credit_ticker, "HYG")
  ig_ticker <- to_scalar_ticker(ig_ticker, "LQD")

  get_col <- function(x, tk) {
    if (!is.null(x) && tk %in% colnames(x)) {
      return(x[, tk])
    }
    xts::xts(rep(NA_real_, nrow(x)), order.by = zoo::index(x))
  }

  rate_sig <- get_col(momentum_xts, rates_ticker)
  credit_sig <- get_col(momentum_xts, credit_ticker) - get_col(momentum_xts, ig_ticker)

  rate_sig <- as.numeric(rate_sig)
  credit_sig <- as.numeric(credit_sig)

  rate_sig[is.na(rate_sig)] <- 0
  credit_sig[is.na(credit_sig)] <- 0

  data.frame(
    date = as.Date(zoo::index(momentum_xts)),
    rate_signal = rate_sig,
    credit_signal = credit_sig,
    rate_state = ifelse(rate_sig >= rate_threshold, "risk_on", "risk_off"),
    credit_state = ifelse(credit_sig >= credit_threshold, "risk_on", "risk_off")
  )
}

apply_regime_filters <- function(weights_df,
                                 regimes_df,
                                 risk_off_ticker = "BIL",
                                 rate_off_scale = 0.3,
                                 credit_off_scale = 0.3) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  duration <- c("TLT", "IEF")
  credit <- c("HYG", "LQD", "EMB")

  dates <- sort(unique(weights_df$date))
  out <- vector("list", length(dates))

  for (i in seq_along(dates)) {
    d <- dates[i]
    wdf <- weights_df[weights_df$date == d, ]
    reg <- regimes_df[regimes_df$date == d, ]

    rate_state <- if (nrow(reg) > 0) reg$rate_state[1] else "risk_on"
    credit_state <- if (nrow(reg) > 0) reg$credit_state[1] else "risk_on"

    w <- wdf$w
    names(w) <- wdf$ticker

    if (rate_state == "risk_off") {
      idx <- names(w) %in% duration
      w[idx] <- w[idx] * rate_off_scale
    }

    if (credit_state == "risk_off") {
      idx <- names(w) %in% credit
      w[idx] <- w[idx] * credit_off_scale
    }

    w[is.na(w)] <- 0
    w[w < 0] <- 0

    leftover <- 1 - sum(w)

    if (leftover > 0 && risk_off_ticker %in% names(w)) {
      w[risk_off_ticker] <- w[risk_off_ticker] + leftover
    } else if (leftover > 0) {
      w <- w + leftover / length(w)
    }

    if (sum(w) > 0) w <- w / sum(w)

    out[[i]] <- data.frame(date = d, ticker = names(w), w = as.numeric(w))
  }

  dplyr::bind_rows(out)
}
