simulate_portfolio <- function(returns_xts, weights_df, drawdown_limit = 0.08, risk_off_ticker = "BIL") {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }
  if (!requireNamespace("xts", quietly = TRUE)) {
    stop("Package 'xts' is required.")
  }

  tickers <- colnames(returns_xts)
  dates <- as.Date(xts::index(returns_xts))

  wmat <- matrix(NA_real_, nrow = length(dates), ncol = length(tickers), dimnames = list(as.character(dates), tickers))

  for (d in unique(weights_df$date)) {
    row <- weights_df[weights_df$date == d, ]
    if (nrow(row) == 0) next
    wvec <- rep(0, length(tickers))
    names(wvec) <- tickers
    wvec[row$ticker] <- row$w
    wmat[as.character(d), ] <- wvec
  }

  for (i in seq_len(nrow(wmat))) {
    if (i == 1) {
      if (any(is.na(wmat[i, ]))) {
        wmat[i, ] <- rep(1 / length(tickers), length(tickers))
      }
    } else {
      na_idx <- is.na(wmat[i, ])
      if (any(na_idx)) wmat[i, na_idx] <- wmat[i - 1, na_idx]
    }

    if (sum(wmat[i, ], na.rm = TRUE) == 0) {
      wmat[i, ] <- rep(1 / length(tickers), length(tickers))
    }

    wmat[i, ] <- pmax(0, wmat[i, ])
    wmat[i, ] <- wmat[i, ] / sum(wmat[i, ])
  }

  n <- length(dates)
  port_val <- numeric(n)
  port_ret <- numeric(n)
  drawdown <- numeric(n)
  risk_off <- logical(n)

  port_val[1] <- 1
  port_ret[1] <- 0
  drawdown[1] <- 0
  peak <- 1

  for (i in 2:n) {
    w_use <- wmat[i - 1, ]

    if (drawdown[i - 1] < -abs(drawdown_limit) && risk_off_ticker %in% tickers) {
      w_use <- rep(0, length(tickers))
      w_use[risk_off_ticker] <- 1
      risk_off[i] <- TRUE
    } else {
      risk_off[i] <- FALSE
    }

    r <- as.numeric(returns_xts[i, , drop = TRUE])
    port_ret[i] <- sum(w_use * r, na.rm = TRUE)
    port_val[i] <- port_val[i - 1] * (1 + port_ret[i])
    peak <- max(peak, port_val[i])
    drawdown[i] <- (port_val[i] / peak) - 1
  }

  data.frame(
    date = dates,
    port_ret = port_ret,
    port_val = port_val,
    drawdown = drawdown,
    risk_off = risk_off
  )
}

compute_drawdown <- function(values) {
  peak <- cummax(values)
  (values / peak) - 1
}
