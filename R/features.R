compute_weekly_returns <- function(prices_xts) {
  if (requireNamespace("PerformanceAnalytics", quietly = TRUE)) {
    ret <- PerformanceAnalytics::Return.calculate(prices_xts, method = "discrete")
  } else {
    ret <- diff(log(prices_xts))
  }
  ret[is.na(ret)] <- 0
  ret
}

compute_liquidity_spread <- function(ohlc_list) {
  out <- list()
  for (tk in names(ohlc_list)) {
    xt <- ohlc_list[[tk]]
    if (is.null(xt)) next

    sp <- NULL
    if (requireNamespace("bidask", quietly = TRUE)) {
      sp <- tryCatch(bidask::spread(xt), error = function(e) NULL)
    }

    if (is.null(sp)) {
      hi <- quantmod::Hi(xt)
      lo <- quantmod::Lo(xt)
      cl <- quantmod::Cl(xt)
      sp <- (hi - lo) / cl
    }

    df <- data.frame(
      date = as.Date(zoo::index(sp)),
      ticker = tk,
      spread_est = as.numeric(sp)
    )
    out[[tk]] <- df
  }

  do.call(rbind, out)
}

compute_tail_risk_scores <- function(returns_xts, window = 52) {
  if (!requireNamespace("zoo", quietly = TRUE) || !requireNamespace("moments", quietly = TRUE)) {
    stop("Packages 'zoo' and 'moments' are required for tail risk scores.")
  }

  res <- list()
  for (tk in colnames(returns_xts)) {
    r <- returns_xts[, tk]

    skew <- zoo::rollapply(r, width = window, FUN = function(x) moments::skewness(x, na.rm = TRUE), fill = NA, align = "right")
    kurt <- zoo::rollapply(r, width = window, FUN = function(x) moments::kurtosis(x, na.rm = TRUE), fill = NA, align = "right")

    tail <- (-skew) + kurt

    res[[tk]] <- data.frame(
      date = as.Date(zoo::index(r)),
      ticker = tk,
      tail_score = as.numeric(tail),
      skew = as.numeric(skew),
      kurt = as.numeric(kurt)
    )
  }

  do.call(rbind, res)
}

compute_hofa_factors <- function(returns_xts, n_factors = 3) {
  X <- as.matrix(returns_xts)
  X <- X[stats::complete.cases(X), , drop = FALSE]
  dates <- as.Date(rownames(X))
  n_factors <- min(n_factors, ncol(X))

  pkg <- NULL
  if (requireNamespace("HOFA", quietly = TRUE)) pkg <- "HOFA"
  if (is.null(pkg) && requireNamespace("hofa", quietly = TRUE)) pkg <- "hofa"

  loadings <- NULL
  factors <- NULL

  if (!is.null(pkg)) {
    m2pca <- getExportedValue(pkg, "M2.pca")
    res <- tryCatch(m2pca(X, r = n_factors, method = "PCA", center = TRUE, scale = TRUE), error = function(e) NULL)

    if (!is.null(res) && is.list(res)) {
      loadings <- res$u %||% res$U %||% res$loadings %||% res$A
      factors <- res$f %||% res$F %||% res$scores
    }
  }

  if (is.null(loadings)) {
    pca <- stats::prcomp(X, center = TRUE, scale. = TRUE)
    loadings <- pca$rotation[, 1:n_factors, drop = FALSE]
    factors <- pca$x[, 1:n_factors, drop = FALSE]
  }

  if (is.null(factors)) {
    factors <- X %*% loadings
  }

  colnames(factors) <- paste0("F", seq_len(n_factors))
  colnames(loadings) <- paste0("F", seq_len(n_factors))

  list(
    loadings = data.frame(ticker = colnames(returns_xts), loadings, row.names = NULL),
    factors = xts::xts(factors, order.by = dates)
  )
}
