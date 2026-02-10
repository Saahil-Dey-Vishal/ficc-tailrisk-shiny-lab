# FICC Liquidity-Aware Tail-Risk Lab (R + Shiny)

A weekly, Friday-close portfolio research lab for FICC ETF sleeves using Yahoo Finance data. It blends
liquidity estimates, tail-risk structure, discrete trade sizing, and portfolio optimization into a
single Shiny dashboard, then paper-trades **in simulation** (no broker connection).

## What it does
- Weekly rebalancing on Friday close
- Liquidity-aware signal scoring (bid-ask spread estimates)
- Tail-risk features (rolling skew/kurtosis + HOFA factor plot if available)
- Discrete trade/size modeling (echoice2 if available)
- Portfolio optimization (portfolio.optimization if available)
- Max drawdown guardrail (8% default) that shifts to BIL when breached

## ETF universe
The default universe targets liquid FICC sleeves plus FX and commodities proxies:
`AGG, BIL, SHY, IEF, TLT, TIP, MBB, LQD, HYG, EMB, MUB, FLOT, USDU, GSG`

Edit `config/universe.csv` to customize.

## Quick start
1. Install packages

```r
install.packages(c(
  "shiny", "bslib", "DT", "ggplot2", "scales",
  "quantmod", "xts", "zoo", "moments",
  "dplyr", "tidyr"
))

# Optional (used when available)
install.packages(c(
  "bidask", "hofa", "echoice2", "portfolio.optimization", "rsims"
))
```

2. Run the app

```r
shiny::runApp(".")
```

3. Run the pipeline headless (optional)

```r
Rscript scripts/run_pipeline.R 2019-01-01 2025-12-31
```

## Notes
- Data is pulled from Yahoo Finance via `quantmod` and resampled to weekly bars.
- If `bidask`, `hofa`/`HOFA`, `echoice2`, or `portfolio.optimization` are not installed, the system
  falls back to deterministic heuristics so the app still runs.
- The drawdown guardrail uses a **next-week risk-off switch** into `BIL` after a breach.

## Structure
- `app.R` Shiny UI
- `R/` data, features, portfolio, and backtest modules
- `config/universe.csv` ETF universe
- `scripts/run_pipeline.R` CLI entrypoint
- `data/` results cache (not tracked)

## Disclaimer
This is a research/education tool. It is not investment advice.
