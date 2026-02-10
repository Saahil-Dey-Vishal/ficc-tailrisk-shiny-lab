# FICC Liquidity-Aware Tail-Risk Lab (R + Shiny)

[![R](https://img.shields.io/badge/R-4.3%2B-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-App-0b5d3a)](https://shiny.posit.co/)
[![License](https://img.shields.io/github/license/Saahil-Dey-Vishal/ficc-tailrisk-shiny-lab)](LICENSE)
[![Last Commit](https://img.shields.io/github/last-commit/Saahil-Dey-Vishal/ficc-tailrisk-shiny-lab)](https://github.com/Saahil-Dey-Vishal/ficc-tailrisk-shiny-lab/commits/main)
[![Run Local](https://img.shields.io/badge/Run-Local-0b5d3a)](#quick-start)
[![Run Docker](https://img.shields.io/badge/Run-Docker-0b5d3a)](#docker)

A weekly, Friday-close portfolio research lab for FICC ETF sleeves using Yahoo Finance data. It blends
liquidity estimates, tail-risk structure, discrete trade sizing, and portfolio optimization into a
single Shiny dashboard, then paper-trades **in simulation** (no broker connection).

![App preview](docs/preview.svg)

## Project purpose
This project is designed to help external users, researchers, and portfolio teams evaluate weekly FICC allocation decisions in a structured way.

It answers five practical questions:
1. What should we own this week?
2. How much should we own?
3. What is likely to be expensive to trade?
4. Are we in a risk-on or risk-off market state?
5. Are drawdown controls working as expected?

## Terminology (plain English)
- `FICC`: Fixed Income, Currencies, and Commodities. In this app, FICC exposure is represented with liquid ETFs.
- `Tail risk`: Risk of rare but severe portfolio losses that basic volatility metrics may not capture.
- `Liquidity`: How easy and cheap it is to trade an instrument without moving its price too much.
- `Regime`: A market state (for example, risk-on or risk-off) inferred from rates and credit behavior.
- `Drawdown`: Percentage decline from the portfolio's previous peak.
- `Risk-off switch`: Rule that shifts to safer exposure (`BIL`) when drawdown breaches the configured threshold.

## Why this is unique
Most strategy demos focus only on returns or prediction accuracy. This project integrates:
- Return signals
- Liquidity-aware constraints
- Tail-risk features
- Regime-aware filtering
- Drawdown guardrails

That makes it closer to production portfolio workflow than a single-signal toy model.

## Ultimate purpose
The ultimate objective is to produce a repeatable, explainable, risk-aware weekly allocation process that can be reviewed by humans before deployment.

It is intended to reduce avoidable losses and improve governance quality, not just maximize backtest return.

## Industry-level use cases
- Multi-asset teams testing tactical fixed-income and macro sleeves.
- Risk management teams validating regime and drawdown controls.
- Quant research teams prototyping portfolio construction logic before OMS integration.
- Investment committees reviewing transparent model behavior with auditable outputs.

## What it does
- Weekly rebalancing on Friday close
- Liquidity-aware signal scoring (bid-ask spread estimates)
- Tail-risk features (rolling skew/kurtosis + HOFA factor plot if available)
- Regime filters (rates and credit risk-on/off)
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

## Docker
1. Build the image

```bash
docker build -t ficc-tailrisk-shiny-lab .
```

2. Run the container

```bash
docker run --rm -p 3838:3838 ficc-tailrisk-shiny-lab
```

3. Open

```
http://localhost:3838
```

## Notes
- Data is pulled from Yahoo Finance via `quantmod` and resampled to weekly bars.
- If `bidask`, `hofa`/`HOFA`, `echoice2`, or `portfolio.optimization` are not installed, the system
  falls back to deterministic heuristics so the app still runs.
- Regime signals use weekly momentum on `TLT` (rates) and `HYG - LQD` (credit).
- Regime tickers and thresholds are configurable in the sidebar.
- The drawdown guardrail uses a **next-week risk-off switch** into `BIL` after a breach.

## Structure
- `app.R` Shiny UI
- `R/` data, features, portfolio, and backtest modules
- `config/universe.csv` ETF universe
- `scripts/run_pipeline.R` CLI entrypoint
- `data/` results cache (not tracked)

## Disclaimer
This is a research/education tool. It is not investment advice.

## One-file run (VS Code)
Run everything (install/update + launch app) using one command:

```bash
cd "/Users/saahildey/ficc-tailrisk-shiny-lab"
Rscript run_vscode.R
```

Optional custom port:

```bash
Rscript run_vscode.R 3839
```
