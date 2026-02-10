library(shiny)
library(bslib)
library(ggplot2)
library(DT)

source("R/utils.R")
source("R/data_fetch.R")
source("R/features.R")
source("R/choice_model.R")
source("R/portfolio_opt.R")
source("R/backtest.R")
source("R/regime.R")
source("R/pipeline.R")

# Seed regime selectors before the first run, so run_pipeline() never receives NULL tickers.
default_universe <- tryCatch(load_universe(), error = function(e) data.frame(ticker = c("TLT", "HYG", "LQD", "BIL")))
ticker_choices <- sort(unique(default_universe$ticker))
if (length(ticker_choices) == 0) ticker_choices <- c("TLT", "HYG", "LQD", "BIL")

default_rates_ticker <- if ("TLT" %in% ticker_choices) "TLT" else ticker_choices[1]
default_credit_ticker <- if ("HYG" %in% ticker_choices) "HYG" else ticker_choices[1]
default_ig_ticker <- if ("LQD" %in% ticker_choices) "LQD" else ticker_choices[1]

app_theme <- bs_theme(
  version = 5,
  bg = "#f5efe6",
  fg = "#161616",
  primary = "#0b5d3a",
  secondary = "#c76b2e",
  base_font = font_google("Space Grotesk"),
  heading_font = font_google("Fraunces"),
  code_font = font_google("Fira Code")
)

ui <- page_sidebar(
  title = "FICC Liquidity-Aware Tail-Risk Lab",
  theme = app_theme,
  sidebar = sidebar(
    width = 340,
    dateRangeInput("dates", "History window", start = Sys.Date() - 365 * 5, end = Sys.Date()),
    sliderInput("max_weight", "Max single-ETF weight", min = 0.05, max = 0.4, value = 0.2, step = 0.01),
    sliderInput("drawdown", "Max drawdown limit", min = 0.02, max = 0.2, value = 0.08, step = 0.01),
    numericInput("tail_window", "Tail-risk window (weeks)", value = 52, min = 20, max = 156, step = 1),
    numericInput("mom_window", "Momentum window (weeks)", value = 13, min = 4, max = 52, step = 1),
    numericInput("n_factors", "HOFA factors", value = 3, min = 1, max = 6, step = 1),
    checkboxInput("use_echoice2", "Use echoice2 choice model", value = TRUE),
    checkboxInput("use_opt", "Use portfolio.optimization", value = TRUE),
    checkboxInput("use_regimes", "Use regime filters", value = TRUE),
    selectInput("rates_ticker", "Rates regime ticker", choices = ticker_choices, selected = default_rates_ticker),
    selectInput("credit_ticker", "Credit regime ticker", choices = ticker_choices, selected = default_credit_ticker),
    selectInput("ig_ticker", "IG comparator ticker", choices = ticker_choices, selected = default_ig_ticker),
    numericInput("rate_threshold", "Rates regime threshold", value = 0, step = 0.0025),
    numericInput("credit_threshold", "Credit regime threshold", value = 0, step = 0.0025),
    actionButton("run", "Run Pipeline", class = "btn-primary")
  ),
  navset_card_tab(
    nav_panel(
      "Universe",
      fluidRow(
        column(7, DTOutput("universe_tbl")),
        column(5, plotOutput("spread_plot", height = 320))
      )
    ),
    nav_panel(
      "Signals",
      fluidRow(
        column(4, selectInput("signal_ticker", "Ticker", choices = NULL)),
        column(8, plotOutput("signal_plot", height = 320))
      ),
      fluidRow(
        column(12, plotOutput("hofa_plot", height = 240))
      ),
      fluidRow(
        column(12, DTOutput("signals_tbl"))
      )
    ),
    nav_panel(
      "Portfolio",
      fluidRow(
        column(4, selectInput("weight_ticker", "Ticker", choices = NULL)),
        column(8, plotOutput("weights_plot", height = 320))
      ),
      fluidRow(
        column(12, DTOutput("weights_tbl"))
      )
    ),
    nav_panel(
      "Regimes",
      fluidRow(
        column(4, uiOutput("regime_summary")),
        column(8, plotOutput("regime_plot", height = 280))
      ),
      fluidRow(
        column(12, DTOutput("regime_tbl"))
      )
    ),
    nav_panel(
      "Backtest",
      fluidRow(
        column(12, plotOutput("backtest_plot", height = 320))
      ),
      fluidRow(
        column(12, plotOutput("drawdown_plot", height = 240))
      )
    )
  ),
  tags$style(HTML(".bslib-sidebar-layout { background: linear-gradient(135deg, #f5efe6 0%, #f2e4d4 45%, #f8f2eb 100%); }
                  .card { box-shadow: 0 8px 20px rgba(0,0,0,0.06); border: 1px solid rgba(0,0,0,0.06);} 
                  .sidebar { background: rgba(255,255,255,0.9);} 
                  h2, h3 { letter-spacing: 0.5px; }")
  )
)

server <- function(input, output, session) {
  first_scalar <- function(value) {
    if (is.null(value) || length(value) == 0) return(NA_character_)
    as.character(value[[1]])
  }

  pick_or_default <- function(value, fallback) {
    x <- first_scalar(value)
    if (is.na(x) || !nzchar(x)) return(fallback)
    x
  }

  result <- eventReactive(input$run, {
    tryCatch(
      run_pipeline(
        start = input$dates[1],
        end = input$dates[2],
        max_weight = input$max_weight,
        drawdown_limit = input$drawdown,
        tail_window = input$tail_window,
        momentum_window = input$mom_window,
        n_factors = input$n_factors,
        use_echoice2 = input$use_echoice2,
        use_opt = input$use_opt,
        use_regimes = input$use_regimes,
        regime_rates_ticker = pick_or_default(input$rates_ticker, default_rates_ticker),
        regime_credit_ticker = pick_or_default(input$credit_ticker, default_credit_ticker),
        regime_ig_ticker = pick_or_default(input$ig_ticker, default_ig_ticker),
        regime_rate_threshold = input$rate_threshold,
        regime_credit_threshold = input$credit_threshold,
        risk_off_ticker = "BIL"
      ),
      error = function(e) {
        showNotification(paste("Pipeline failed:", e$message), type = "error", duration = NULL)
        NULL
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(result(), {
    if (is.null(result())) return()
    ticks <- sort(unique(result()$signals$ticker))
    if (length(ticks) == 0) return()

    updateSelectInput(session, "signal_ticker", choices = ticks, selected = ticks[1])
    updateSelectInput(session, "weight_ticker", choices = ticks, selected = ticks[1])

    selected_rates <- pick_or_default(input$rates_ticker, if ("TLT" %in% ticks) "TLT" else ticks[1])
    selected_credit <- pick_or_default(input$credit_ticker, if ("HYG" %in% ticks) "HYG" else ticks[1])
    selected_ig <- pick_or_default(input$ig_ticker, if ("LQD" %in% ticks) "LQD" else ticks[1])

    updateSelectInput(session, "rates_ticker", choices = ticks, selected = selected_rates)
    updateSelectInput(session, "credit_ticker", choices = ticks, selected = selected_credit)
    updateSelectInput(session, "ig_ticker", choices = ticks, selected = selected_ig)
  })

  output$universe_tbl <- renderDT({
    req(result())
    datatable(result()$universe, options = list(pageLength = 15), rownames = FALSE)
  })

  output$spread_plot <- renderPlot({
    req(result())
    latest <- result()$spreads
    if (nrow(latest) == 0) return(NULL)
    latest <- latest[latest$date == max(latest$date), ]
    ggplot(latest, aes(x = reorder(ticker, spread_est), y = spread_est, fill = spread_est)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
      labs(title = "Estimated Weekly Spread", x = NULL, y = "Spread") +
      theme_minimal(base_size = 12)
  })

  output$signal_plot <- renderPlot({
    req(result(), input$signal_ticker)
    df <- result()$signals
    df <- df[df$ticker == input$signal_ticker, ]
    if (nrow(df) == 0) return(NULL)

    df_long <- tidyr::pivot_longer(df, cols = c(momentum, tail_score), names_to = "metric", values_to = "value")
    ggplot(df_long, aes(x = date, y = value, color = metric)) +
      geom_line(linewidth = 1) +
      labs(title = paste("Signals:", input$signal_ticker), x = NULL, y = "Score") +
      theme_minimal(base_size = 12)
  })

  output$hofa_plot <- renderPlot({
    req(result())
    fx <- result()$hofa$factors
    if (is.null(fx) || nrow(fx) == 0) return(NULL)

    df <- data.frame(date = as.Date(xts::index(fx)), xts::coredata(fx))
    df_long <- tidyr::pivot_longer(df, -date, names_to = "factor", values_to = "value")

    ggplot(df_long, aes(x = date, y = value, color = factor)) +
      geom_line(linewidth = 1) +
      labs(title = "HOFA Factor Signals", x = NULL, y = "Score") +
      theme_minimal(base_size = 12)
  })

  output$signals_tbl <- renderDT({
    req(result())
    df <- result()$signals
    df <- df[df$date == max(df$date), ]
    datatable(df, options = list(pageLength = 15), rownames = FALSE)
  })

  output$weights_plot <- renderPlot({
    req(result(), input$weight_ticker)
    df <- result()$weights
    df <- df[df$ticker == input$weight_ticker, ]
    if (nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = date, y = w)) +
      geom_line(color = "#0b5d3a", linewidth = 1) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      labs(title = paste("Weights:", input$weight_ticker), x = NULL, y = "Weight") +
      theme_minimal(base_size = 12)
  })

  output$weights_tbl <- renderDT({
    req(result())
    df <- result()$weights
    df <- df[df$date == max(df$date), ]
    datatable(df, options = list(pageLength = 15), rownames = FALSE)
  })

  output$regime_plot <- renderPlot({
    req(result())
    df <- result()$regimes
    if (is.null(df) || nrow(df) == 0) return(NULL)

    df_long <- tidyr::pivot_longer(df, cols = c(rate_signal, credit_signal), names_to = "signal", values_to = "value")

    ggplot(df_long, aes(x = date, y = value, color = signal)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#1f1f1f") +
      labs(title = "Regime Signals", x = NULL, y = "Signal") +
      theme_minimal(base_size = 12)
  })

  output$regime_summary <- renderUI({
    req(result())
    df <- result()$regimes
    if (is.null(df) || nrow(df) == 0) return(NULL)

    latest <- df[which.max(df$date), ]
    rate_counts <- table(df$rate_state)
    credit_counts <- table(df$credit_state)

    bslib::card(
      bslib::card_header("Regime Summary"),
      bslib::card_body(
        tags$p(tags$strong("Latest rates:"), paste(latest$rate_state)),
        tags$p(tags$strong("Latest credit:"), paste(latest$credit_state)),
        tags$hr(),
        tags$p(tags$strong("Rates counts:"), paste(names(rate_counts), rate_counts, collapse = " | ")),
        tags$p(tags$strong("Credit counts:"), paste(names(credit_counts), credit_counts, collapse = " | "))
      )
    )
  })

  output$regime_tbl <- renderDT({
    req(result())
    df <- result()$regimes
    df <- df[order(df$date, decreasing = TRUE), ]
    df <- head(df, 20)
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })

  output$backtest_plot <- renderPlot({
    req(result())
    df <- result()$backtest
    ggplot(df, aes(x = date, y = port_val)) +
      geom_line(color = "#0b5d3a", linewidth = 1) +
      labs(title = "Portfolio Value", x = NULL, y = "Index") +
      theme_minimal(base_size = 12)
  })

  output$drawdown_plot <- renderPlot({
    req(result())
    df <- result()$backtest
    ggplot(df, aes(x = date, y = drawdown)) +
      geom_line(color = "#b54b2f", linewidth = 1) +
      geom_hline(yintercept = -input$drawdown, linetype = "dashed", color = "#1f1f1f") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      labs(title = "Drawdown", x = NULL, y = "Drawdown") +
      theme_minimal(base_size = 12)
  })
}

shinyApp(ui, server)
