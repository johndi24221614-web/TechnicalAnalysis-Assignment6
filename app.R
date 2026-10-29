# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Student: John Diaz
#
# Portfolio visualization dashboard built with R Shiny.
#   - Fetches stock data from Yahoo Finance (with a Stooq fallback source)
#   - Visualizes it as a line, candlestick or area chart
#   - Overlays technical indicators (Moving Averages, RSI, MACD) that can be
#     switched on and off by the user
#   - Applies a moving-average crossover trading rule (with optional RSI filter)
#     and annotates the chart with Buy / Sell / Hold signals
#
# How to run:  open this file in RStudio and click "Run App"
#              (or run  shiny::runApp("path/to/this/folder")  in the console)
# ==============================================================================


# ==============================================================================
# SECTION 1: DATA COLLECTION AND SETUP
# ==============================================================================

# --- 1.1 Install (only if missing) and load the necessary packages ------------
# shiny    -> interactive web application framework
# ggplot2  -> grammar-of-graphics plotting library
# quantmod -> downloads historical stock data from Yahoo Finance
# TTR      -> technical indicators (SMA, RSI, MACD)
required_pkgs <- c("shiny", "ggplot2", "quantmod", "TTR")
missing_pkgs  <- required_pkgs[!(required_pkgs %in% rownames(installed.packages()))]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

# --- 1.2 Global settings --------------------------------------------------------
# Stocks from the portfolio used in the previous assignments. The symbol box in
# the app also accepts any other Yahoo Finance ticker typed by the user.
portfolio_symbols <- c("AAPL", "MSFT", "TSLA", "GM", "F", "TM")

# Extra history downloaded BEFORE the selected start date so that long
# indicators (e.g. 50-period moving average) already have values on day one.
history_buffer_days <- 365 * 6

# --- 1.3 Fetch historical stock data --------------------------------------------
# Standardise column names: Yahoo returns "AAPL.Open", "AAPL.Close", ...
# and we want plain "Open", "High", "Low", "Close", "Volume".
standardize_ohlc <- function(x) {
  colnames(x) <- sub(".*\\.", "", colnames(x))
  x[, c("Open", "High", "Low", "Close", "Volume")]
}

# Source 1: Yahoo Finance through quantmod::getSymbols (as in the assignment)
fetch_yahoo <- function(symbol, from, to) {
  x <- getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  standardize_ohlc(x)
}

# Source 2 (fallback): Stooq CSV download. It is only used when Yahoo fails.
fetch_stooq <- function(symbol, from, to) {
  url <- sprintf("https://stooq.com/q/d/l/?s=%s.us&i=d&d1=%s&d2=%s",
                 tolower(symbol), format(from, "%Y%m%d"), format(to, "%Y%m%d"))
  df <- read.csv(url, stringsAsFactors = FALSE)
  if (!all(c("Date", "Open", "High", "Low", "Close") %in% names(df))) {
    stop("unexpected response from Stooq")
  }
  if (!"Volume" %in% names(df)) df$Volume <- NA_real_
  xts(df[, c("Open", "High", "Low", "Close", "Volume")], order.by = as.Date(df$Date))
}

# Try each data source in order, handling errors and missing data.
# Returns a list with: data (xts or NULL), source, symbol, error.
get_stock_data <- function(symbol, from, to) {
  sources <- list(Yahoo = fetch_yahoo, Stooq = fetch_stooq)
  errors  <- character(0)

  for (source_name in names(sources)) {
    result <- tryCatch({
      x <- sources[[source_name]](symbol, from, to)
      # Missing-data handling: drop rows without a complete OHLC record
      x <- x[complete.cases(x[, c("Open", "High", "Low", "Close")]), ]
      if (nrow(x) < 2) stop("no usable rows returned")
      x
    }, error = function(e) {
      errors <<- c(errors, paste0(source_name, ": ", conditionMessage(e)))
      NULL
    })

    if (!is.null(result)) {
      return(list(data = result, source = source_name, symbol = symbol, error = NULL))
    }
  }

  list(data = NULL, source = NA, symbol = symbol,
       error = paste0("Could not download data for '", symbol, "'. Check the ticker ",
                      "and your internet connection. (", paste(errors, collapse = " | "), ")"))
}

# Aggregate daily OHLC data to weekly or monthly bars
# (open = first, high = max, low = min, close = last, volume = sum).
to_timeframe <- function(x, time_frame) {
  if (time_frame == "Daily") return(x)
  period <- if (time_frame == "Weekly") "weeks" else "months"
  agg <- to.period(x, period = period, indexAt = "endof", name = "agg")
  colnames(agg) <- sub(".*\\.", "", colnames(agg))
  agg[, c("Open", "High", "Low", "Close", "Volume")]
}

# Convert an xts object into a plain data.frame (ggplot2 friendly)
xts_to_df <- function(x) {
  data.frame(Date = as.Date(index(x)), coredata(x), row.names = NULL)
}


# ==============================================================================
# SECTION 3 (helpers): TECHNICAL INDICATORS
# Defined here so the UI/server sections below stay readable.
# ==============================================================================

# Adds SMA (short & long), RSI and MACD columns to the price data.frame.
# Each indicator is wrapped in tryCatch: if there is not enough data for the
# chosen period, the column is filled with NA instead of crashing the app.
add_indicators <- function(df, p) {
  n    <- nrow(df)
  safe <- function(expr) tryCatch(as.numeric(expr), error = function(e) rep(NA_real_, n))

  df$SMA_short <- safe(SMA(df$Close, n = p$short_n))
  df$SMA_long  <- safe(SMA(df$Close, n = p$long_n))
  df$RSI       <- safe(RSI(df$Close, n = p$rsi_n))

  macd <- tryCatch(
    MACD(df$Close, nFast = p$macd_fast, nSlow = p$macd_slow, nSig = p$macd_sig, maType = "EMA"),
    error = function(e) matrix(NA_real_, n, 2, dimnames = list(NULL, c("macd", "signal")))
  )
  df$MACD        <- as.numeric(macd[, "macd"])
  df$MACD_signal <- as.numeric(macd[, "signal"])
  df$MACD_hist   <- df$MACD - df$MACD_signal
  df
}


# ==============================================================================
# SECTION 4 (helpers): TRADING RULES
# ==============================================================================

# Moving Average Crossover Trading Rule
#   mode = "cross" : signal only on the bar where the crossover happens
#       Buy  -> short MA crosses ABOVE the long MA
#       Sell -> short MA crosses BELOW the long MA
#       Hold -> any other bar
#   mode = "state" : template version from the assignment, every bar is labelled
#       Buy if short MA > long MA, Sell if short MA < long MA, otherwise Hold
generate_signals <- function(short_ma, long_ma, mode = "cross") {
  n      <- length(short_ma)
  signal <- rep("Hold", n)
  valid  <- !is.na(short_ma) & !is.na(long_ma)

  if (mode == "state") {
    signal[valid & short_ma > long_ma] <- "Buy"
    signal[valid & short_ma < long_ma] <- "Sell"
    return(signal)
  }

  # Previous bar's values (lagged by one) are needed to detect a crossover
  prev_short <- c(NA_real_, short_ma[-n])
  prev_long  <- c(NA_real_, long_ma[-n])
  ok         <- valid & !is.na(prev_short) & !is.na(prev_long)

  signal[ok & short_ma > long_ma & prev_short <= prev_long] <- "Buy"
  signal[ok & short_ma < long_ma & prev_short >= prev_long] <- "Sell"
  signal
}

# Applies the crossover rule and (optionally) the RSI filter:
#   - a Buy is ignored (-> Hold) when RSI is above the overbought level
#   - a Sell is ignored (-> Hold) when RSI is below the oversold level
apply_trading_rules <- function(df, p) {
  df$Signal <- generate_signals(df$SMA_short, df$SMA_long, p$signal_mode)

  if (isTRUE(p$use_rsi_filter)) {
    blocked_buy  <- df$Signal == "Buy"  & !is.na(df$RSI) & df$RSI > p$rsi_overbought
    blocked_sell <- df$Signal == "Sell" & !is.na(df$RSI) & df$RSI < p$rsi_oversold
    df$Signal[blocked_buy | blocked_sell] <- "Hold"
  }
  df
}


# ==============================================================================
# SECTION 2: VISUALIZING STOCK DATA - USER INTERFACE
# ==============================================================================

# Small CSS block for the summary tiles
app_css <- "
  .metric-row { display:flex; flex-wrap:wrap; gap:10px; margin-bottom:12px; }
  .metric { background:#f4f6fb; border-radius:8px; padding:8px 14px; min-width:120px; }
  .metric-label { font-size:12px; color:#5b6475; }
  .metric-value { font-size:20px; font-weight:600; color:#1f3b73; }
  .metric.pos .metric-value { color:#1b9e5a; }
  .metric.neg .metric-value { color:#d64545; }
  .help-note { font-size:12px; color:#5b6475; }
"

# JavaScript conditions used by conditionalPanel() to show/hide controls & plots
cond_rsi  <- "input.technical_indicators && input.technical_indicators.indexOf('RSI') > -1"
cond_macd <- "input.technical_indicators && input.technical_indicators.indexOf('MACD') > -1"

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  titlePanel("Portfolio Technical Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # ---- Data selection widgets ----
      h4("Data"),
      selectizeInput("symbol", "Stock symbol:", choices = portfolio_symbols,
                     selected = "AAPL",
                     options = list(create = TRUE, placeholder = "Type any Yahoo Finance ticker")),
      dateRangeInput("date_range", "Select Date Range:",
                     start = "2023-01-01", end = "2023-07-01", max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart Type:",
                  choices = c("Line", "Candlestick", "Area"), selected = "Candlestick"),

      hr(),

      # ---- Technical indicator toggles (SECTION 3) ----
      h4("Technical Indicators"),
      checkboxGroupInput("technical_indicators", "Show on chart:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = "Moving Averages"),
      p(class = "help-note",
        "Moving Averages are drawn on top of the price chart. RSI and MACD use a different ",
        "scale, so they appear in panels aligned under the price chart."),

      conditionalPanel(
        paste0("input.use_rsi_filter || (", cond_rsi, ")"),
        numericInput("rsi_n", "RSI period:", value = 14, min = 2, max = 100, step = 1)
      ),
      conditionalPanel(
        cond_macd,
        fluidRow(
          column(4, numericInput("macd_fast", "Fast", value = 12, min = 2, max = 100, step = 1)),
          column(4, numericInput("macd_slow", "Slow", value = 26, min = 3, max = 200, step = 1)),
          column(4, numericInput("macd_sig",  "Signal", value = 9, min = 2, max = 100, step = 1))
        )
      ),

      hr(),

      # ---- Trading rules (SECTION 4) ----
      h4("Trading Rules"),
      fluidRow(
        column(6, numericInput("short_n", "Short MA:", value = 20, min = 2, max = 200, step = 1)),
        column(6, numericInput("long_n",  "Long MA:",  value = 50, min = 3, max = 400, step = 1))
      ),
      radioButtons("signal_mode", "Signal style:",
                   choices = c("Crossover events only" = "cross",
                               "Label every bar (assignment template)" = "state")),
      checkboxInput("use_rsi_filter", "Filter signals with RSI", value = FALSE),
      conditionalPanel(
        "input.use_rsi_filter",
        fluidRow(
          column(6, numericInput("rsi_oversold",   "Oversold <",   value = 30, min = 1, max = 49, step = 1)),
          column(6, numericInput("rsi_overbought", "Overbought >", value = 70, min = 51, max = 99, step = 1))
        )
      ),
      checkboxInput("show_signals", "Annotate chart with Buy/Sell signals", value = TRUE),
      checkboxInput("show_hold", "Also label Hold bars", value = FALSE)
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Chart",
          br(),
          uiOutput("summary"),
          plotOutput("price_plot", height = "480px"),
          conditionalPanel(cond_rsi,  plotOutput("rsi_plot",  height = "200px")),
          conditionalPanel(cond_macd, plotOutput("macd_plot", height = "220px"))
        ),
        tabPanel(
          "Signals",
          br(),
          downloadButton("download_signals", "Download signals (CSV)"),
          br(), br(),
          tableOutput("signal_table")
        ),
        tabPanel(
          "Trading rules",
          br(),
          h4("Moving Average Crossover rule"),
          tags$ul(
            tags$li("Buy: the short-term MA crosses above the long-term MA."),
            tags$li("Sell: the short-term MA crosses below the long-term MA."),
            tags$li("Hold: any other bar.")
          ),
          h4("Optional RSI filter"),
          tags$ul(
            tags$li("A Buy is ignored (becomes Hold) when RSI is above the overbought level."),
            tags$li("A Sell is ignored (becomes Hold) when RSI is below the oversold level.")
          ),
          p("The short and long MA periods, the RSI period and levels, and the MACD parameters ",
            "are all customizable from the sidebar. This dashboard is for educational purposes only ",
            "and is not financial advice.")
        )
      )
    )
  )
)


# ==============================================================================
# SECTION 2-4: SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {

  # ---- Validated indicator / rule parameters ----------------------------------
  params <- reactive({
    req(input$short_n, input$long_n, input$rsi_n, input$macd_fast, input$macd_slow,
        input$macd_sig, input$rsi_oversold, input$rsi_overbought)
    validate(
      need(input$short_n < input$long_n, "The short MA period must be smaller than the long MA period."),
      need(input$macd_fast < input$macd_slow, "MACD: the fast period must be smaller than the slow period."),
      need(input$rsi_oversold < input$rsi_overbought, "RSI: the oversold level must be below the overbought level.")
    )
    list(
      short_n = as.integer(input$short_n), long_n = as.integer(input$long_n),
      rsi_n = as.integer(input$rsi_n),
      macd_fast = as.integer(input$macd_fast), macd_slow = as.integer(input$macd_slow),
      macd_sig = as.integer(input$macd_sig),
      signal_mode = input$signal_mode, use_rsi_filter = isTRUE(input$use_rsi_filter),
      rsi_oversold = input$rsi_oversold, rsi_overbought = input$rsi_overbought
    )
  })

  # ---- Step 1: download the data (only re-runs if symbol or dates change) -----
  raw_data <- reactive({
    req(input$symbol, input$date_range)
    validate(
      need(!any(is.na(input$date_range)), "Please select a valid date range."),
      need(input$date_range[1] < input$date_range[2], "The start date must be before the end date.")
    )
    symbol <- toupper(trimws(input$symbol))
    from   <- input$date_range[1] - history_buffer_days
    to     <- input$date_range[2]

    result <- withProgress(message = paste("Downloading", symbol, "..."), value = 0.5, {
      get_stock_data(symbol, from, to)
    })
    validate(need(!is.null(result$data), result$error))
    result
  })

  # ---- Step 2: time frame, indicators, trading rules, date filtering ----------
  analysis <- reactive({
    res <- raw_data()
    p   <- params()

    # Aggregate to the chosen time frame, then compute indicators and signals on
    # the FULL history so they are correct from the first visible bar.
    df <- xts_to_df(to_timeframe(res$data, input$time_frame))
    validate(need(nrow(df) >= 2, "Not enough data for this time frame."))
    df <- add_indicators(df, p)
    df <- apply_trading_rules(df, p)

    # Filter to the date range selected by the user
    view <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(view) >= 2,
                  "No data in the selected range. Try a wider date range or a shorter time frame."))

    list(view = view, source = res$source, symbol = res$symbol)
  })

  # Helper: x-axis limits shared by the three plots so they line up
  x_limits <- function(d) {
    pad <- switch(input$time_frame, Daily = 1, Weekly = 5, Monthly = 20)
    c(min(d$Date) - pad, max(d$Date) + pad)
  }

  # ---- Summary tiles -----------------------------------------------------------
  output$summary <- renderUI({
    a <- analysis()
    d <- a$view
    change <- (tail(d$Close, 1) / head(d$Close, 1) - 1) * 100
    metric <- function(label, value, cls = "") {
      div(class = paste("metric", cls),
          div(class = "metric-label", label), div(class = "metric-value", value))
    }
    div(class = "metric-row",
        metric(paste(a$symbol, "last close"), sprintf("%.2f", tail(d$Close, 1))),
        metric("Change in period", sprintf("%+.1f%%", change), if (change >= 0) "pos" else "neg"),
        metric("Buy signals", sum(d$Signal == "Buy")),
        metric("Sell signals", sum(d$Signal == "Sell")),
        metric("Data source", a$source))
  })

  # ---- SECTION 2 + 3 + 4: main price chart -------------------------------------
  output$price_plot <- renderPlot({
    a   <- analysis()
    d   <- a$view
    sel <- input$technical_indicators
    d$Direction <- ifelse(d$Close >= d$Open, "Up", "Down")   # candle colour

    # One colour scale shared by price, candles and the MA lines
    cols <- c(Close = "#1f3b73", Up = "#1b9e5a", Down = "#d64545",
              "Short MA" = "#f28e2b", "Long MA" = "#7b3fa0")

    p <- ggplot(d, aes(x = Date))

    # --- Chart type (line / candlestick / area) ---
    if (input$chart_type == "Candlestick") {
      w <- switch(input$time_frame, Daily = 0.35, Weekly = 2.5, Monthly = 10)
      p <- p +
        geom_segment(aes(xend = Date, y = Low, yend = High, color = Direction)) +
        geom_rect(aes(xmin = Date - w, xmax = Date + w,
                      ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                      fill = Direction, color = Direction))
    } else if (input$chart_type == "Area") {
      y_floor <- min(d$Close, na.rm = TRUE) * 0.98
      p <- p +
        geom_ribbon(aes(ymin = y_floor, ymax = Close), fill = "#1f3b73", alpha = 0.18) +
        geom_line(aes(y = Close, color = "Close"), linewidth = 0.8)
    } else {
      p <- p + geom_line(aes(y = Close, color = "Close"), linewidth = 0.8)
    }

    # --- Technical indicator overlay (toggled by the checkboxes) ---
    if ("Moving Averages" %in% sel) {
      p <- p +
        geom_line(aes(y = SMA_short, color = "Short MA"), linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = SMA_long,  color = "Long MA"),  linewidth = 0.8, na.rm = TRUE)
    }

    # --- Trading signal annotations ---
    if (isTRUE(input$show_signals)) {
      rng   <- diff(range(c(d$Low, d$High), na.rm = TRUE))
      off   <- 0.04 * rng
      buys  <- d[d$Signal == "Buy", ]
      sells <- d[d$Signal == "Sell", ]

      if (nrow(buys) > 0) {
        p <- p +
          geom_point(data = buys, aes(y = Low - off), shape = 24, size = 3,
                     fill = "#1b9e5a", color = "#0d5c34") +
          geom_text(data = buys, aes(y = Low - off, label = "BUY"), vjust = 2,
                    size = 3, fontface = "bold", color = "#0d5c34")
      }
      if (nrow(sells) > 0) {
        p <- p +
          geom_point(data = sells, aes(y = High + off), shape = 25, size = 3,
                     fill = "#d64545", color = "#8e1f1f") +
          geom_text(data = sells, aes(y = High + off, label = "SELL"), vjust = -1.2,
                    size = 3, fontface = "bold", color = "#8e1f1f")
      }
      if (isTRUE(input$show_hold)) {
        holds <- d[d$Signal == "Hold", ]
        if (nrow(holds) > 0) {
          p <- p + geom_text(data = holds, aes(y = Close, label = "Hold"), vjust = -1,
                             size = 2.4, color = "grey50", check_overlap = TRUE)
        }
      }
    }

    # Legend: list only the lines (price / moving averages). The Up/Down candle
    # colours are self-explanatory and their legend keys rendered as grey boxes.
    legend_breaks <- as.character(c(
      if (input$chart_type != "Candlestick") "Close",
      if ("Moving Averages" %in% sel) c("Short MA", "Long MA")
    ))

    p +
      scale_color_manual(values = cols, breaks = legend_breaks, name = NULL) +
      scale_fill_manual(values = cols, guide = "none") +
      scale_y_continuous(expand = expansion(mult = 0.08)) +
      coord_cartesian(xlim = x_limits(d)) +
      labs(title = sprintf("%s - %s chart (%s)", a$symbol, input$chart_type, input$time_frame),
           subtitle = sprintf("Source: %s | MA rule: %d / %d", a$source,
                              params()$short_n, params()$long_n),
           x = NULL, y = "Price") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  })

  # ---- RSI panel (shown only when "RSI" is ticked) ------------------------------
  output$rsi_plot <- renderPlot({
    d  <- analysis()$view
    ob <- if (isTRUE(input$use_rsi_filter)) input$rsi_overbought else 70
    os <- if (isTRUE(input$use_rsi_filter)) input$rsi_oversold   else 30

    ggplot(d, aes(x = Date, y = RSI)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = ob, ymax = 100, fill = "#d64545", alpha = 0.08) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = os,  fill = "#1b9e5a", alpha = 0.08) +
      geom_hline(yintercept = c(os, ob), linetype = "dashed", color = "grey50") +
      geom_line(color = "#7b3fa0", linewidth = 0.8, na.rm = TRUE) +
      scale_y_continuous(limits = c(0, 100)) +
      coord_cartesian(xlim = x_limits(d)) +
      labs(title = sprintf("RSI (%d)", params()$rsi_n), x = NULL, y = "RSI") +
      theme_minimal(base_size = 12)
  })

  # ---- MACD panel (shown only when "MACD" is ticked) ----------------------------
  output$macd_plot <- renderPlot({
    d <- analysis()$view
    p <- params()
    d$Positive <- ifelse(is.na(d$MACD_hist) | d$MACD_hist >= 0, "pos", "neg")

    ggplot(d, aes(x = Date)) +
      geom_col(aes(y = MACD_hist, fill = Positive), alpha = 0.5, na.rm = TRUE) +
      geom_hline(yintercept = 0, color = "grey50") +
      geom_line(aes(y = MACD,        color = "MACD"),   linewidth = 0.8, na.rm = TRUE) +
      geom_line(aes(y = MACD_signal, color = "Signal"), linewidth = 0.8, na.rm = TRUE) +
      scale_fill_manual(values = c(pos = "#1b9e5a", neg = "#d64545"), guide = "none") +
      scale_color_manual(values = c(MACD = "#1f3b73", Signal = "#f28e2b"), name = NULL) +
      coord_cartesian(xlim = x_limits(d)) +
      labs(title = sprintf("MACD (%d, %d, %d)", p$macd_fast, p$macd_slow, p$macd_sig),
           x = NULL, y = "MACD") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  # ---- Signals table & CSV download ---------------------------------------------
  signals_df <- reactive({
    d   <- analysis()$view
    sig <- d[d$Signal %in% c("Buy", "Sell"),
             c("Date", "Close", "Signal", "SMA_short", "SMA_long", "RSI")]
    names(sig) <- c("Date", "Close", "Signal", "Short MA", "Long MA", "RSI")
    sig$Date   <- format(sig$Date, "%Y-%m-%d")
    sig
  })

  output$signal_table <- renderTable({
    sig <- signals_df()
    validate(need(nrow(sig) > 0, "No Buy/Sell signals in this period with the current settings."))
    sig
  }, digits = 2, na = "-")

  output$download_signals <- downloadHandler(
    filename = function() paste0(toupper(trimws(input$symbol)), "_signals.csv"),
    content  = function(file) write.csv(signals_df(), file, row.names = FALSE)
  )
}


# ==============================================================================
# RUN THE APP
# ==============================================================================
shinyApp(ui, server)
