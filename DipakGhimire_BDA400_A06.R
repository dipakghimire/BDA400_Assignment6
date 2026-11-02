# =============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Portfolio Visualization Dashboard (R Shiny)
# Author: Dipak Ghimire
#
# Run with:  shiny::runApp("DipakGhimire_BDA400_A06.R")   or press "Run App" in RStudio
# =============================================================================


# =============================================================================
# STEP 1: DATA COLLECTION AND SETUP
# =============================================================================

# ---- 1.1 Data source ---------------------------------------------------------
# Primary source : Yahoo Finance (through quantmod::getSymbols)
# Backup source  : Stooq (daily CSV download). It is used automatically if Yahoo
#                  fails, and the user can also pick it manually in the sidebar.

# ---- 1.2 Install and load the necessary packages ----------------------------
# The first time the script runs, any missing package is installed. This is the
# same as calling install.packages("shiny"), install.packages("ggplot2"), etc.
required_pkgs <- c("shiny", "ggplot2", "quantmod", "TTR", "xts", "zoo")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(shiny)
library(ggplot2)
library(quantmod)   # getSymbols() and the Op/Hi/Lo/Cl/Vo helpers
library(TTR)        # SMA(), RSI(), MACD()
library(xts)        # time series objects and to.weekly()/to.monthly()
library(zoo)

# ---- 1.3 Default stock symbol and dates (same values as in the brief) -------
stock_symbol <- "AAPL"
start_date   <- "2023-01-01"
end_date     <- "2023-07-01"

# How many calendar days of history are downloaded BEFORE the chosen start date.
# Moving averages need earlier observations, otherwise the first 50 bars of the
# chart would have no long MA and no signals.
WARMUP_DAYS <- 1825

# ---- 1.4 Fetch historical stock data ----------------------------------------

# Yahoo Finance via quantmod. auto.assign = FALSE returns the xts object directly
# instead of creating a variable in the global environment.
fetch_yahoo <- function(symbol, from, to) {
  stock_data <- getSymbols(symbol, src = "yahoo", from = from, to = to,
                           auto.assign = FALSE)
  data.frame(
    Date   = as.Date(index(stock_data)),
    Open   = as.numeric(Op(stock_data)),
    High   = as.numeric(Hi(stock_data)),
    Low    = as.numeric(Lo(stock_data)),
    Close  = as.numeric(Cl(stock_data)),
    Volume = as.numeric(Vo(stock_data))
  )
}

# Stooq as a second source. US tickers need the ".us" suffix on Stooq.
fetch_stooq <- function(symbol, from, to) {
  sym <- tolower(symbol)
  if (!grepl("\\.", sym)) sym <- paste0(sym, ".us")
  url <- sprintf("https://stooq.com/q/d/l/?s=%s&d1=%s&d2=%s&i=d",
                 sym, format(as.Date(from), "%Y%m%d"), format(as.Date(to), "%Y%m%d"))
  raw <- read.csv(url, stringsAsFactors = FALSE)
  needed <- c("Date", "Open", "High", "Low", "Close")
  if (!all(needed %in% names(raw))) stop("Stooq did not return usable price data")
  if (!"Volume" %in% names(raw)) raw$Volume <- 0
  raw$Date <- as.Date(raw$Date)
  raw[, c("Date", "Open", "High", "Low", "Close", "Volume")]
}

# Clean whatever a source returned: drop rows with missing prices, sort by date,
# and make sure there is enough data left to work with.
clean_prices <- function(df) {
  df <- df[complete.cases(df[, c("Open", "High", "Low", "Close")]), ]
  df <- df[order(df$Date), ]
  df <- df[!duplicated(df$Date), ]
  if (nrow(df) < 2) stop("fewer than 2 valid price rows were returned")
  df$Volume[is.na(df$Volume)] <- 0
  rownames(df) <- NULL
  df
}

# Simple in-memory cache so changing a slider does not trigger a new download.
price_cache <- new.env()

# Try the selected source first, optionally fall back to the other one.
# Always returns a list: $data (data.frame or NULL), $source, $msg.
load_prices <- function(symbol, from, to, source = "yahoo", fallback = TRUE) {
  key <- paste(symbol, from, to, source, fallback, sep = "|")
  if (exists(key, envir = price_cache, inherits = FALSE)) {
    return(get(key, envir = price_cache))
  }

  sources <- if (source == "yahoo") c("yahoo", "stooq") else c("stooq", "yahoo")
  if (!fallback) sources <- sources[1]

  errors <- character(0)
  result <- NULL
  for (src in sources) {
    attempt <- tryCatch({
      raw <- if (src == "yahoo") fetch_yahoo(symbol, from, to) else fetch_stooq(symbol, from, to)
      list(data = clean_prices(raw), source = src, msg = "")
    }, error = function(e) {
      errors <<- c(errors, paste0(src, ": ", conditionMessage(e)))
      NULL
    })
    if (!is.null(attempt)) {
      result <- attempt
      break
    }
  }

  if (is.null(result)) {
    result <- list(
      data = NULL, source = NA,
      msg = paste0("Could not download data for '", symbol, "'. Check the symbol and ",
                   "your internet connection. Details: ", paste(errors, collapse = " | "))
    )
  } else {
    assign(key, result, envir = price_cache)   # only cache successful downloads
  }
  result
}


# =============================================================================
# STEP 2 (helpers): TIME FRAMES
# =============================================================================

# Convert daily bars to weekly or monthly bars (OHLC aggregation, volume summed).
resample_prices <- function(df, frame) {
  if (frame == "Daily") return(df)
  x <- xts(df[, c("Open", "High", "Low", "Close", "Volume")], order.by = df$Date)
  r <- if (frame == "Weekly") to.weekly(x, indexAt = "startof") else to.monthly(x, indexAt = "startof")
  out <- data.frame(Date = as.Date(index(r)), coredata(r))
  names(out) <- c("Date", "Open", "High", "Low", "Close", "Volume")
  out
}


# =============================================================================
# STEP 3 (helpers): TECHNICAL INDICATORS
# =============================================================================

# TTR functions throw an error when the series is shorter than the window, so
# every indicator goes through this guard and returns NA instead.
guarded <- function(n_needed, len, f) {
  if (len >= n_needed) as.numeric(f()) else rep(NA_real_, len)
}

add_indicators <- function(d, prm) {
  n <- nrow(d)

  # Moving averages (short and long)
  d$SMA_short <- guarded(prm$short, n, function() SMA(d$Close, n = prm$short))
  d$SMA_long  <- guarded(prm$long,  n, function() SMA(d$Close, n = prm$long))

  # Relative Strength Index
  d$RSI <- guarded(prm$rsi_n + 1, n, function() RSI(d$Close, n = prm$rsi_n))

  # MACD: macd line, signal line, and histogram (macd - signal)
  macd_ok <- n >= (prm$slow + prm$sig)
  if (macd_ok) {
    m <- MACD(d$Close, nFast = prm$fast, nSlow = prm$slow, nSig = prm$sig, maType = "EMA")
    d$MACD        <- as.numeric(m[, "macd"])
    d$MACD_signal <- as.numeric(m[, "signal"])
  } else {
    d$MACD <- NA_real_
    d$MACD_signal <- NA_real_
  }
  d$MACD_hist <- d$MACD - d$MACD_signal
  d
}


# =============================================================================
# STEP 4 (helpers): TRADING RULES
# =============================================================================

# Moving Average Crossover rule
#   Buy  : short MA crosses ABOVE the long MA (on this bar)
#   Sell : short MA crosses BELOW the long MA (on this bar)
#   Hold : everything else
# Optional RSI confirmation (user can switch it on):
#   a Buy is ignored when RSI is above the overbought level
#   a Sell is ignored when RSI is below the oversold level
generate_signals <- function(d, prm) {
  n <- nrow(d)
  sig <- rep("Hold", n)
  if (n < 2) return(sig)

  s <- d$SMA_short
  l <- d$SMA_long
  s_prev <- c(NA, s[-n])
  l_prev <- c(NA, l[-n])

  cross_up   <- s_prev <= l_prev & s > l
  cross_down <- s_prev >= l_prev & s < l
  cross_up[is.na(cross_up)]     <- FALSE
  cross_down[is.na(cross_down)] <- FALSE

  if (isTRUE(prm$use_rsi)) {
    r <- d$RSI
    cross_up   <- cross_up   & !(!is.na(r) & r > prm$ob)
    cross_down <- cross_down & !(!is.na(r) & r < prm$os)
  }

  sig[cross_up]   <- "Buy"
  sig[cross_down] <- "Sell"
  sig
}


# =============================================================================
# CHART BUILDERS
# =============================================================================

col_up   <- "#2e9e5b"
col_down <- "#d64545"

# ---- Main price chart: line / candlestick / area + MA overlay + signals ------
build_price_chart <- function(d, chart_type, frame, prm, show_ma, show_signals, show_hold, symbol) {

  p <- ggplot(d, aes(x = Date))

  if (chart_type == "Line") {
    p <- p + geom_line(aes(y = Close), color = "#333333", linewidth = 0.7)

  } else if (chart_type == "Area") {
    floor_y <- min(d$Low) * 0.98   # start the fill near the data, not at zero
    p <- p +
      geom_ribbon(aes(ymin = floor_y, ymax = Close), fill = "#4c78a8", alpha = 0.25) +
      geom_line(aes(y = Close), color = "#4c78a8", linewidth = 0.7)

  } else {  # Candlestick, drawn with segments (wicks) and rectangles (bodies)
    w <- switch(frame, Daily = 0.35, Weekly = 2, Monthly = 10)
    d$Direction <- ifelse(d$Close >= d$Open, "Up", "Down")
    p <- p +
      geom_segment(data = d, aes(xend = Date, y = Low, yend = High), color = "#555555", linewidth = 0.4) +
      geom_rect(data = d, aes(xmin = Date - w, xmax = Date + w,
                              ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                              fill = Direction), color = "#555555", linewidth = 0.2) +
      scale_fill_manual(values = c(Up = col_up, Down = col_down), guide = "none")
  }

  # Moving averages overlay
  if (show_ma) {
    lab_s <- paste0("SMA ", prm$short)
    lab_l <- paste0("SMA ", prm$long)
    p <- p +
      geom_line(aes(y = SMA_short, colour = lab_s), linewidth = 0.9, na.rm = TRUE) +
      geom_line(aes(y = SMA_long,  colour = lab_l), linewidth = 0.9, na.rm = TRUE) +
      scale_colour_manual(values = setNames(c("#1f77b4", "#ff7f0e"), c(lab_s, lab_l)), name = NULL)
  }

  # Signal annotations on the bars
  if (show_signals) {
    buys  <- d[d$Signal == "Buy", ]
    sells <- d[d$Signal == "Sell", ]
    holds <- d[d$Signal == "Hold", ]
    span  <- max(d$High) - min(d$Low)

    if (show_hold && nrow(holds) > 0) {
      p <- p + geom_text(data = holds, aes(y = High, label = "Hold"),
                         vjust = -0.6, size = 2.2, color = "grey55", check_overlap = TRUE)
    }
    if (nrow(buys) > 0) {
      p <- p +
        geom_point(data = buys, aes(y = Low - 0.03 * span), shape = 24,
                   fill = col_up, color = "black", size = 4) +
        geom_text(data = buys, aes(y = Low - 0.03 * span, label = "Buy"),
                  vjust = 2.1, color = col_up, fontface = "bold", size = 3.5)
    }
    if (nrow(sells) > 0) {
      p <- p +
        geom_point(data = sells, aes(y = High + 0.03 * span), shape = 25,
                   fill = col_down, color = "black", size = 4) +
        geom_text(data = sells, aes(y = High + 0.03 * span, label = "Sell"),
                  vjust = -1.2, color = col_down, fontface = "bold", size = 3.5)
    }
  }

  p +
    scale_x_date(expand = expansion(mult = 0.02)) +
    scale_y_continuous(expand = expansion(mult = 0.1)) +
    labs(title = paste0(symbol, " - ", chart_type, " chart (", frame, ")"),
         x = NULL, y = "Price (USD)") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))
}

# ---- RSI panel ---------------------------------------------------------------
build_rsi_chart <- function(d, prm) {
  ggplot(d, aes(x = Date, y = RSI)) +
    annotate("rect", xmin = min(d$Date), xmax = max(d$Date),
             ymin = prm$os, ymax = prm$ob, fill = "grey90", alpha = 0.6) +
    geom_hline(yintercept = c(prm$os, prm$ob), linetype = "dashed", color = "grey40") +
    geom_line(color = "#7b3fa0", linewidth = 0.8, na.rm = TRUE) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_date(expand = expansion(mult = 0.02)) +
    labs(title = paste0("RSI (", prm$rsi_n, ")"), x = NULL, y = "RSI") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
}

# ---- MACD panel --------------------------------------------------------------
build_macd_chart <- function(d, prm) {
  d$HistDir <- ifelse(is.na(d$MACD_hist) | d$MACD_hist >= 0, "Pos", "Neg")
  ggplot(d, aes(x = Date)) +
    geom_col(aes(y = MACD_hist, fill = HistDir), width = 0.8, na.rm = TRUE) +
    geom_line(aes(y = MACD, colour = "MACD"), linewidth = 0.8, na.rm = TRUE) +
    geom_line(aes(y = MACD_signal, colour = "Signal"), linewidth = 0.8, na.rm = TRUE) +
    geom_hline(yintercept = 0, color = "grey40") +
    scale_fill_manual(values = c(Pos = col_up, Neg = col_down), guide = "none") +
    scale_colour_manual(values = c(MACD = "#1f77b4", Signal = "#ff7f0e"), name = NULL) +
    scale_x_date(expand = expansion(mult = 0.02)) +
    labs(title = paste0("MACD (", prm$fast, ", ", prm$slow, ", ", prm$sig, ")"),
         x = NULL, y = "MACD") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))
}


# =============================================================================
# STEP 2: SHINY APP - USER INTERFACE
# =============================================================================

popular_symbols <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA", "META", "RY.TO", "SHOP.TO")

ui <- fluidPage(
  titlePanel("Portfolio Technical Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Data"),
      # selectize with create = TRUE lets the user pick a preset or type any ticker
      selectizeInput("symbol", "Stock Symbol:", choices = popular_symbols,
                     selected = stock_symbol, options = list(create = TRUE)),
      dateRangeInput("date_range", "Select Date Range:",
                     start = start_date, end = end_date),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart Type:",
                  choices = c("Candlestick", "Line", "Area")),
      selectInput("source", "Data Source:",
                  choices = c("Yahoo Finance" = "yahoo", "Stooq" = "stooq")),
      checkboxInput("fallback", "Use the other source if the first one fails", TRUE),

      hr(),
      h4("Technical Indicators"),
      checkboxGroupInput("technical_indicators", "Show on chart:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = "Moving Averages"),
      conditionalPanel(
        "input.technical_indicators.indexOf('Moving Averages') > -1",
        sliderInput("short_n", "Short MA window:", min = 5, max = 50, value = 20, step = 1),
        sliderInput("long_n",  "Long MA window:",  min = 20, max = 200, value = 50, step = 1)
      ),
      conditionalPanel(
        "input.technical_indicators.indexOf('RSI') > -1",
        sliderInput("rsi_n", "RSI period:", min = 5, max = 30, value = 14, step = 1)
      ),
      conditionalPanel(
        "input.technical_indicators.indexOf('MACD') > -1",
        numericInput("macd_fast", "MACD fast EMA:", value = 12, min = 2, max = 50),
        numericInput("macd_slow", "MACD slow EMA:", value = 26, min = 5, max = 100),
        numericInput("macd_sig",  "MACD signal EMA:", value = 9, min = 2, max = 50)
      ),

      hr(),
      h4("Trading Rule"),
      helpText("Buy when the short MA crosses above the long MA, sell when it crosses below, otherwise hold."),
      checkboxInput("show_signals", "Annotate Buy / Sell signals on chart", TRUE),
      checkboxInput("show_hold", "Also label Hold bars", FALSE),
      checkboxInput("use_rsi", "Confirm signals with RSI", FALSE),
      conditionalPanel(
        "input.use_rsi",
        sliderInput("rsi_levels", "RSI oversold / overbought:", min = 10, max = 90,
                    value = c(30, 70), step = 1)
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Technical Analysis",
          br(),
          textOutput("status"),
          uiOutput("summary_box"),
          plotOutput("stock_chart", height = "430px"),
          conditionalPanel("input.technical_indicators.indexOf('RSI') > -1",
                           plotOutput("rsi_chart", height = "190px")),
          conditionalPanel("input.technical_indicators.indexOf('MACD') > -1",
                           plotOutput("macd_chart", height = "220px"))
        ),
        tabPanel(
          "Signals Log",
          br(),
          downloadButton("download_signals", "Download signals (CSV)"),
          br(), br(),
          tableOutput("signal_table")
        ),
        tabPanel(
          "Portfolio Comparison",
          br(),
          selectizeInput("cmp_symbols", "Stocks to compare:", choices = popular_symbols,
                         selected = c("AAPL", "MSFT", "GOOGL"), multiple = TRUE,
                         options = list(create = TRUE)),
          helpText("Each series is rebased to 100 at the start of the selected date range."),
          plotOutput("compare_chart", height = "450px"),
          textOutput("compare_status")
        )
      )
    )
  )
)


# =============================================================================
# STEP 2-4: SHINY APP - SERVER LOGIC
# =============================================================================

server <- function(input, output) {

  # ---- Download the raw daily data (only re-runs if symbol/dates/source change)
  price_data <- reactive({
    sym <- toupper(trimws(input$symbol))
    req(nzchar(sym), input$date_range)
    from <- as.Date(input$date_range[1]) - WARMUP_DAYS
    to   <- as.Date(input$date_range[2]) + 1
    withProgress(message = paste("Downloading", sym, "..."), value = 0.5, {
      load_prices(sym, from, to, input$source, input$fallback)
    })
  })

  # ---- Collect indicator parameters in one list
  prm <- reactive({
    list(
      short = input$short_n, long = input$long_n,
      rsi_n = input$rsi_n,
      fast = input$macd_fast, slow = input$macd_slow, sig = input$macd_sig,
      use_rsi = input$use_rsi,
      os = input$rsi_levels[1], ob = input$rsi_levels[2]
    )
  })

  # ---- Main analysis: resample, add indicators, create signals, then filter.
  # Indicators and signals are calculated on the full history first and the date
  # filter is applied afterwards, so the first bars of the chart are not empty.
  analysis <- reactive({
    raw <- price_data()
    validate(need(!is.null(raw$data), raw$msg))
    req(input$date_range, input$short_n, input$long_n, input$rsi_n,
        input$macd_fast, input$macd_slow, input$macd_sig)

    p <- prm()
    validate(
      need(input$date_range[1] < input$date_range[2], "The start date must be before the end date."),
      need(p$short < p$long, "The short MA window must be smaller than the long MA window."),
      need(p$fast < p$slow, "The MACD fast EMA must be smaller than the slow EMA.")
    )

    full <- resample_prices(raw$data, input$time_frame)
    full <- add_indicators(full, p)
    full$Signal <- generate_signals(full, p)
    full$Trend  <- ifelse(is.na(full$SMA_short) | is.na(full$SMA_long), NA,
                          ifelse(full$SMA_short > full$SMA_long, "Bullish", "Bearish"))

    # Filter based on date range (the filtering step from the assignment)
    filtered_data <- full[full$Date >= input$date_range[1] & full$Date <= input$date_range[2], ]
    validate(need(nrow(filtered_data) >= 2,
                  "Not enough bars in this date range / time frame. Widen the date range or use a shorter time frame."))
    filtered_data
  })

  # ---- Status line
  output$status <- renderText({
    raw <- price_data()
    if (is.null(raw$data)) return(raw$msg)
    paste0("Data source: ", if (raw$source == "yahoo") "Yahoo Finance" else "Stooq",
           if (raw$source != input$source) " (backup source used)" else "")
  })

  # ---- Quick summary under the status line
  output$summary_box <- renderUI({
    d <- analysis()
    sym <- toupper(trimws(input$symbol))
    change <- (tail(d$Close, 1) / d$Close[1] - 1) * 100
    acted <- d[d$Signal != "Hold", ]
    last_sig <- if (nrow(acted) > 0) {
      paste0(toupper(tail(acted$Signal, 1)), " on ", format(tail(acted$Date, 1), "%b %d, %Y"))
    } else "no crossover in this range"
    trend <- tail(d$Trend, 1)

    div(style = "margin-bottom:10px; padding:8px 12px; background:#f4f6f8; border-radius:6px;",
        strong(sym), sprintf(" | Last close: $%.2f", tail(d$Close, 1)),
        sprintf(" | Change over range: %+.1f%%", change),
        " | Latest signal: ", strong(last_sig),
        if (!is.na(trend)) tagList(" | Trend: ", strong(trend)),
        sprintf(" | Buys: %d, Sells: %d", sum(d$Signal == "Buy"), sum(d$Signal == "Sell")))
  })

  # ---- Price chart (line / candlestick / area, MA overlay, signal annotations)
  output$stock_chart <- renderPlot({
    d <- analysis()
    p <- build_price_chart(
      d, input$chart_type, input$time_frame, prm(),
      show_ma      = "Moving Averages" %in% input$technical_indicators,
      show_signals = input$show_signals,
      show_hold    = input$show_hold,
      symbol       = toupper(trimws(input$symbol))
    )
    print(p)
  })

  # ---- RSI panel (only drawn when the RSI box is ticked)
  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$technical_indicators)
    d <- analysis()
    validate(need(any(!is.na(d$RSI)), "Not enough data to calculate RSI for this range."))
    print(build_rsi_chart(d, prm()))
  })

  # ---- MACD panel (only drawn when the MACD box is ticked)
  output$macd_chart <- renderPlot({
    req("MACD" %in% input$technical_indicators)
    d <- analysis()
    validate(need(any(!is.na(d$MACD)), "Not enough data to calculate MACD for this range. Try a longer date range."))
    print(build_macd_chart(d, prm()))
  })

  # ---- Signals log
  signal_log <- reactive({
    d <- analysis()
    s <- d[d$Signal != "Hold", c("Date", "Signal", "Close", "SMA_short", "SMA_long", "RSI")]
    if (nrow(s) == 0) return(data.frame(Message = "No Buy/Sell crossovers in the selected range."))
    s$Date <- format(s$Date, "%Y-%m-%d")
    names(s) <- c("Date", "Signal", "Close", paste0("SMA ", prm()$short), paste0("SMA ", prm()$long), "RSI")
    s
  })

  output$signal_table <- renderTable(signal_log(), digits = 2)

  output$download_signals <- downloadHandler(
    filename = function() paste0(toupper(trimws(input$symbol)), "_signals.csv"),
    content  = function(file) write.csv(signal_log(), file, row.names = FALSE)
  )

  # ---- Portfolio comparison (rebased to 100)
  compare_data <- reactive({
    req(input$cmp_symbols, input$date_range)
    syms <- toupper(trimws(input$cmp_symbols))
    from <- as.Date(input$date_range[1])
    to   <- as.Date(input$date_range[2]) + 1
    failed <- character(0)
    parts <- list()

    withProgress(message = "Loading portfolio...", value = 0, {
      for (s in syms) {
        res <- load_prices(s, from, to, input$source, input$fallback)
        if (is.null(res$data)) {
          failed <- c(failed, s)
        } else {
          x <- res$data[res$data$Date >= from & res$data$Date <= input$date_range[2], ]
          if (nrow(x) >= 2) {
            parts[[s]] <- data.frame(Date = x$Date, Symbol = s, Index = x$Close / x$Close[1] * 100)
          } else failed <- c(failed, s)
        }
        incProgress(1 / length(syms))
      }
    })
    list(data = if (length(parts)) do.call(rbind, parts) else NULL, failed = failed)
  })

  output$compare_chart <- renderPlot({
    cd <- compare_data()
    validate(need(!is.null(cd$data), "No data could be loaded for the selected stocks."))
    ggplot(cd$data, aes(x = Date, y = Index, colour = Symbol)) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
      geom_line(linewidth = 0.9) +
      labs(title = "Relative performance (start = 100)", x = NULL, y = "Index", colour = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))
  })

  output$compare_status <- renderText({
    cd <- compare_data()
    if (length(cd$failed)) paste("Could not load:", paste(cd$failed, collapse = ", ")) else ""
  })
}


# =============================================================================
# RUN THE APP
# =============================================================================
shinyApp(ui, server)
