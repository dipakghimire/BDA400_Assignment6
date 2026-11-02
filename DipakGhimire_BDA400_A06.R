# BDA400 - Assignment 6: Technical Analysis using R, Visualization Phase
# Dipak Ghimire

# ---------------------------------------------------------------------------
# Step 1: Data Collection and Setup
# ---------------------------------------------------------------------------

# Install (if missing) and load packages
for (pkg in c("shiny", "ggplot2", "quantmod", "TTR", "xts")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(xts)

# Default stock and dates
stock_symbol <- "AAPL"
start_date   <- "2023-01-01"
end_date     <- "2023-07-01"

# Extra history downloaded before the start date so moving averages are
# available from the first bar of the chart
WARMUP_DAYS <- 1825

# Yahoo Finance (main source)
fetch_yahoo <- function(symbol, from, to) {
  stock_data <- getSymbols(symbol, src = "yahoo", from = from, to = to,
                           auto.assign = FALSE)
  data.frame(Date   = as.Date(index(stock_data)),
             Open   = as.numeric(Op(stock_data)),
             High   = as.numeric(Hi(stock_data)),
             Low    = as.numeric(Lo(stock_data)),
             Close  = as.numeric(Cl(stock_data)),
             Volume = as.numeric(Vo(stock_data)))
}

# Stooq (backup source)
fetch_stooq <- function(symbol, from, to) {
  sym <- tolower(symbol)
  if (!grepl("\\.", sym)) sym <- paste0(sym, ".us")
  url <- sprintf("https://stooq.com/q/d/l/?s=%s&d1=%s&d2=%s&i=d", sym,
                 format(as.Date(from), "%Y%m%d"), format(as.Date(to), "%Y%m%d"))
  raw <- read.csv(url, stringsAsFactors = FALSE)
  if (!all(c("Date", "Open", "High", "Low", "Close") %in% names(raw)))
    stop("No data returned")
  if (!"Volume" %in% names(raw)) raw$Volume <- 0
  raw$Date <- as.Date(raw$Date)
  raw[, c("Date", "Open", "High", "Low", "Close", "Volume")]
}

# Remove missing rows and sort by date
clean_prices <- function(df) {
  df <- df[complete.cases(df[, c("Open", "High", "Low", "Close")]), ]
  df <- df[order(df$Date), ]
  df <- df[!duplicated(df$Date), ]
  if (nrow(df) < 2) stop("Not enough data")
  df$Volume[is.na(df$Volume)] <- 0
  rownames(df) <- NULL
  df
}

# Try the selected source, then the other one if fallback is on
load_prices <- function(symbol, from, to, source, fallback) {
  sources <- if (source == "yahoo") c("yahoo", "stooq") else c("stooq", "yahoo")
  if (!fallback) sources <- sources[1]
  for (src in sources) {
    res <- tryCatch({
      raw <- if (src == "yahoo") fetch_yahoo(symbol, from, to) else fetch_stooq(symbol, from, to)
      list(data = clean_prices(raw), source = src)
    }, error = function(e) NULL)
    if (!is.null(res)) return(res)
  }
  list(data = NULL, source = NA,
       msg = paste0("Could not download data for '", symbol,
                    "'. Check the symbol and your internet connection."))
}

# Convert daily data to weekly or monthly bars
resample_prices <- function(df, frame) {
  if (frame == "Daily") return(df)
  x <- xts(df[, c("Open", "High", "Low", "Close", "Volume")], order.by = df$Date)
  r <- if (frame == "Weekly") to.weekly(x, indexAt = "startof") else to.monthly(x, indexAt = "startof")
  out <- data.frame(Date = as.Date(index(r)), coredata(r))
  names(out) <- c("Date", "Open", "High", "Low", "Close", "Volume")
  out
}

# ---------------------------------------------------------------------------
# Step 3: Technical indicators (moving averages, RSI, MACD)
# ---------------------------------------------------------------------------

# Returns NA instead of an error when there are too few observations
guarded <- function(n_needed, len, f) {
  if (len >= n_needed) as.numeric(f()) else rep(NA_real_, len)
}

add_indicators <- function(d, prm) {
  n <- nrow(d)
  d$SMA_short <- guarded(prm$short, n, function() SMA(d$Close, n = prm$short))
  d$SMA_long  <- guarded(prm$long,  n, function() SMA(d$Close, n = prm$long))
  d$RSI       <- guarded(prm$rsi_n + 1, n, function() RSI(d$Close, n = prm$rsi_n))

  if (n >= prm$slow + prm$sig) {
    m <- MACD(d$Close, nFast = prm$fast, nSlow = prm$slow, nSig = prm$sig, maType = "EMA")
    d$MACD        <- as.numeric(m[, "macd"])
    d$MACD_signal <- as.numeric(m[, "signal"])
  } else {
    d$MACD        <- NA_real_
    d$MACD_signal <- NA_real_
  }
  d$MACD_hist <- d$MACD - d$MACD_signal
  d
}

# ---------------------------------------------------------------------------
# Step 4: Trading rule (moving average crossover)
# Buy  : short MA crosses above long MA
# Sell : short MA crosses below long MA
# Hold : otherwise
# Optional RSI filter: ignore a Buy if RSI is overbought, a Sell if oversold
# ---------------------------------------------------------------------------

generate_signals <- function(d, prm) {
  n <- nrow(d)
  signals <- rep("Hold", n)
  if (n < 2) return(signals)

  s <- d$SMA_short
  l <- d$SMA_long
  s_prev <- c(NA, s[-n])
  l_prev <- c(NA, l[-n])

  buy  <- s_prev <= l_prev & s > l
  sell <- s_prev >= l_prev & s < l
  buy[is.na(buy)]   <- FALSE
  sell[is.na(sell)] <- FALSE

  if (isTRUE(prm$use_rsi)) {
    buy  <- buy  & !(!is.na(d$RSI) & d$RSI > prm$ob)
    sell <- sell & !(!is.na(d$RSI) & d$RSI < prm$os)
  }

  signals[buy]  <- "Buy"
  signals[sell] <- "Sell"
  signals
}

# ---------------------------------------------------------------------------
# Step 2 and 3: Charts
# ---------------------------------------------------------------------------

# Price chart: line, candlestick or area, with moving averages and signals
build_price_chart <- function(d, chart_type, frame, prm, show_ma, show_signals, show_hold, symbol) {
  p <- ggplot(d, aes(x = Date))

  if (chart_type == "Line") {
    p <- p + geom_line(aes(y = Close), color = "black")

  } else if (chart_type == "Area") {
    p <- p +
      geom_ribbon(aes(ymin = min(Low) * 0.98, ymax = Close), fill = "grey80") +
      geom_line(aes(y = Close), color = "black")

  } else {
    w <- switch(frame, Daily = 0.35, Weekly = 2, Monthly = 10)
    d$Direction <- ifelse(d$Close >= d$Open, "Up", "Down")
    p <- p +
      geom_segment(data = d, aes(xend = Date, y = Low, yend = High), color = "black") +
      geom_rect(data = d, aes(xmin = Date - w, xmax = Date + w,
                              ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                              fill = Direction), color = "black", linewidth = 0.2) +
      scale_fill_manual(values = c(Up = "white", Down = "grey30"), name = NULL)
  }

  # Moving averages overlay
  if (show_ma) {
    lab_s <- paste0("SMA ", prm$short)
    lab_l <- paste0("SMA ", prm$long)
    p <- p +
      geom_line(aes(y = SMA_short, linetype = lab_s), color = "#1f77b4", linewidth = 0.9, na.rm = TRUE) +
      geom_line(aes(y = SMA_long,  linetype = lab_l), color = "#ff7f0e", linewidth = 0.9, na.rm = TRUE) +
      scale_linetype_manual(values = setNames(c("solid", "dashed"), c(lab_s, lab_l)), name = NULL)
  }

  # Buy / Sell / Hold annotations
  if (show_signals) {
    span  <- max(d$High) - min(d$Low)
    buys  <- d[d$Signal == "Buy", ]
    sells <- d[d$Signal == "Sell", ]
    holds <- d[d$Signal == "Hold", ]

    if (show_hold && nrow(holds) > 0)
      p <- p + geom_text(data = holds, aes(y = High, label = "Hold"),
                         vjust = -0.6, size = 2.2, check_overlap = TRUE)
    if (nrow(buys) > 0)
      p <- p +
        geom_point(data = buys, aes(y = Low - 0.03 * span), shape = 24,
                   fill = "#2e9e5b", color = "black", size = 4) +
        geom_text(data = buys, aes(y = Low - 0.03 * span, label = "Buy"),
                  vjust = 2.1, fontface = "bold", size = 3.5)
    if (nrow(sells) > 0)
      p <- p +
        geom_point(data = sells, aes(y = High + 0.03 * span), shape = 25,
                   fill = "#d64545", color = "black", size = 4) +
        geom_text(data = sells, aes(y = High + 0.03 * span, label = "Sell"),
                  vjust = -1.2, fontface = "bold", size = 3.5)
  }

  p +
    scale_y_continuous(expand = expansion(mult = 0.1)) +
    labs(title = paste0(symbol, " - ", chart_type, " chart (", frame, ")"),
         x = NULL, y = "Price (USD)") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))
}

build_rsi_chart <- function(d, prm) {
  ggplot(d, aes(x = Date, y = RSI)) +
    geom_hline(yintercept = c(prm$os, prm$ob), linetype = "dashed") +
    geom_line(na.rm = TRUE) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(title = paste0("RSI (", prm$rsi_n, ")"), x = NULL, y = "RSI") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
}

build_macd_chart <- function(d, prm) {
  ggplot(d, aes(x = Date)) +
    geom_col(aes(y = MACD_hist), fill = "grey70", na.rm = TRUE) +
    geom_line(aes(y = MACD, linetype = "MACD"), na.rm = TRUE) +
    geom_line(aes(y = MACD_signal, linetype = "Signal"), na.rm = TRUE) +
    geom_hline(yintercept = 0) +
    scale_linetype_manual(values = c(MACD = "solid", Signal = "dashed"), name = NULL) +
    labs(title = paste0("MACD (", prm$fast, ", ", prm$slow, ", ", prm$sig, ")"),
         x = NULL, y = "MACD") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))
}

# ---------------------------------------------------------------------------
# Step 2: User interface
# ---------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput("symbol", "Stock Symbol:",
                     choices = c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA"),
                     selected = stock_symbol, options = list(create = TRUE)),
      dateRangeInput("date_range", "Select Date Range:",
                     start = start_date, end = end_date),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart Type:",
                  choices = c("Candlestick", "Line", "Area")),
      selectInput("source", "Data Source:",
                  choices = c("Yahoo Finance" = "yahoo", "Stooq" = "stooq")),
      checkboxInput("fallback", "Use the other source if the first fails", TRUE),
      hr(),
      checkboxGroupInput("technical_indicators", "Technical Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = "Moving Averages"),
      conditionalPanel(
        "input.technical_indicators.indexOf('Moving Averages') > -1",
        sliderInput("short_n", "Short MA window:", min = 5, max = 50, value = 20),
        sliderInput("long_n", "Long MA window:", min = 20, max = 200, value = 50)),
      conditionalPanel(
        "input.technical_indicators.indexOf('RSI') > -1",
        sliderInput("rsi_n", "RSI period:", min = 5, max = 30, value = 14)),
      conditionalPanel(
        "input.technical_indicators.indexOf('MACD') > -1",
        numericInput("macd_fast", "MACD fast EMA:", value = 12, min = 2, max = 50),
        numericInput("macd_slow", "MACD slow EMA:", value = 26, min = 5, max = 100),
        numericInput("macd_sig", "MACD signal EMA:", value = 9, min = 2, max = 50)),
      hr(),
      checkboxInput("show_signals", "Show Buy / Sell signals", TRUE),
      checkboxInput("show_hold", "Label Hold bars", FALSE),
      checkboxInput("use_rsi", "Confirm signals with RSI", FALSE),
      conditionalPanel(
        "input.use_rsi",
        sliderInput("rsi_levels", "RSI oversold / overbought:",
                    min = 10, max = 90, value = c(30, 70)))
    ),
    mainPanel(
      width = 9,
      textOutput("status"),
      plotOutput("stock_chart", height = "430px"),
      conditionalPanel("input.technical_indicators.indexOf('RSI') > -1",
                       plotOutput("rsi_chart", height = "190px")),
      conditionalPanel("input.technical_indicators.indexOf('MACD') > -1",
                       plotOutput("macd_chart", height = "220px")),
      h4("Signals"),
      tableOutput("signal_table")
    )
  )
)

# ---------------------------------------------------------------------------
# Step 2 to 4: Server
# ---------------------------------------------------------------------------

server <- function(input, output) {

  # Download data (re-runs only when symbol, dates or source change)
  price_data <- reactive({
    sym <- toupper(trimws(input$symbol))
    req(nzchar(sym), input$date_range)
    from <- as.Date(input$date_range[1]) - WARMUP_DAYS
    to   <- as.Date(input$date_range[2]) + 1
    withProgress(message = paste("Downloading", sym), value = 0.5,
                 load_prices(sym, from, to, input$source, input$fallback))
  })

  # Indicator and rule settings
  prm <- reactive({
    list(short = input$short_n, long = input$long_n, rsi_n = input$rsi_n,
         fast = input$macd_fast, slow = input$macd_slow, sig = input$macd_sig,
         use_rsi = input$use_rsi, os = input$rsi_levels[1], ob = input$rsi_levels[2])
  })

  # Resample, add indicators, generate signals, then filter by date range
  analysis <- reactive({
    raw <- price_data()
    validate(need(!is.null(raw$data), raw$msg))
    req(input$date_range, input$short_n, input$long_n, input$rsi_n,
        input$macd_fast, input$macd_slow, input$macd_sig)
    p <- prm()
    validate(
      need(input$date_range[1] < input$date_range[2], "Start date must be before end date."),
      need(p$short < p$long, "Short MA window must be smaller than long MA window."),
      need(p$fast < p$slow, "MACD fast EMA must be smaller than slow EMA."))

    full <- resample_prices(raw$data, input$time_frame)
    full <- add_indicators(full, p)
    full$Signal <- generate_signals(full, p)

    filtered_data <- full[full$Date >= input$date_range[1] &
                          full$Date <= input$date_range[2], ]
    validate(need(nrow(filtered_data) >= 2,
                  "Not enough data for this date range and time frame."))
    filtered_data
  })

  output$status <- renderText({
    raw <- price_data()
    if (is.null(raw$data)) return(raw$msg)
    paste("Data source:", if (raw$source == "yahoo") "Yahoo Finance" else "Stooq")
  })

  output$stock_chart <- renderPlot({
    p <- build_price_chart(analysis(), input$chart_type, input$time_frame, prm(),
                           "Moving Averages" %in% input$technical_indicators,
                           input$show_signals, input$show_hold,
                           toupper(trimws(input$symbol)))
    print(p)
  })

  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$technical_indicators)
    d <- analysis()
    validate(need(any(!is.na(d$RSI)), "Not enough data to calculate RSI."))
    print(build_rsi_chart(d, prm()))
  })

  output$macd_chart <- renderPlot({
    req("MACD" %in% input$technical_indicators)
    d <- analysis()
    validate(need(any(!is.na(d$MACD)), "Not enough data to calculate MACD. Use a longer date range."))
    print(build_macd_chart(d, prm()))
  })

  output$signal_table <- renderTable({
    d <- analysis()
    s <- d[d$Signal != "Hold", c("Date", "Signal", "Close")]
    if (nrow(s) == 0) return(data.frame(Message = "No Buy/Sell signals in this range."))
    s$Date <- format(s$Date, "%Y-%m-%d")
    s
  }, digits = 2)
}

shinyApp(ui, server)
