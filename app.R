# Inventory Demand Prediction - Shiny App (BDA Mini Project)
# Deployment-friendly version: uses only shiny + randomForest plus base R.

suppressPackageStartupMessages({
  library(shiny)
  library(randomForest)
})

set.seed(42)

# ---- Helpers (base R; no tidyverse/lubridate/zoo dependency) ----
weekday_num <- function(x) as.integer(format(x, "%u"))
month_num <- function(x) as.integer(format(x, "%m"))
year_num <- function(x) as.integer(format(x, "%Y"))
doy_num <- function(x) as.integer(format(x, "%j"))

make_lag <- function(x, k) {
  c(rep(NA_real_, k), x[seq_len(length(x) - k)])
}

make_roll <- function(x, lag_k = 7, width = 7) {
  out <- rep(NA_real_, length(x))
  if (length(x) > lag_k + width - 1) {
    for (i in (lag_k + width):length(x)) {
      out[i] <- mean(x[(i - lag_k - width + 1):(i - lag_k)])
    }
  }
  out
}

# ---- Load data ----
if (file.exists("train.csv")) {
  raw <- read.csv("train.csv", stringsAsFactors = FALSE)
  raw$date <- as.Date(raw$date)
} else {
  dates <- seq(as.Date("2013-01-01"), as.Date("2017-12-31"), by = "day")
  grid <- expand.grid(date = dates, store = 1:5, item = 1:20)
  base <- runif(100, 15, 60)
  grid$base <- base[(grid$store - 1) * 20 + grid$item]
  dow_eff <- c(.85, .85, .9, .95, 1.05, 1.25, 1.2)
  wd <- weekday_num(grid$date)
  yr <- year_num(grid$date)
  yd <- doy_num(grid$date)
  grid$sales <- rpois(
    nrow(grid),
    grid$base *
      (1 + .3 * sin(2 * pi * yd / 365)) *
      dow_eff[wd] *
      (1 + .08 * (yr - 2013))
  )
  raw <- grid[, c("date", "store", "item", "sales")]
}

df <- raw[!is.na(raw$sales), c("date", "store", "item", "sales")]
df$sales <- pmax(as.numeric(df$sales), 0)
df$store <- as.integer(df$store)
df$item <- as.integer(df$item)
df <- df[order(df$store, df$item, df$date), ]

last_date <- max(df$date)
TEST_DAYS <- 90
split_date <- last_date - TEST_DAYS
stores <- sort(unique(df$store))
items <- sort(unique(df$item))

features <- c(
  "store", "item", "lag_7", "lag_14", "lag_28",
  "roll_7", "roll_28", "dow", "month", "year", "doy", "weekend"
)

# ---- Load cached model or train it ----
if (file.exists("rf_cache.rds")) {
  cache <- readRDS("rf_cache.rds")
} else {
  message("rf_cache.rds not found; training Random Forest...")
  feat <- df
  groups <- split(seq_len(nrow(df)), interaction(df$store, df$item, drop = TRUE))

  feat$lag_7 <- NA_real_
  feat$lag_14 <- NA_real_
  feat$lag_28 <- NA_real_
  feat$roll_7 <- NA_real_
  feat$roll_28 <- NA_real_

  for (idx in groups) {
    x <- df$sales[idx]
    feat$lag_7[idx] <- make_lag(x, 7)
    feat$lag_14[idx] <- make_lag(x, 14)
    feat$lag_28[idx] <- make_lag(x, 28)
    feat$roll_7[idx] <- make_roll(x, 7, 7)
    feat$roll_28[idx] <- make_roll(x, 7, 28)
  }

  feat$dow <- weekday_num(feat$date)
  feat$month <- month_num(feat$date)
  feat$year <- year_num(feat$date)
  feat$doy <- doy_num(feat$date)
  feat$weekend <- as.integer(feat$dow >= 6)
  feat <- feat[complete.cases(feat[, features]), ]

  train <- feat[feat$date <= split_date, ]
  test <- feat[feat$date > split_date, ]
  n_train <- min(60000, nrow(train))
  train_s <- train[sample.int(nrow(train), n_train), ]

  rf <- randomForest(
    x = train_s[, features],
    y = train_s$sales,
    ntree = 100
  )

  test$pred <- predict(rf, test[, features])
  keys <- paste(test$store, test$item, sep = "_")
  err_sd <- tapply(test$sales - test$pred, keys, sd)
  err_tbl <- data.frame(
    store = as.integer(sub("_.*", "", names(err_sd))),
    item = as.integer(sub(".*_", "", names(err_sd))),
    err_sd = as.numeric(err_sd)
  )

  nz <- test$sales > 0
  cache <- list(
    rf = rf,
    err_tbl = err_tbl,
    rmse = sqrt(mean((test$sales - test$pred)^2)),
    mape = mean(abs(test$sales[nz] - test$pred[nz]) / test$sales[nz]) * 100
  )
  saveRDS(cache, "rf_cache.rds")
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Inventory Demand Prediction"),
  sidebarLayout(
    sidebarPanel(
      h4("Product Details"),
      selectInput("store", "Store", choices = stores),
      selectInput("item", "Item", choices = items),
      sliderInput("horizon", "Forecast period (days)", 7, 30, 14, 1),
      h4("Inventory Settings"),
      numericInput("lead_time", "Supplier lead time (days)", 7, 1, 30),
      selectInput("service", "Service level",
                  choices = c("90%" = 1.28, "95%" = 1.65, "99%" = 2.33),
                  selected = 1.65),
      numericInput("stock", "Current stock (units)", 100, min = 0),
      actionButton("go", "Predict Demand", class = "btn-primary")
    ),
    mainPanel(
      h2("Prediction"),
      uiOutput("recommendation"),
      br(),
      h4("Key Numbers"),
      tableOutput("summary_tbl"),
      h4("Demand Forecast"),
      plotOutput("forecast_plot", height = "320px"),
      h4("Daily Forecast Table"),
      tableOutput("forecast_tbl"),
      br(),
      p(textOutput("model_info"))
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  result <- eventReactive(input$go, {
    st <- as.integer(input$store)
    it <- as.integer(input$item)
    H <- input$horizon
    L <- input$lead_time
    z <- as.numeric(input$service)

    hist <- df[df$store == st & df$item == it, ]
    hist <- hist[order(hist$date), ]
    validate(need(nrow(hist) > 60, "Not enough history for this store-item."))

    n <- nrow(hist)
    v <- c(hist$sales, rep(NA_real_, H))
    fdates <- max(hist$date) + seq_len(H)

    for (h in seq_len(H)) {
      t <- n + h
      d <- fdates[h]
      wd <- weekday_num(d)
      nd <- data.frame(
        store = st,
        item = it,
        lag_7 = v[t - 7],
        lag_14 = v[t - 14],
        lag_28 = v[t - 28],
        roll_7 = mean(v[(t - 13):(t - 7)]),
        roll_28 = mean(v[(t - 34):(t - 7)]),
        dow = wd,
        month = month_num(d),
        year = year_num(d),
        doy = doy_num(d),
        weekend = as.integer(wd >= 6)
      )
      v[t] <- max(0, as.numeric(predict(cache$rf, nd[, features])))
    }

    fc <- data.frame(date = fdates, forecast = v[(n + 1):(n + H)])

    err_sd <- cache$err_tbl[
      cache$err_tbl$store == st & cache$err_tbl$item == it, "err_sd"
    ]
    if (length(err_sd) == 0 || is.na(err_sd)) err_sd <- sd(hist$sales) * .3

    avg_daily <- mean(fc$forecast)
    safety <- ceiling(z * err_sd * sqrt(L))
    rop <- ceiling(avg_daily * L + safety)
    total_h <- ceiling(sum(fc$forecast))
    order_qty <- max(0, total_h + safety - input$stock)

    list(
      fc = fc, hist = hist, avg_daily = avg_daily,
      safety = safety, rop = rop, total_h = total_h,
      order_qty = order_qty, stock = input$stock,
      H = H, st = st, it = it
    )
  })

  output$recommendation <- renderUI({
    r <- result()
    if (r$stock <= r$rop) {
      div(
        style = "padding:20px;border:2px solid #dc3545;border-radius:10px;",
        h2("Reorder Now"),
        p(paste0(
          "Current stock (", r$stock,
          ") is at or below the reorder point (", r$rop,
          "). Suggested order quantity: ", r$order_qty,
          " units to cover the next ", r$H, " days."
        ))
      )
    } else {
      div(
        style = "padding:20px;border:2px solid #198754;border-radius:10px;",
        h2("Stock is Sufficient"),
        p(paste0(
          "Current stock (", r$stock,
          ") is above the reorder point (", r$rop,
          "). No order needed yet."
        ))
      )
    }
  })

  output$summary_tbl <- renderTable({
    r <- result()
    data.frame(
      Metric = c(
        "Average daily demand (forecast)",
        paste0("Total demand, next ", r$H, " days"),
        "Safety stock", "Reorder point", "Current stock"
      ),
      Value = c(
        round(r$avg_daily, 1), r$total_h,
        r$safety, r$rop, r$stock
      )
    )
  })

  output$forecast_plot <- renderPlot({
    r <- result()
    h <- tail(r$hist, 60)
    plot(
      h$date, h$sales, type = "l",
      xlim = range(c(h$date, r$fc$date)),
      ylim = range(c(h$sales, r$fc$forecast)),
      xlab = "Date", ylab = "Units per day",
      main = paste0("Store ", r$st, " - Item ", r$it)
    )
    lines(r$fc$date, r$fc$forecast, lty = 2)
    legend(
      "topleft", legend = c("History", "Forecast"),
      lty = c(1, 2), bty = "n"
    )
  })

  output$forecast_tbl <- renderTable({
    r <- result()
    data.frame(
      Date = format(r$fc$date, "%d %b %Y"),
      Day = format(r$fc$date, "%A"),
      Forecast_units = round(r$fc$forecast, 1)
    )
  })

  output$model_info <- renderText({
    paste0(
      "Model: Random Forest trained on store-item sales. Accuracy on the last ",
      TEST_DAYS, " days: MAPE = ", round(cache$mape, 1),
      "%, RMSE = ", round(cache$rmse, 1), " units."
    )
  })
}

shinyApp(ui = ui, server = server)
