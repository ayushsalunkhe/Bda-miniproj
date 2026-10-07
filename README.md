# Inventory Demand Prediction — BDA Mini Project

An interactive R Shiny application for inventory demand forecasting and replenishment recommendations using a Random Forest model.

## Features
- Forecast daily demand for a selected store and item
- Recursive multi-day forecasting
- Safety stock and reorder point calculation
- Suggested order quantity
- Interactive forecast visualization
- Model RMSE and MAPE information

## Run locally
Install R/RStudio, then:

```r
install.packages(c("shiny", "tidyverse", "lubridate", "zoo", "randomForest", "scales"))
shiny::runApp()
```

The app uses `train.csv` when available and falls back to synthetic data if it is absent. If `rf_cache.rds` is present, the pre-trained Random Forest is loaded instead of retrained.

## Deployment
Designed for Shiny-compatible hosting such as shinyapps.io.
