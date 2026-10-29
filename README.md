# Assignment 6: Technical Analysis using R, Visualization Phase

**Course:** BDA400 - Data Science Tools and Techniques
**Student:** John Diaz
**Repository:** https://github.com/johndi24221614-web/TechnicalAnalysis-Assignment6

## Project description

This is the third stage of the three-part Technical Analysis project. It is an interactive
portfolio dashboard built with **R Shiny** that downloads historical stock data from
**Yahoo Finance** (with a Stooq fallback) and visualizes it with **ggplot2**.

The dashboard lets the user:

- Pick a stock symbol (portfolio: AAPL, MSFT, TSLA, GM, F, TM, or any other ticker), a date range and a
  time frame (Daily / Weekly / Monthly).
- Display the data as a **line**, **candlestick** or **area** chart.
- Turn technical indicators on and off: **Moving Averages** (overlaid on the price chart), **RSI** and **MACD**
  (shown in aligned panels because they use a different scale).
- Apply a **Moving Average crossover trading rule** with customizable short/long periods and an optional
  **RSI filter**, and see **Buy / Sell / Hold** signals annotated on the chart and in a downloadable table.

## Files

| File | Description |
|------|-------------|
| `app.R` | R script with the full dashboard, organized in sections that match the assignment steps |
| `README.md` | Cover page of the project |

## Code organization (`app.R`)

1. **Section 1 - Data Collection and Setup:** package installation/loading, Yahoo Finance download with error handling and fallback source.
2. **Section 2 - Visualizing Stock Data:** Shiny skeleton, interactive widgets (date range, time frame, chart type) and the line/candlestick/area plot.
3. **Section 3 - Overlay Technical Indicators:** SMA, RSI and MACD layers with on/off toggling from a checkbox group.
4. **Section 4 - Trading Rules and Annotations:** crossover rule, RSI filter, and Buy/Sell/Hold annotations on the chart.

## How to run

1. Open `app.R` in RStudio.
2. Click **Run App** (missing packages `shiny`, `ggplot2`, `quantmod` and `TTR` are installed automatically).
3. Use the sidebar to choose the stock, dates, chart type, indicators and trading-rule parameters.

An internet connection is required to download the stock data.
