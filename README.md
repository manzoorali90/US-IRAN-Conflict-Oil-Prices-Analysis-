## Authors

- Umer Riaz
- Manzoor Ali
- Khayam Khan
- Muhammad Saeed

Supervisor: Dr. Muhammad Aamir, Department of Statistics, Abdul Wali Khan University Mardan


# US-IRAN-Conflict-Oil-Prices-Analysis-
Time series analysis of the effect of the 2026 U.S.–Iran confl ict on WTI and Brent crude oil prices, using R (ARIMA modeling, hypothesis testing, correlation analysis). BS Statistics capstone thesis, Abdul Wali Khan University Mardan.

R code 
Uploaded CSV Files
setwd("C:/Users/manzo/Downloads/Brent and wti daa before and after")
wti_before   <- read.csv("Crude Oil WTI Historical Data Before war.csv")
wti_after    <- read.csv("Crude Oil WTI Historical Data After war.csv")
brent_before <- read.csv("Brent OilHistorical Data Before war.csv")
brent_after  <- read.csv("Brent Oil Historical Data after war.csv")
View(wti_before)
str(wti_after)

Descriptive statistics
#fINDING MEAN FOR ALL 
mean(wti_before$Price, na.rm = TRUE)
mean(brent_before$Price, na.rm = TRUE)
mean(wti_after$Price, na.rm = TRUE)
mean(brent_after$Price, na.rm = TRUE)

#Variances_Brent
var(brent_before$Price)
var(brent_after$Price)
#Varinces_WTI
var(wti_before$Price)
var(wti_after$Price)

#STANDARD DEVIEATION WTI
sd(wti_before$Price)
sd(wti_after$Price)
#STANDARD DEVIEATION BRENT
sd(brent_before$Price)
sd(brent_after$Price)

Finding Correlation Between Brent and WTI oil
#Correlations (Brent vs wti)
#before conflict
nrow(wti_after)
nrow(wti_before)
nrow(brent_after)
nrow(brent_before)

# ── Before_Merged 
before_merged <- merge(wti_before, brent_before, by = "Date", suffixes = c("_WTI", "_Brent"))
cat("Before merged rows:", nrow(before_merged), "\n")
# ── AFTER_Merged
after_merged <- merge(wti_after, brent_after, by = "Date", suffixes = c("_WTI", "_Brent"))
cat("After merged rows:", nrow(after_merged), "\n")

#Correlations (Brent vs wti)
#before conflict
cor_before <- cor(before_merged$Price_WTI, before_merged$Price_Brent, use = "complete.obs")
cat("Correlation Before Conflict:", cor_before, "\n")

#After conflict
cor_after <- cor(after_merged$Price_WTI, after_merged$Price_Brent, use = "complete.obs")
cat("Correlation After Conflict:", cor_after, "\n")

# Run Correltion Before conflict
before_merged <- merge(wti_before, brent_before, by = "Date", suffixes = c("_WTI", "_Brent"))
cor_before <- cor(before_merged$Price_WTI, before_merged$Price_Brent, use = "complete.obs")
cat("Correlation Before:", cor_before, "\n")

# Run correlation After conflict
after_merged <- merge(wti_after, brent_after, by = "Date", suffixes = c("_WTI", "_Brent"))
cor_after <- cor(after_merged$Price_WTI, after_merged$Price_Brent, use = "complete.obs")
cat("Correlation After:", cor_after, "\n")

# WTI: Before vs After
t.test(wti_before$Price, wti_after$Price)
wilcox.test(wti_before$Price, wti_after$Price)

#Brent: Before vs After
t.test(brent_before$Price, brent_after$Price)
wilcox.test(brent_before$Price, brent_after$Price)

#Intalling ggplot2
install.packages("ggplot2")
library(ggplot2)

Showing a plot Before and after Conflict 
# BEFORE CONFLICT 
ggplot(before_merged, aes(x = Price_WTI, y = Price_Brent)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  annotate("text", 
           x = min(before_merged$Price_WTI), 
           y = max(before_merged$Price_Brent),
           label = paste("r =", round(cor_before, 4)),
           hjust = 0, size = 5, color = "darkred", fontface = "bold") +
  labs(title    = "WTI vs Brent Correlation — Before Conflict",
       x        = "WTI Price (USD)",
       y        = "Brent Price (USD)") +
  theme_minimal()

# AFTER CONFLICT 
ggplot(after_merged, aes(x = Price_WTI, y = Price_Brent)) +
  geom_point(color = "darkorange", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  annotate("text", 
           x = min(after_merged$Price_WTI), 
           y = max(after_merged$Price_Brent),
           label = paste("r =", round(cor_after, 4)),
           hjust = 0, size = 5, color = "darkred", fontface = "bold") +
  labs(title = "WTI vs Brent Correlation — After Conflict",
       x     = "WTI Price (USD)",
       y     = "Brent Price (USD)") +
  theme_minimal()

# both combined 

before_merged$Period <- "Before"
after_merged$Period  <- "After"
all_merged <- rbind(before_merged, after_merged)

ggplot(all_merged, aes(x = Price_WTI, y = Price_Brent, color = Period)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("Before" = "steelblue", "After" = "darkorange")) +
  labs(title = "WTI vs Brent Correlation — Before & After Conflict",
       x     = "WTI Price (USD)",
       y     = "Brent Price (USD)") +
  theme_minimal()


Now to frocast the data,  we dawnload some packages which is required
required_packages <- c("forecast", "tseries", "ggplot2")
to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)
install.packages("tseries")
install.packages("forecast")
library(forecast)
library(tseries)
library(ggplot2)

##  Reusable ARIMA workflow function
## Runs stationarity test -> auto.arima -> diagnostics -> forecast
## for a single price series, and returns everything in a list.

run_arima_analysis <- function(df,
                               series_name,
                               price_col = "Price",
                               date_col  = "Date",
                               forecast_horizon = 14) {
  
  cat("\n\n=====================================================\n")
  cat(" ARIMA ANALYSIS:", series_name, "\n")
  cat("=====================================================\n")
  
  ## --- Clean & order data ---
  df <- df[!is.na(df[[price_col]]), ]
  if (date_col %in% names(df)) {
    df[[date_col]] <- as.Date(df[[date_col]])
    df <- df[order(df[[date_col]]), ]
  }
  
  price_series <- df[[price_col]]
  
  ## Convert to ts object ---
  ## frequency = 1 since crude oil prices don't have a fixed
  ## seasonal cycle in the same way monthly/quarterly data does.
  ts_data <- ts(price_series, frequency = 1)
  
  ## --- Plot the raw series ---
  plot(ts_data, main = paste(series_name, "- Price Series"),
       ylab = "Price (USD)", xlab = "Time Index", col = "steelblue", lwd = 1.5)
  
  ## --- Stationarity check (Augmented Dickey-Fuller test) ---
  adf_result <- tryCatch(
    adf.test(ts_data),
    error = function(e) NULL
  )
  if (!is.null(adf_result)) {
    cat("\n--- Augmented Dickey-Fuller Test (stationarity) ---\n")
    cat("ADF Statistic:", round(adf_result$statistic, 4), "\n")
    cat("p-value:", round(adf_result$p.value, 4), "\n")
    if (adf_result$p.value < 0.05) {
      cat("=> Series is likely STATIONARY (reject unit root null).\n")
    } else {
      cat("=> Series is likely NON-STATIONARY (auto.arima will difference it).\n")
    }
  }
  
  ## --- Fit ARIMA model automatically ---
  fit <- auto.arima(ts_data,
                    stepwise = FALSE,
                    approximation = FALSE,
                    trace = TRUE)
  
  cat("\n--- Selected Model ---\n")
  print(summary(fit))
  
  ## --- Residual diagnostics (Ljung-Box test + ACF + histogram) ---
  cat("\n--- Residual Diagnostics ---\n")
  checkresiduals(fit)
  
  ## --- Forecast ---
  fc <- forecast(fit, h = forecast_horizon)
  plot(fc, main = paste(series_name, "- ARIMA Forecast (", forecast_horizon, "periods)"))
  
  ## --- Accuracy on training data ---
  acc <- accuracy(fit)
  cat("\n--- In-sample Accuracy ---\n")
  print(acc)
  
  return(list(
    model      = fit,
    order      = arimaorder(fit),
    aic        = fit$aic,
    bic        = fit$bic,
    forecast   = fc,
    adf_pvalue = if (!is.null(adf_result)) adf_result$p.value else NA,
    accuracy   = acc
  ))
}

## ---- 3. Run ARIMA for all four series ----
results_wti_before   <- run_arima_analysis(wti_before,   "WTI - Before Conflict")
results_wti_after    <- run_arima_analysis(wti_after,    "WTI - After Conflict")
results_brent_before <- run_arima_analysis(brent_before, "Brent - Before Conflict")
results_brent_after  <- run_arima_analysis(brent_after,  "Brent - After Conflict")

## ---- 4. Summary comparison table ----
summary_table <- data.frame(
  Series       = c("WTI Before", "WTI After", "Brent Before", "Brent After"),
  ARIMA_Order  = c(
    paste0("(", paste(results_wti_before$order[1:3], collapse = ","), ")"),
    paste0("(", paste(results_wti_after$order[1:3], collapse = ","), ")"),
    paste0("(", paste(results_brent_before$order[1:3], collapse = ","), ")"),
    paste0("(", paste(results_brent_after$order[1:3], collapse = ","), ")")
  ),
  AIC          = c(results_wti_before$aic, results_wti_after$aic,
                   results_brent_before$aic, results_brent_after$aic),
  ADF_pvalue   = c(results_wti_before$adf_pvalue, results_wti_after$adf_pvalue,
                   results_brent_before$adf_pvalue, results_brent_after$adf_pvalue),
  RMSE         = c(results_wti_before$accuracy[,"RMSE"], results_wti_after$accuracy[,"RMSE"],
                   results_brent_before$accuracy[,"RMSE"], results_brent_after$accuracy[,"RMSE"]),
  MAPE         = c(results_wti_before$accuracy[,"MAPE"], results_wti_after$accuracy[,"MAPE"],
                   results_brent_before$accuracy[,"MAPE"], results_brent_after$accuracy[,"MAPE"])
)

cat("\n\n=====================================================\n")
cat(" SUMMARY: ARIMA Models Across All Series\n")
cat("=====================================================\n")
print(summary_table)

## ---- 5. (Optional) Forecast values only, as data frames ----
fc_wti_before_df   <- as.data.frame(results_wti_before$forecast)
fc_wti_after_df    <- as.data.frame(results_wti_after$forecast)
fc_brent_before_df <- as.data.frame(results_brent_before$forecast)
fc_brent_after_df  <- as.data.frame(results_brent_after$forecast)

## Example: view the WTI-after forecast
# print(fc_wti_after_df)
